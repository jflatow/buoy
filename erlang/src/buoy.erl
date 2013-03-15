-module(buoy).

-export([buoy/0,
         buoy/1,
         copy/1,
         save/2,
         fetch/2,
         store/3,
         learn/3,
         score/2,
         train/2]).

-export([moor/1]).

-define(MaxDelta, 1.0e300).

shuffled(List) -> %% simple algorithm, but we might want to do better
    [X || {_, X} <- lists:sort([{random:uniform(), X} || X <- List])].

str(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
str(Str) when is_list(Str) ->
    Str.

init(Func, Args) ->
    case buoy_nif:init(Func, Args) of
        Buoy when is_binary(Buoy) ->
            receive
                ok ->
                    Buoy;
                Reply ->
                    Reply
            end;
        Error ->
            Error
    end.

call(Buoy, Method, Args) ->
    case buoy_nif:call(Buoy, Method, Args) of
        Buoy ->
            receive
                ok ->
                    Buoy;
                Reply ->
                    Reply
            end;
        Error ->
            Error
    end.

buoy() ->
    init(buoy_new, {}).

buoy(Filename) ->
    init(buoy_open, str(Filename)).

copy(Buoy) ->
    init(buoy_copy, Buoy).

save(Buoy, Filename) ->
    call(Buoy, buoy_save, str(Filename)).

fetch(Buoy, Attr) ->
    call(Buoy, buoy_fetch, Attr).

store(Buoy, Attr, Weight) ->
    call(Buoy, buoy_store, {Attr, float(Weight)}).

learn(Buoy, {Doc, {Start, End}}, Score) ->
    call(Buoy, lexi_learn, {Doc, Start, End, float(Score)});
learn(Buoy, Doc, Score) ->
    call(Buoy, doxi_learn, {Doc, float(Score)}).

score(Buoy, {Doc, {Start, End}}) ->
    call(Buoy, lexi_score, {Doc, Start, End});
score(Buoy, Doc) ->
    call(Buoy, doxi_score, Doc).

train(Buoy, Lessons) when is_list(Lessons) ->
    train(Buoy, {Lessons, 1.0e-8, 10000});
train(Buoy, {Lessons, Tolerance, MaxIter}) ->
    train(Buoy, {Lessons, Tolerance, MaxIter}, 0).

train(Buoy, {Lessons, Tolerance, MaxIter}, Iter) when Iter < MaxIter ->
    case lists:foldl(fun ({Example, Score}, Deltas) ->
                             Deltas + abs(learn(Buoy, Example, Score))
                     end, 0, shuffled(Lessons)) of
        Deltas when Deltas > ?MaxDelta ->
            {error, Lessons};
        Deltas when Deltas > Tolerance ->
            train(Buoy, {Lessons, Tolerance, MaxIter}, Iter + 1);
        _ ->
            {ok, Buoy, Iter + 1}
    end;
train(_, {Lessons, _, _}, _) ->
    {error, Lessons}.

moor(Filename) ->
    Owner = self(),
    _Init = spawn_link(fun () -> Owner ! buoy(Filename) end),
    receive
        Buoy when is_binary(Buoy) ->
            spawn_link(fun () -> moor(Buoy, Filename, none, []) end);
        {error, enoent} ->
            spawn_link(fun () -> moor(buoy(), Filename, none, []) end);
        Error ->
            Error
    end.

moor(Buoy, Filename, none, [{From, Args}|Pending]) ->
    Moor = self(),
    moor(Buoy, Filename,
         spawn_link(fun () ->
                            case train(copy(Buoy), Args) of
                                {ok, Copy, Iter} ->
                                    Moor ! {ready, From, self(), save(Copy, Filename), Iter};
                                {error, Lessons} ->
                                    Moor ! {error, From, self(), Buoy, Lessons}
                            end
                    end),
         Pending);
moor(Buoy, Filename, Trainer, Pending) ->
    receive
        {buoy, From} ->
            From ! {buoy, Buoy},
            moor(Buoy, Filename, Trainer, Pending);
        {error, From, Trainer, Buoy, Lessons} ->
            From ! {buoy_error, Buoy, Lessons},
            moor(Buoy, Filename, none, Pending);
        {ready, From, Trainer, NewBuoy, Iter} ->
            From ! {buoy_ready, NewBuoy, Iter},
            moor(NewBuoy, Filename, none, Pending);
        {train, From, Args} ->
            moor(Buoy, Filename, Trainer, [{From, Args}|Pending]);
        stop ->
            ok
    end.
