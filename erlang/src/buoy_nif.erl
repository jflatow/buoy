-module(buoy_nif).

-export([init/2,
         call/3]).

-define(nif_not_loaded, erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE})).

-on_load(load_nif/0).
load_nif() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

init(_Func, _Args) ->
    ?nif_not_loaded.

call(_Buoy, _Method, _Args) ->
    ?nif_not_loaded.
