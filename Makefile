
ifeq ($(shell uname), Darwin)
	export CC = clang
	export CFLAGS = -O3
else
	export CC = gcc
	export CFLAGS = -O3 -std=c99
endif

REBAR = rebar

.PHONY: erlang python

alex:	alex.c buoy.o lexi.o
	$(CC) $(CFLAGS) -o $@ $^

erlang: CMD = compile
erlang:
	(cd erlang && $(REBAR) $(CMD))

python: CMD = build
python:
	(cd python && python setup.py $(CMD))

clean:
	rm -rf `find . -name \*.o`
	rm -rf alex
	rm -rf python/build
	rm -rf erlang/ebin erlang/priv