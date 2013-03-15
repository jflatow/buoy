
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

erlang:
	(cd erlang && $(REBAR) compile)

python:
	(cd python && python setup.py build)

clean:
	rm -rf `find . -name \*.o`
	rm -rf alex
	rm -rf python/build
	rm -rf erlang/ebin