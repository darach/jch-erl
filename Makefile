#
ifeq ($(wildcard rebar3),rebar3)
REBAR = $(CURDIR)/rebar3
endif

# Fallback to rebar on PATH
REBAR ?= $(shell which rebar3)

.PHONY: all erl test clean doc perf

all: perf

build:
	$(REBAR) compile

dialyze:
	$(REBAR) dialyzer

test:
	$(REBAR) eunit

clean:
	$(REBAR) clean
	$(REBAR) as perf clean
	-rm -rvf doc
	-rm -f perf/*.o
	-rm -f perf/ch

doc:
	$(REBAR) edoc

perf:
	$(REBAR) as perf compile
	gcc -c -O3 -ffast-math -std=c99 -I c_src perf/ch.c -o perf/ch.o
	gcc -o perf/ch c_src/jch.o perf/ch.o -lm
	perf/ch 10000000 10
	perf/ch 10000000 100
	perf/ch 10000000 1000
	perf/ch 10000000 10000
	perf/ch 10000000 100000
	perf/ch 10000000 1000000
	perf/ch 10000000 10000000
	perf/ch 10000000 100000000
	ERL_LIBS=_build/perf/lib erl +sfwi 1 +scl false -pa _build/perf/lib/jch/perf -noinput -eval "bench:main([])"
