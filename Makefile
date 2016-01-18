REBAR:=rebar

.PHONY: all erl test clean doc 

all: perf

build:
	$(REBAR) get-deps compile

buildplt:
	if [ ! -f .plt ]; then \
        dialyzer --build_plt --output_plt .plt --apps kernel stdlib ; \
    fi

pltclean:
	@rm .plt

dialyze:
	@ERL_LIBS=deps dialyzer --fullpath -Wno_undefined_callbacks \
        --plts .plt \
        -r ebin --src src \
        | grep -v -f ./dialyzer.ignore-warnings

test: build dialyze
	@mkdir -p .eunit
	$(REBAR) skip_deps=true eunit ct

clean:
	$(REBAR) clean
	-rm -rvf deps ebin doc .eunit
	-rm -f perf/*.o
	-rm -f perf/ch

doc:
	$(REBAR) doc

perf: test
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
	erlc -pa ebin -o perf perf/bench.erl
	ERL_LIBS=deps erl +sfwi 1 +scl false -pa ebin -pa perf -noinput -eval "bench:main([])"
