%% -------------------------------------------------------------------
%% Copyright (c) 2014 Darach Ennis < darach at gmail dot com > 
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%%
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% File: bench.erl. Quick and dirty micro benchmark experiments
%%
%% -------------------------------------------------------------------

-module(bench).
-export([main/1]).

main(_) ->
    % tunables
    process_flag(priority, max),
    % reduces outliers for purpose of the micro benchmark
    process_flag(min_heap_size, trunc(math:pow(2,26))),
    %dbg:tracer(port,dbg:trace_port(file,"binary_gc.dump")),
    %dbg:p(self(),[garbage_collection,timestamp]),

    io:format("~s ~s ~n~n", [ 
        cake:fg(blue,"JCH. "),
        cake:fg(green,"Performance microbenchmark suite.")
        ] ),

    %
    % BEAM warmup. Take care of startup anomalies ...
    %

    b(jch,ch, fun() -> [random:uniform(1000000000),1000000] end, 10, "warmup"),
    b(jch,ch, fun() -> [random:uniform(1000000000),1000000] end, 10, "warmup"),
    b(jch,ch, fun() -> [random:uniform(1000000000),1000000] end, 10, "warmup"),
    b(jch,ch, fun() -> [random:uniform(1000000000),1000000] end, 10, "warmup"),
    b(jch,ch, fun() -> [random:uniform(1000000000),1000000] end, 10, "warmup"),
    b(jch,ch, fun() -> [random:uniform(1000000000),1000000] end, 10, "warmup"),

    %
    % Fundamental performance characteristics by:
    % 1. Runs, Increasing event frequency. GC at end of each run
    % 2. Repeated with increasing bucket sizes
    %

    io:format("~n~s ~s ~n~n", [ 
        cake:fg(blue,"- 100 Buckets. "),
        cake:fg(green,"Hash performance.")
        ] ),
    b(jch,ch, fun() -> [random:uniform(1000000000),100] end, 10, "ch100"),
    b(jch,ch, fun() -> [random:uniform(1000000000),100] end, 100, "ch100"),
    b(jch,ch, fun() -> [random:uniform(1000000000),100] end, 1000, "ch100"),
    b(jch,ch, fun() -> [random:uniform(1000000000),100] end, 10000, "ch100"),
    b(jch,ch, fun() -> [random:uniform(1000000000),100] end, 100000, "ch100"),
    b(jch,ch, fun() -> [random:uniform(1000000000),100] end, 1000000, "ch100"),
    %b(jch,ch, fun() -> [random:uniform(1000000000),100] end, 10000000, "ch100"),

    io:format("~n~n~s ~s ~n~n", [ 
        cake:fg(blue,"- 10K Buckets. "),
        cake:fg(green,"Hash performance.")
        ] ),
    b(jch,ch, fun() -> [random:uniform(1000000000),10000] end, 10, "ch10k"),
    b(jch,ch, fun() -> [random:uniform(1000000000),10000] end, 100, "ch10k"),
    b(jch,ch, fun() -> [random:uniform(1000000000),10000] end, 1000, "ch10k"),
    b(jch,ch, fun() -> [random:uniform(1000000000),10000] end, 10000, "ch10k"),
    b(jch,ch, fun() -> [random:uniform(1000000000),10000] end, 100000, "ch10k"),
    b(jch,ch, fun() -> [random:uniform(1000000000),10000] end, 1000000, "ch10k"),
    %b(jch,ch, fun() -> [random:uniform(1000000000),10000] end, 10000000, "ch10k"),

    io:format("~n~n~s ~s ~n~n", [ 
        cake:fg(blue,"- 1M Buckets. "),
        cake:fg(green,"Hash performance.")
        ] ),
    b(jch,ch, fun() -> [random:uniform(1000000000),1000000] end, 10, "ch1m"),
    b(jch,ch, fun() -> [random:uniform(1000000000),1000000] end, 100, "ch1m"),
    b(jch,ch, fun() -> [random:uniform(1000000000),1000000] end, 1000, "ch1m"),
    b(jch,ch, fun() -> [random:uniform(1000000000),1000000] end, 10000, "ch1m"),
    b(jch,ch, fun() -> [random:uniform(1000000000),1000000] end, 100000, "ch1m"),
    b(jch,ch, fun() -> [random:uniform(1000000000),1000000] end, 1000000, "ch1m"),
    %b(jch,ch, fun() -> [random:uniform(1000000000),1000000] end, 10000000, "ch1m"),

    io:format("~n~n~s ~s ~n~n", [ 
        cake:fg(blue,"- 1B Buckets. "),
        cake:fg(green,"Hash performance.")
        ] ),
    b(jch,ch, fun() -> [random:uniform(1000000000),1000000000] end, 10, "ch1b"),
    b(jch,ch, fun() -> [random:uniform(1000000000),1000000000] end, 100, "ch1b"),
    b(jch,ch, fun() -> [random:uniform(1000000000),1000000000] end, 1000, "ch1b"),
    b(jch,ch, fun() -> [random:uniform(1000000000),1000000000] end, 10000, "ch1b"),
    b(jch,ch, fun() -> [random:uniform(1000000000),1000000000] end, 100000, "ch1b"),
    b(jch,ch, fun() -> [random:uniform(1000000000),1000000000] end, 1000000, "ch1b"),
    %b(jch,ch, fun() -> [random:uniform(1000000000),1000000000] end, 10000000, "ch1b"),

    %dbg:stop(),

    %%
    %% Record partition hit frequency during a long run by
    %% maintaining frequency counters in a dict(). Allows
    %% ascertaining distribution and dispersion artefacts
    %% statistically. We don't care about speed here.
    %%
    io:format("~n~n~s ~s ~n~n", [ 
        cake:fg(blue,"- 32 Buckets. 1M hashes"),
        cake:fg(green,"Uniform Distribution Check.")
        ] ),
    c(32,1000000),
    
    erlang:garbage_collect(),
    
    io:format("~n~n~s ~s ~n~n", [ 
        cake:fg(blue,"- 32 Buckets. 5M hashes"),
        cake:fg(green,"Uniform Distribution Check.")
        ] ),
    c(32,5000000),
    
    %erlang:garbage_collect(),
    %
    %io:format("~n~n~s ~s ~n~n", [ 
    %    cake:fg(blue,"- 32 Buckets. 10M hashes"),
    %    cake:fg(green,"Uniform Distribution Check.")
    %    ] ),
    %c(32,10000000),

    erlang:garbage_collect(),
   
    io:format("done!~n"),

    halt(0).

c(B,N) ->
    S = os:timestamp(),
    C = lists:foldl( tt(B), dict:new(), lists:seq(1,N)),
    E = os:timestamp(),
    Elapsed = timer:now_diff(E,S),
    L = [ begin {_,A} = X, A end || X <- dict:to_list(C) ],
    Length = length(L),
    L1 = lists:sublist(L,1,8),
    L2 = lists:sublist(L,9,8),
    L3 = lists:sublist(L,17,8),
    L4 = lists:sublist(L,25,8),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    Stdevs = math:sqrt(round(lists:foldl(fun(X, Acc) -> Acc + (X-Avg)*(X-Avg) end, 0, L))),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("~s ~s ~s ~s ~s ~s ~s ~s~n", [ cake:fg(white,string:left(io_lib:format("~p", [X]),6)) || X <- L1]),
    io:format("~s ~s ~s ~s ~s ~s ~s ~s~n", [ cake:fg(white,string:left(io_lib:format("~p", [X]),6)) || X <- L2]),
    io:format("~s ~s ~s ~s ~s ~s ~s ~s~n", [ cake:fg(white,string:left(io_lib:format("~p", [X]),6)) || X <- L3]),
    io:format("~s ~s ~s ~s ~s ~s ~s ~s~n", [ cake:fg(white,string:left(io_lib:format("~p", [X]),6)) || X <- L4]),
    io:format("~s  ~s~6..-b  ~s~6..-b  ~s~6..-b  ~s~6..-b  ~s~9.. b~n   ~s~.4f  ~s~.4f ~s~.4f ~s~.4f~n~n", [
        cake:fg(blue,string:left("oOo|",8)), 
        cake:fg(yellow,string:left("Min: ",6)), Min, 
        cake:fg(yellow,string:left("Max: ",6)), Max,
        cake:fg(yellow,string:left("Median: ",8)), Med,
        cake:fg(yellow,string:left("Avg: ",6)), Avg,
        cake:fg(green,string:left("Elapsed: ",8)), Elapsed,
        cake:ta(bold,cake:fg(red,string:left("Worst: ",8))), (Min*100/Max),
        cake:ta(bold,cake:fg(red,string:left("Med: ",8))), (Med*100/Max),
        cake:ta(bold,cake:fg(red,string:left("Avg: ",8))), (Avg*100/Max),
        cake:ta(bold,cake:fg(red,string:left("RSD: ",10))), (Stdevs*100/Avg)
    ]),

    ok.

tt(X) ->
    fun(I,D) ->
        K = jch:ch(I,X),
        case dict:is_key(K,D) of
            false -> dict:store(K,1,D); 
            true -> dict:store(K,dict:fetch(K,D)+1,D)
        end
    end.

b(M, F, A, N, X) when N > 0 ->
    S = os:timestamp(),
    L = loop(M, F, A, N, []),
    E = os:timestamp(),
    Elapsed = timer:now_diff(E,S),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(C, Sum) -> C + Sum end, 0, L) / Length),
    io:format("~s  ~s~10.. b ~s~6..-b  ~s~6..-b  ~s~6..-b  ~s~6..-b  ~s~9.. b ~n", [
        cake:fg(blue,string:left(X,8)), 
        cake:fg(magenta,string:left("N: ",4)), N, 
        cake:fg(yellow,string:left("Min: ",6)), Min, 
        cake:fg(yellow,string:left("Max: ",6)), Max,
        cake:fg(yellow,string:left("Median: ",8)), Med,
        cake:fg(yellow,string:left("Avg: ",6)), Avg,
        cake:fg(green,string:left("Elapsed: ",8)), Elapsed
    ]),
    erlang:garbage_collect(),
    ok.
 
loop(_M, _F, _A, 0, List) ->
    List;
loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A()),
    loop(M, F, A, N - 1, [T|List]).
