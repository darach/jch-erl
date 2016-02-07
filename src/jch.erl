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
%% File: jch.erl. Jump Consistent Hashing
%%
%% NIF wrapper for Jump Consistent Hash algorithm by John Lamping and Eric Veach
%% developed at Google, Inc. Paper: "A Fast, Minimal Memory, Consistent Hash Algorithm.
%% This implementation uses the xorshift64* PRNG rather than the LCG PRNG in the paper
%% by default, but can be switched to compatible algorithm by passing 3'rd argument
%% as atom 'orig'.
%%
%% -------------------------------------------------------------------

-module(jch).
-export([ch/2, ch/3]).
-on_load(init/0).

init() ->
    SoName = filename:join(
        case code:priv_dir(?MODULE) of
            {error, bad_name} ->
                Dir = code:which(?MODULE),
                filename:join([filename:dirname(Dir),"..","priv"]);
            Dir -> Dir
        end, atom_to_list(?MODULE) ++ "_nif"),
    erlang:load_nif(SoName, 0).

-spec ch(Key,Buckets) -> Hash when
    Key :: integer(),
    Buckets :: integer(),
    Hash :: integer().
ch(Key, Buckets) ->
    ch(Key, Buckets, xorshift64).


-spec ch(Key, Buckets, Type) -> Hash when
    Key :: integer(),
    Buckets :: integer(),
    Type :: orig | xorshift64,
    Hash :: integer().
ch(Key, Buckets, Type) when is_integer(Key) andalso (Key >= 0)
                            andalso  is_integer(Buckets) andalso (Buckets > 0)
                            andalso ((Type == orig) or (Type == xorshift64)) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ch_xorshift_test_() ->
    Cases =
        %% {Expect, Key, Buckets}
        [{0, 0, 1},
         {0, 3, 1},
         {0, 0, 2},
         {1, 2, 2},
         {0, 4, 2},
         {29, 1, 128},
         {113, 129, 128},
         {0, 0, 100000000},
         {82916011, 128, 100000000},
         {239467867, 128, 2147483648},
         {78, 18446744073709551615, 128}
        ],
    [?_assertEqual(Expect, jch:ch(K, B)) || {Expect, K, B} <- Cases].

ch_orig_test_() ->
    Cases =
        %% {Expect, Key, Buckets}
        [{0, 0, 1},
         {0, 3, 1},
         {0, 0, 2},
         {1, 4, 2},
         {0, 7, 2},
         {55, 1, 128},
         {120, 129, 128},
         {0, 0, 100000000},
         {38172097, 128, 100000000},
         {1644467860, 128, 2147483648},
         {92, 18446744073709551615, 128}
        ],
    [?_assertEqual(Expect, jch:ch(K, B, orig)) || {Expect, K, B} <- Cases].


%% https://github.com/beefsack/jch-rs/blob/master/src/lib.rs#L30
ch_range_test() ->
    test_ch_range(orig, 0),
    test_ch_range(xorshift64, 0).

test_ch_range(_, 10000) -> ok;
test_ch_range(Algo, Key) ->
    LastVal = ch(Key, 1),
    test_ch_range(Algo, Key, LastVal, 1),
    test_ch_range(Algo, Key + 1).

test_ch_range(_Algo, _Key, _LastVal, 100) -> ok;
test_ch_range(Algo, Key, LastVal, Buckets) ->
    Val = ch(Key, Buckets, Algo),
    %% io:format("ch(~p, ~p, ~p) -> ~p~n", [Key, Buckets, Algo, Val]),
    ?assert((Val == LastVal) orelse (Val == Buckets - 1)),
    test_ch_range(Algo, Key, Val, Buckets + 1).

-endif.
