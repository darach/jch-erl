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
%% This implementation uses the xorshift64* PRNG rather than the LCG PRNG in the paper.
%%
%% -------------------------------------------------------------------

-module(jch).
-export([ch/2]).
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
ch(_Key,_Buckets) when is_integer(_Key) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).
