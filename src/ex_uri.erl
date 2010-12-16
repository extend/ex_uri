%% Copyright (c) 2010, Dev:Extend
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%  * Redistributions of source code must retain the above copyright notice,
%%    this list of conditions and the following disclaimer.
%%  * Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%  * Neither the name of Dev:Extend nor the names of its contributors may be
%%    used to endorse or promote products derived from this software without
%%    specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

-module(ex_uri).
-include("ex_uri.hrl").

-export([decode/1,
         decode_ref/1,
         decode_abs/1,
         decode_pct/1]).

%% @spec decode(string()) -> {ok, #ex_uri{}, string()}
%% @doc Decode an URI.
decode(String) ->
  {ok, _URI, _Rest} = ex_uri_parser:decode('URI', String).

%% @spec decode_ref(string()) -> {ok, #ex_uri{} | #ex_uri_ref{}, string()}
%% @doc Decode an URI reference.
decode_ref(String) ->
  {ok, _Ref, _Rest} = ex_uri_parser:decode('URI-reference', String).

%% @spec decode_abs(string()) -> {ok, #ex_uri{}, string()}
%% @doc Decode an absolute URI.
decode_abs(String) ->
  {ok, _URI, _Rest} = ex_uri_parser:decode('absolute-URI', String).

%% @spec decode_pct(string()) -> string()
%% @doc Decode a percent-encoded string.
decode_pct(String) ->
  decode_pct(String, []).


%% @hidden
decode_pct([C | String], Acc) when C =/= $% ->
  decode_pct(String, [C | Acc]);
decode_pct([$%, D1, D2 | String], Acc) ->
  decode_pct(String, [erlang:list_to_integer([D1, D2], 16) | Acc]);
decode_pct([], Acc) ->
  lists:reverse(Acc).
