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

%% @type uri() = #ex_uri{}.
%% @type ref() = #ex_uri_ref{} | uri().

-module(ex_uri).
-include("ex_uri.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([decode/1,
         decode_ref/1,
         decode_abs/1,
         decode_pct/1,
         encode/1,
         encode_pct/2]).

%% @spec decode(string()) -> {ok, uri(), string()}
%% @doc Decode an URI.
decode(String) ->
  {ok, _URI, _Rest} = ex_uri_parser:decode('URI', String).

%% @spec decode_ref(string()) -> {ok, ref(), string()}
%% @doc Decode an URI reference.
decode_ref(String) ->
  {ok, _Ref, _Rest} = ex_uri_parser:decode('URI-reference', String).

%% @spec decode_abs(string()) -> {ok, uri(), string()}
%% @doc Decode an absolute URI.
decode_abs(String) ->
  {ok, _URI, _Rest} = ex_uri_parser:decode('absolute-URI', String).

%% @spec decode_pct(string()) -> string()
%% @doc Decode a percent-encoded string.
decode_pct(String) ->
  decode_pct(String, []).

%% @spec encode(Value :: uri() | ref()) -> string()
%% @doc Encode an URI or an URI reference.
encode(#ex_uri{scheme = Scheme, authority = Authority, path = Path,
               q = Query, fragment = Fragment}) ->
  Acc = [$:, Scheme],
  encode(Authority, Path, Query, Fragment, Acc);
encode(#ex_uri_ref{authority = Authority, path = Path, q = Query,
                   fragment = Fragment}) ->
  encode(Authority, Path, Query, Fragment, []).

%% @spec encode_pct(string(), [char()]) -> string()
%% @doc Percent-encode a given set of chars in a string.
encode_pct(String, Chars) ->
  encode_pct(String, Chars, []).

%% @hidden
decode_pct([C | String], Acc) when C =/= $% ->
  decode_pct(String, [C | Acc]);
decode_pct([$%, D1, D2 | String], Acc) ->
  decode_pct(String, [erlang:list_to_integer([D1, D2], 16) | Acc]);
decode_pct([], Acc) ->
  lists:reverse(Acc).

%% @spec encode(#ex_uri_authority{} | undefined,
%%              string(),
%%              string() | undefined,
%%              string() | undefined,
%%              string()) -> string()
encode(Authority, Path, Query, Fragment, Acc) ->
  Acc1 = case Authority of
           undefined -> Acc;
           A -> encode_authority(A, [$/, $/ | Acc]) end,
  Acc2 = [Path | Acc1],
  Acc3 = case Query of
           undefined -> Acc2;
           Q -> [Q, $? | Acc2] end,
  Acc4 = case Fragment of
           undefined -> Acc3;
           F -> [F, $# | Acc3] end,
  lists:flatten(lists:reverse(Acc4)).

%% @spec encode_authority(Authority :: #ex_uri_authority{},
%%                        string()) -> string()
encode_authority(#ex_uri_authority{userinfo = UserInfo,
                                   host = Host,
                                   port = Port}, Acc) ->
  Acc1 = case UserInfo of
           undefined -> Acc;
           UI -> [$@, UI | Acc] end,
  Acc2 = [Host | Acc1],
  case Port of
    undefined -> Acc2;
    P -> [integer_to_list(P), $: | Acc2] end.

%% @hidden
encode_pct([C | String], Chars, Acc) ->
  case lists:member(C, Chars) of
    true ->
      [D1, D2] = erlang:integer_to_list(C, 16),
      encode_pct(String, Chars, [D2, D1, $% | Acc]);
    false -> encode_pct(String, Chars, [C | Acc]) end.


-ifdef(TEST).

%% @hidden
simple_uris() ->
  [{"foo://example.com:8042/over/there?name=ferret#nose",
    #ex_uri{scheme = "foo",
            authority = #ex_uri_authority{host = "example.com",
                                          port = 8042},
            path = "/over/there", q = "name=ferret",
            fragment = "nose"}},
   {"urn:example:animal:ferret:nose",
    #ex_uri{scheme = "urn", path = "example:animal:ferret:nose"}}].

-endif.

%% @hidden
decode_test_() ->
  Test = fun (String, URI) ->
               fun () ->
                     {ok, URI, ""} = decode(String) end end,
  [ Test(String, URI) || {String, URI} <- simple_uris() ].

%% @hidden
encode_test_() ->
  Test = fun (String, URI) ->
               fun () ->
                     String = encode(URI) end end,
  [ Test(String, URI) || {String, URI} <- simple_uris() ].
