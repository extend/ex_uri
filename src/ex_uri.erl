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
         encode_pct/2,
         resolve/2,
         merge/2,
         remove_dot_segments/1]).

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

%% @spec resolve(Ref :: ref(), Base :: uri()) -> uri()
%% @doc Resolve an URI reference with a given base URI.
resolve(#ex_uri_ref{authority = RA, path = RP, q = RQ, fragment = RF},
        #ex_uri{scheme = BS, authority = BA, path = BP, q = BQ}) ->
  [TA, TP, TQ] = case RA of
                   undefined ->
                     Acc = case RP of
                             "" ->
                               TQ1 = case RQ of
                                       undefined -> BQ;
                                       RQ -> RQ end,
                               [BP, TQ1];
                             RP ->
                               TP1 = case RP of
                                       [$/ | _] -> RP;
                                       RP -> merge(RP, BP) end,
                               [remove_dot_segments(TP1), RQ] end,
                     [BA | Acc];
                   RA ->
                     [RA, remove_dot_segments(RP), RQ] end,
  #ex_uri{scheme = BS, authority = TA, path = TP, q = TQ, fragment = RF};
resolve(Ref = #ex_uri{path = RP}, #ex_uri{}) ->
  Ref#ex_uri{path = remove_dot_segments(RP)}.

%% @spec merge(string(), string()) -> string()
%% @doc Merge a relative path with a given base path.
merge(Rel, Base) ->
  merge(Rel, Base, "", "").

%% @spec remove_dot_segments(string()) -> string()
%% @doc Remove superfluous dot segments from a given path.
remove_dot_segments(Path) ->
  remove_dot_segments1(Path).

%% @hidden
encode_pct([C | String], Chars, Acc) ->
  case lists:member(C, Chars) of
    true ->
      [D1, D2] = erlang:integer_to_list(C, 16),
      encode_pct(String, Chars, [D2, D1, $% | Acc]);
    false -> encode_pct(String, Chars, [C | Acc]) end.

%% @hidden
merge(Rel, [$/ | Base], Last, Acc) ->
  merge(Rel, Base, [], [$/ | Last] ++ Acc);
merge(Rel, [C | Base], Last, Acc) ->
  merge(Rel, Base, [C | Last], Acc);
merge(Rel, [], _Last, Acc) ->
  lists:reverse(Acc, Rel).

%% @hidden
remove_dot_segments1("/" ++ Path) ->
  remove_dot_segments1(Path, "/", "");
remove_dot_segments1(Path) ->
  remove_dot_segments1(Path, "", "").

%% @hidden
remove_dot_segments1("", Prefix, Acc) ->
  Acc1 = Prefix ++ lists:reverse(Acc),
  case Acc1 of
    "//" ++ _ -> Prefix ++ [$. | Acc1];
    _ -> Acc1 end;
remove_dot_segments1(Path, Prefix, Acc) ->
  Pred = fun (C) -> C =/= $/ end,
  {S, R} = case lists:splitwith(Pred, Path) of
             {Segment, "/" ++ Rest} -> {Segment ++ "/", Rest};
             Result -> Result end,
  if
    S =:= "."; S =:= "./" ->
      remove_dot_segments1(R, Prefix, Acc);
    S =:= ".."; S =:= "../" ->
      Acc2 = case lists:dropwhile(Pred, Acc) of
               "/" ++ Acc1 -> Acc1;
               Acc1 -> Acc1 end,
      remove_dot_segments1(R, Prefix, lists:dropwhile(Pred, Acc2));
    true ->
      remove_dot_segments1(R, Prefix, lists:reverse(S, Acc)) end.


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

%% @hidden
resolve_test_() ->
  Base1 = "http://a/b/c/d;p?q",
  Base2 = "http://a/b/c/d;p?q=1/2",
  Base3 = "http://a/b/c/d;p=1/2?q",
  Base4 = "fred:///s//a/b/c",
  Base5 = "http:///s//a/b/c",
  Data = [% http://tools.ietf.org/html/rfc3986#section-5.4.1
          {"g:h", Base1, "g:h"},
          {"g", Base1, "http://a/b/c/g"},
          {"./g", Base1, "http://a/b/c/g"},
          {"g/", Base1, "http://a/b/c/g/"},
          {"/g", Base1, "http://a/g"},
          {"//g", Base1, "http://g"},
          {"?y", Base1, "http://a/b/c/d;p?y"},
          {"g?y", Base1, "http://a/b/c/g?y"},
          {"#s", Base1, "http://a/b/c/d;p?q#s"},
          {"g#s", Base1, "http://a/b/c/g#s"},
          {"g?y#s", Base1, "http://a/b/c/g?y#s"},
          {";x", Base1, "http://a/b/c/;x"},
          {"g;x", Base1, "http://a/b/c/g;x"},
          {"g;x?y#s", Base1, "http://a/b/c/g;x?y#s"},
          {"", Base1, Base1},
          {".", Base1, "http://a/b/c/"},
          {"./", Base1, "http://a/b/c/"},
          {"..", Base1, "http://a/b/"},
          {"../", Base1, "http://a/b/"},
          {"../g", Base1, "http://a/b/g"},
          {"../..", Base1, "http://a/"},
          {"../../", Base1, "http://a/"},
          {"../../g", Base1, "http://a/g"},
          % http://tools.ietf.org/html/rfc3986#section-5.4.2
          {"../../../g", Base1, "http://a/g"},
          {"../../../../g", Base1, "http://a/g"},
          {"/./g", Base1, "http://a/g"},
          {"/../g", Base1, "http://a/g"},
          {"g.", Base1, "http://a/b/c/g."},
          {".g", Base1, "http://a/b/c/.g"},
          {"g..", Base1, "http://a/b/c/g.."},
          {"..g", Base1, "http://a/b/c/..g"},
          {"./../g", Base1, "http://a/b/g"},
          {"./g/.", Base1, "http://a/b/c/g/"},
          {"g/./h", Base1, "http://a/b/c/g/h"},
          {"g/../h", Base1, "http://a/b/c/h"},
          {"g;x=1/./y", Base1, "http://a/b/c/g;x=1/y"},
          {"g;x=1/../y", Base1, "http://a/b/c/y"},
          {"g?y/./x", Base1, "http://a/b/c/g?y/./x"},
          {"g?y/../x", Base1, "http://a/b/c/g?y/../x"},
          {"g#s/./x", Base1, "http://a/b/c/g#s/./x"},
          {"g#s/../x", Base1, "http://a/b/c/g#s/../x"},
          {"http:g", Base1, "http:g"},
          % http://labs.apache.org/webarch/uri/test/rel_examples2.html
          {"g", Base2, "http://a/b/c/g"},
          {"./g", Base2, "http://a/b/c/g"},
          {"g/", Base2, "http://a/b/c/g/"},
          {"/g", Base2, "http://a/g"},
          {"//g", Base2, "http://g"},
          {"?y", Base2, "http://a/b/c/d;p?y"},
          {"g?y", Base2, "http://a/b/c/g?y"},
          {"g?y/./x", Base2, "http://a/b/c/g?y/./x"},
          {"g?y/../x", Base2, "http://a/b/c/g?y/../x"},
          {"g#s", Base2, "http://a/b/c/g#s"},
          {"g#s/./x", Base2, "http://a/b/c/g#s/./x"},
          {"g#s/../x", Base2, "http://a/b/c/g#s/../x"},
          {"./", Base2, "http://a/b/c/"},
          {"../", Base2, "http://a/b/"},
          {"../g", Base2, "http://a/b/g"},
          {"../../", Base2, "http://a/"},
          {"../../g", Base2, "http://a/g"},
          % http://labs.apache.org/webarch/uri/test/rel_examples3.html
          {"g", Base3, "http://a/b/c/d;p=1/g"},
          {"./g", Base3, "http://a/b/c/d;p=1/g"},
          {"g/", Base3, "http://a/b/c/d;p=1/g/"},
          {"g?y", Base3, "http://a/b/c/d;p=1/g?y"},
          {";x", Base3, "http://a/b/c/d;p=1/;x"},
          {"g;x", Base3, "http://a/b/c/d;p=1/g;x"},
          {"g;x=1/./y", Base3, "http://a/b/c/d;p=1/g;x=1/y"},
          {"g;x=1/../y", Base3, "http://a/b/c/d;p=1/y"},
          {"./", Base3, "http://a/b/c/d;p=1/"},
          {"../", Base3, "http://a/b/c/"},
          {"../g", Base3, "http://a/b/c/g"},
          {"../../", Base3, "http://a/b/"},
          {"../../g", Base3, "http://a/b/g"},
          % http://labs.apache.org/webarch/uri/test/rel_examples4.html
          {"g:h", Base4, "g:h"},
          {"g", Base4, "fred:///s//a/b/g"},
          {"./g", Base4, "fred:///s//a/b/g"},
          {"g/", Base4, "fred:///s//a/b/g/"},
          {"/g", Base4, "fred:///g"},
          {"//g", Base4, "fred://g"},
          {"//g/x", Base4, "fred://g/x"},
          {"///g", Base4, "fred:///g"},
          {"./", Base4, "fred:///s//a/b/"},
          {"../", Base4, "fred:///s//a/"},
          {"../g", Base4, "fred:///s//a/g"},
          {"../../", Base4, "fred:///s//"},
          {"../../g", Base4, "fred:///s//g"},
          {"../../../g", Base4, "fred:///s/g"},
          {"../../../../g", Base4, "fred:///g"},
          % http://labs.apache.org/webarch/uri/test/rel_examples5.html
          {"g:h", Base5, "g:h"},
          {"g", Base5, "http:///s//a/b/g"},
          {"./g", Base5, "http:///s//a/b/g"},
          {"g/", Base5, "http:///s//a/b/g/"},
          {"/g", Base5, "http:///g"},
          {"//g", Base5, "http://g"},
          {"//g/x", Base5, "http://g/x"},
          {"///g", Base5, "http:///g"},
          {"./", Base5, "http:///s//a/b/"},
          {"../", Base5, "http:///s//a/"},
          {"../g", Base5, "http:///s//a/g"},
          {"../../", Base5, "http:///s//"},
          {"../../g", Base5, "http:///s//g"},
          {"../../../g", Base5, "http:///s/g"},
          {"../../../../g", Base5, "http:///g"},
          % http://www.w3.org/2000/10/swap/uripath.py
          {"bar:abc", "foo:xyz", "bar:abc"},
          {"../abc", "http://example/x/y/z", "http://example/x/abc"},
          {"http://example/x/abc", "http://example2/x/y/z",
           "http://example/x/abc"},
          {"../r", "http://ex/x/y/z", "http://ex/x/r"},
          {"q/r", "http://ex/x/y", "http://ex/x/q/r"},
          {"q/r#s", "http://ex/x/y", "http://ex/x/q/r#s"},
          {"q/r#s/t", "http://ex/x/y", "http://ex/x/q/r#s/t"},
          {"ftp://ex/x/q/r", "http://ex/x/y", "ftp://ex/x/q/r"},
          {"", "http://ex/x/y", "http://ex/x/y"},
          {"", "http://ex/x/y/", "http://ex/x/y/"},
          {"", "http://ex/x/y/pdq", "http://ex/x/y/pdq"},
          {"z/", "http://ex/x/y/", "http://ex/x/y/z/"},
          {"#Animal", "file:/swap/test/animal.rdf",
           "file:/swap/test/animal.rdf#Animal"},
          {"../abc", "file:/e/x/y/z", "file:/e/x/abc"},
          {"/example/x/abc", "file:/example2/x/y/z", "file:/example/x/abc"},
          {"../r", "file:/ex/x/y/z", "file:/ex/x/r"},
          {"/r", "file:/ex/x/y/z", "file:/r"},
          {"q/r", "file:/ex/x/y", "file:/ex/x/q/r"},
          {"q/r#s", "file:/ex/x/y", "file:/ex/x/q/r#s"},
          {"q/r#", "file:/ex/x/y", "file:/ex/x/q/r#"},
          {"q/r#s/t", "file:/ex/x/y", "file:/ex/x/q/r#s/t"},
          {"ftp://ex/x/q/r", "file:/ex/x/y", "ftp://ex/x/q/r"},
          {"", "file:/ex/x/y", "file:/ex/x/y"},
          {"", "file:/ex/x/y/", "file:/ex/x/y/"},
          {"", "file:/ex/x/y/pdq", "file:/ex/x/y/pdq"},
          {"z/", "file:/ex/x/y/", "file:/ex/x/y/z/"},
          {"file://meetings.example.com/cal#m1",
           "file:/devel/WWW/2000/10/swap/test/reluri-1.n3",
           "file://meetings.example.com/cal#m1"},
          {"file://meetings.example.com/cal#m1",
           "file:/home/connolly/w3ccvs/WWW/2000/10/swap/test/reluri-1.n3",
           "file://meetings.example.com/cal#m1"},
          {"./#blort", "file:/some/dir/foo", "file:/some/dir/#blort"},
          {"./#", "file:/some/dir/foo", "file:/some/dir/#"},
          % http://www.ninebynine.org/Software/HaskellUtils/Network/UriTest.xls
          {"./q:r", "http://ex/x/y", "http://ex/x/q:r"},
          {"./p=q:r", "http://ex/x/y", "http://ex/x/p=q:r"},
          {"?pp/rr", "http://ex/x/y?pp/qq", "http://ex/x/y?pp/rr"},
          {"y/z", "http://ex/x/y?pp/qq", "http://ex/x/y/z"},
          {"local/qual@domain.org#frag", "mailto:local",
           "mailto:local/qual@domain.org#frag"},
          {"more/qual2@domain2.org#frag", "mailto:local/qual1@domain1.org",
           "mailto:local/more/qual2@domain2.org#frag"},
          {"y?q", "http://ex/x/y?q", "http://ex/x/y?q"},
          {"/x/y?q", "http://ex?p", "http://ex/x/y?q"},
          {"c/d", "foo:a/b", "foo:a/c/d"},
          {"/c/d", "foo:a/b", "foo:/c/d"},
          {"", "foo:a/b?c#d", "foo:a/b?c"},
          {"b/c", "foo:a", "foo:b/c"},
          {"../b/c", "foo:/a/y/z", "foo:/a/b/c"},
          {"./b/c", "foo:a", "foo:b/c"},
          {"/./b/c", "foo:a", "foo:/b/c"},
          {"../../d", "foo://a//b/c", "foo://a/d"},
          {".", "foo:a", "foo:"},
          {"..", "foo:a", "foo:"},
          {"abc", "http://example/x/y%2Fz", "http://example/x/abc"},
          {"../../x%2Fabc", "http://example/a/x/y/z",
           "http://example/a/x%2Fabc"},
          {"../x%2Fabc", "http://example/a/x/y%2Fz",
           "http://example/a/x%2Fabc"},
          {"abc", "http://example/x%2Fy/z", "http://example/x%2Fy/abc"},
          {"q%3Ar", "http://ex/x/y", "http://ex/x/q%3Ar"},
          {"/x%2Fabc", "http://example/x/y%2Fz", "http://example/x%2Fabc"},
          {"/x%2Fabc", "http://example/x/y/z", "http://example/x%2Fabc"},
          {"/x%2Fabc", "http://example/x/y%2Fz", "http://example/x%2Fabc"},
          {"local2@domain2", "mailto:local1@domain1?query1",
           "mailto:local2@domain2"},
          {"local2@domain2?query2", "mailto:local1@domain1",
           "mailto:local2@domain2?query2"},
          {"local2@domain2?query2", "mailto:local1@domain1?query1",
           "mailto:local2@domain2?query2"},
          {"?query2", "mailto:local@domain?query1",
           "mailto:local@domain?query2"},
          {"local@domain?query2", "mailto:?query1",
           "mailto:local@domain?query2"},
          {"?query2", "mailto:local@domain?query1",
           "mailto:local@domain?query2"},
          {"http://example/a/b?c/../d", "foo:bar",
           "http://example/a/b?c/../d"},
          {"http://example/a/b#c/../d", "foo:bar",
           "http://example/a/b#c/../d"},
          {"http:this", "http://example.org/base/uri", "http:this"},
          {"http:this", "http:base", "http:this"},
          {".//g", "f:/a", "f:/.//g"},
          {"b/c//d/e", "f://example.org/base/a",
           "f://example.org/base/b/c//d/e"},
          {"m2@example.ord/c2@example.org", "mid:m@example.ord/c@example.org",
           "mid:m@example.ord/m2@example.ord/c2@example.org"},
          {"mini1.xml",
           "file:///C:/DEV/Haskell/lib/HXmlToolbox-3.01/examples/",
            "file:///C:/DEV/Haskell/lib/HXmlToolbox-3.01/examples/mini1.xml"},
          {"../b/c", "foo:a/y/z", "foo:a/b/c"},
          % http://lists.w3.org/Archives/Public/uri/2004Feb/0114.html
          {"../c", "foo:a/b", "foo:c"},
          {"foo:.", "foo:a", "foo:"},
          {"/foo/../../../bar", "zz:abc", "zz:/bar"},
          {"/foo/../bar", "zz:abc", "zz:/bar"},
          {"foo/../../../bar", "zz:abc", "zz:bar"},
          {"foo/../bar", "zz:abc", "zz:bar"},
          {"zz:.", "zz:abc", "zz:"},
          {"/.", Base1, "http://a/"},
          {"/.foo", Base1, "http://a/.foo"},
          {".foo", Base1, "http://a/b/c/.foo"}],
  Test = fun (R, B, T) ->
               Title = B ++ " + " ++ R ++ " = " ++ T,
               {Title,
                fun () ->
                      {ok, Ref, ""} = ex_uri:decode_ref(R),
                      {ok, Base, ""} = ex_uri:decode(B),
                      Target = ex_uri:resolve(Ref, Base),
                      T = ex_uri:encode(Target) end} end,
  [ Test(R, B, T) || {R, B, T} <- Data ].
