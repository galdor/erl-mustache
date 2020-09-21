%% Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

-module(mustache_generator_test).

-include_lib("eunit/include/eunit.hrl").

render_text_test_() ->
  [?_assertEqual({ok, <<"">>},
                 render(<<"">>, #{})),
   ?_assertEqual({ok, <<"Foo {bar}.">>},
                 render(<<"Foo {bar}.">>, #{}))].

render_variables_test_() ->
  [?_assertEqual({ok, <<"Foo: bar.">>},
                 render(<<"Foo: {{a}}.">>, #{a => <<"bar">>})),
   ?_assertEqual({ok, <<"Foo: 42">>},
                 render(<<"Foo: {{a}}">>, #{a => 42})),
   ?_assertEqual({ok, <<"#{a =&gt; 1}">>},
                 render(<<"{{.}}">>, #{a => 1})),
   ?_assertEqual({ok, <<"42">>},
                 render(<<"{{a.b}}">>, #{a => #{b => 42}})),
   ?_assertEqual({ok, <<"">>},
                 render(<<"{{a.c}}">>, #{a => #{b => 42}})),
   ?_assertEqual({ok, <<"">>},
                 render(<<"{{b}}">>, #{a => 42})),
   ?_assertEqual({ok, <<"">>},
                 render(<<"{{a.b}}">>, [1, 2, 3])),
   ?_assertEqual({ok, <<"[1,2,3]">>},
                 render(<<"{{.}}">>, [1, 2, 3])),
   ?_assertEqual({ok, <<"&amp; &quot; &apos; &lt; &gt;">>},
                 render(<<"{{msg}}">>, #{msg => <<"& \" ' < >">>})),
   ?_assertEqual({ok, <<"& \" ' < >">>},
                 render(<<"{{msg}}">>, #{msg => <<"& \" ' < >">>},
                        #{disable_html_escaping => true})),
   ?_assertEqual({ok, <<"été">>},
                 render(<<"{{msg}}">>, #{msg => <<"été">>})),
   ?_assertMatch({error, #{position := {1, 1},
                           tag_name := <<"foo">>,
                           reason := unknown_variable}},
                 render(<<"{{foo}}">>, #{},
                        #{error_on_unknown_variable => true}))].

render_unescaped_variables_test_() ->
  [?_assertEqual({ok, <<"& \" ' < >">>},
                 render(<<"{{{msg}}}">>, #{msg => <<"& \" ' < >">>})),
   ?_assertEqual({ok, <<"& \" ' < >">>},
                 render(<<"{{&msg}}">>, #{msg => <<"& \" ' < >">>})),
   ?_assertEqual({ok, <<"été">>},
                 render(<<"{{&msg}}">>, #{msg => <<"été">>})),
   ?_assertMatch({error, #{position := {1, 1},
                           tag_name := <<"foo">>,
                           reason := unknown_variable}},
                 render(<<"{{{foo}}}">>, #{},
                        #{error_on_unknown_variable => true})),
   ?_assertMatch({error, #{position := {1, 1},
                           tag_name := <<"foo">>,
                           reason := unknown_variable}},
                 render(<<"{{&foo}}">>, #{},
                        #{error_on_unknown_variable => true}))].

render_sections_test_() ->
  [?_assertEqual({ok, <<"foo">>},
                 render(<<"{{#a}}foo{{/a}}">>, #{a => 42})),
   ?_assertEqual({ok, <<"">>},
                 render(<<"{{#a}}foo{{/a}}">>, #{a => false})),
   ?_assertEqual({ok, <<"42">>},
                 render(<<"{{#a}}{{b}}{{/a}}">>, #{a => #{b => 42}})),
   ?_assertEqual({ok, <<"b: ">>},
                 render(<<"{{#a}}b: {{b}}{{/a}}">>, #{a => #{}})),
   ?_assertEqual({ok, <<"b: 42">>},
                 render(<<"{{#a}}b: {{b}}{{/a}}">>, #{a => #{}, b => 42})),
   ?_assertEqual({ok, <<"b: 42">>},
                 render(<<"{{#a}}b: {{b}}{{/a}}">>, #{a => true, b => 42})),
   ?_assertEqual({ok, <<"">>},
                 render(<<"{{#a}}b: {{b}}{{/a}}">>, #{a => false, b => 42})),
   ?_assertEqual({ok, <<"c: 42">>},
                 render(<<"{{#a.b}}c: {{c}}{{/a.b}}">>,
                        #{a => #{b => #{c => 42}}})),
   ?_assertEqual({ok, <<"c: ">>},
                 render(<<"{{#a.b}}c: {{c}}{{/a.b}}">>, #{a => #{b => #{}}})),
   ?_assertEqual({ok, <<"">>},
                 render(<<"{{#a.b}}c: {{c}}{{/a.b}}">>, #{a => 42})),
   ?_assertEqual({ok, <<"">>},
                 render(<<"{{#a.b}}c: {{c}}{{/a.b}}">>, #{})),
   ?_assertEqual({ok, <<">1>2>3">>},
                 render(<<"{{#a}}>{{.}}{{/a}}">>, #{a => [1, 2, 3]})),
   ?_assertEqual({ok, <<">1>2">>},
                 render(<<"{{#a}}>{{b}}{{/a}}">>,
                        #{a => [#{b => 1}, #{b => 2}]})),
   ?_assertEqual({ok, <<">>>">>},
                 render(<<"{{#a}}>{{b}}{{/a}}">>,
                        #{a => [1, 2, 3]})),
   ?_assertEqual({ok, <<"-+1-+4+5+6">>},
                 render(<<"{{#a}}-{{#a}}+{{.}}{{/a}}{{/a}}">>,
                        #{a => [#{a => 1}, #{a => [4, 5, 6]}]})),
   ?_assertEqual({ok, <<"c: 1">>},
                 render(<<"{{#a}}{{#b}}c: {{c}}{{/b}}{{/a}}">>,
                        #{a => #{b => #{c => 1}}})),
   ?_assertEqual({ok, <<"c: 2">>},
                 render(<<"{{#a}}{{#b}}c: {{c}}{{/b}}{{/a}}">>,
                        #{a => #{b => #{}},
                          c => 2})),
   ?_assertEqual({ok, <<"c: 1">>},
                 render(<<"{{#a}}{{#b}}c: {{c}}{{/b}}{{/a}}">>,
                        #{a => #{b => #{c => 1}},
                          c => 2})),
   ?_assertEqual({ok, <<"e: 42">>},
                 render(<<"{{#a.b}}{{#c.d}}e: {{e}}{{/c.d}}{{/a.b}}">>,
                        #{a => #{b => #{c => #{d => #{e => 42}}}}})),
   ?_assertEqual({ok, <<"e: ">>},
                 render(<<"{{#a.b}}{{#c.d}}e: {{e}}{{/c.d}}{{/a.b}}">>,
                        #{a => #{b => #{c => #{d => #{},
                                               e => 42}}}})),
   ?_assertEqual({ok, <<"e: 42">>},
                 render(<<"{{#a.b}}{{#c.d}}e: {{e}}{{/c.d}}{{/a.b}}">>,
                        #{a => #{b => #{c => #{d => #{}},
                                        e => 42}}})),
   ?_assertEqual({ok, <<"e: ">>},
                 render(<<"{{#a.b}}{{#c.d}}e: {{e}}{{/c.d}}{{/a.b}}">>,
                        #{a => #{b => #{c => #{d => #{}}},
                                 e => 42}})),
   ?_assertEqual({ok, <<"e: 42">>},
                 render(<<"{{#a.b}}{{#c.d}}e: {{e}}{{/c.d}}{{/a.b}}">>,
                        #{a => #{b => #{c => #{d => #{}}}},
                          e => 42})),
   ?_assertMatch({error, #{position := {1, 1},
                           tag_name := <<"foo">>,
                           reason := unknown_variable}},
                 render(<<"{{#foo}}a: {{a}}{{/foo}}">>, #{a => 42},
                        #{error_on_unknown_variable => true})),
   ?_assertMatch({error, #{position := {1, 12},
                           tag_name := <<"a">>,
                           reason := unknown_variable}},
                 render(<<"{{#foo}}a: {{a}}{{/foo}}">>,
                        #{foo => true},
                        #{error_on_unknown_variable => true}))].

render_inverted_sections_test_() ->
  [?_assertEqual({ok, <<"false">>},
                 render(<<"{{^a}}false{{/a}}">>, #{})),
   ?_assertEqual({ok, <<"b: 42">>},
                 render(<<"{{^a}}b: {{b}}{{/a}}">>, #{b => 42})),
   ?_assertEqual({ok, <<"false">>},
                 render(<<"{{^a}}false{{/a}}">>, #{a => false})),
   ?_assertEqual({ok, <<"false">>},
                 render(<<"{{^a}}false{{/a}}">>, #{a => []})),
   ?_assertEqual({ok, <<"false">>},
                 render(<<"{{^a}}false{{/a}}">>, #{a => #{}})),
   ?_assertEqual({ok, <<"false">>},
                 render(<<"{{^a}}false{{/a}}">>, #{a => <<>>})),
   ?_assertEqual({ok, <<"">>},
                 render(<<"{{^a}}false{{/a}}">>, #{a => true})),
   ?_assertEqual({ok, <<"">>},
                 render(<<"{{^a}}false{{/a}}">>, #{a => 42})),
   ?_assertEqual({ok, <<"false">>},
                 render(<<"{{^a}}{{^a}}{{^b}}false{{/b}}{{/a}}{{/a}}">>,
                        #{})),
   ?_assertEqual({ok, <<"">>},
                 render(<<"{{^a}}{{^a}}{{^b}}false{{/b}}{{/a}}{{/a}}">>,
                        #{b => 42})),
   ?_assertEqual({ok, <<"false">>},
                 render(<<"{{^a.b.c}}false{{/a.b.c}}">>,
                        #{a => #{b => #{}}})),
   ?_assertEqual({ok, <<"false">>},
                 render(<<"{{^a.b.c}}false{{/a.b.c}}">>, #{a => #{}})),
   ?_assertEqual({ok, <<"">>},
                 render(<<"{{^a.b.c}}false{{/a.b.c}}">>,
                        #{a => #{b => #{c => 42}}}))].

render_partials_test_() ->
  [?_assertEqual({ok, <<"Hello: [42].">>},
                 render(<<"Hello: {{> a}}.">>,
                        #{foo => 42},
                        #{template_sources =>
                            #{<<"a">> => {string, <<"[{{foo}}]">>}}})),
   ?_assertEqual({ok, <<"Hello: [42 [foobar]].">>},
                 render(<<"Hello: {{> a}}.">>,
                        #{foo => 42,
                          bar => <<"foobar">>},
                        #{template_sources =>
                            #{<<"a">> => {string, <<"[{{foo}} {{>b}}]">>},
                              <<"b">> => {string, <<"[{{bar}}]">>}}})),
   ?_assertMatch({error, #{position := {1, 8},
                           tag_name := <<"a">>,
                           reason := unknown_partial,
                           partial_error := #{reason := template_not_found,
                                              template_name := <<"a">>}}},
                 render(<<"Hello: {{> a}}.">>,
                        #{},
                        #{template_sources => #{},
                          error_on_unknown_partial => true})),
   ?_assertMatch({error, #{position := {1, 8},
                           tag_name := <<"a">>,
                           reason := invalid_partial,
                           partial_error :=
                             #{template_name := <<"a">>,
                               template_full_name := <<"a">>,
                               position := {1, 1},
                               reason := truncated_tag}}},
                 render(<<"Hello: {{> a}}.">>,
                        #{foo => 42},
                        #{template_sources =>
                            #{<<"a">> => {string, <<"{{foo">>}},
                          error_on_invalid_partial => true})),
   ?_assertMatch({error, #{position := {1, 8},
                           tag_name := <<"a">>,
                           reason := invalid_partial,
                           partial_error :=
                             #{template_name := <<"a">>,
                               template_full_name := <<"a">>,
                               position := {1, 2},
                               tag_name := <<"b">>,
                               reason := invalid_partial,
                               partial_error :=
                                 #{template_name := <<"b">>,
                                   template_full_name := <<"b">>,
                                   position := {1, 1},
                                   reason := empty_tag}}}},
                 render(<<"Hello: {{> a}}.">>,
                        #{foo => 42},
                        #{template_sources =>
                            #{<<"a">> => {string, <<"[{{> b}}]">>},
                              <<"b">> => {string, <<"{{}}">>}},
                          error_on_invalid_partial => true}))].

render_comments_test_() ->
  [?_assertEqual({ok, <<"Foo .">>},
                 render(<<"Foo {{!foo bar}}.">>, #{})),
   ?_assertEqual({ok, <<"">>},
                 render(<<"{{!foo {bar} baz!}}">>, #{}))].

render_delimiters_test_() ->
  [?_assertEqual({ok, <<"1{{b}}">>},
                 render(<<"{{=<% %>=}}<%a%>{{b}}">>, #{a => 1, b => 2})),
   ?_assertEqual({ok, <<"234">>},
                 render(<<"{{#a}}{{b}}{{=[[ ]]=}}[[c]][[/a]][[d]]">>,
                        #{a => 1, b => 2, c => 3, d => 4})),
   ?_assertEqual({ok, <<"<<foo>>barbar##foo##<<foo>>bar">>},
                 render(<<"{{>a}}{{foo}}##foo##{{=<< >>=}}<<>a>>">>,
                        #{foo => <<"bar">>},
                        #{template_sources =>
                            #{<<"a">> =>
                                {string, <<"<<foo>>{{=## ##=}}##foo##">>}}})),
   ?_assertMatch({error, #{position := {1, 1},
                           reason := {invalid_delimiters, invalid_format}}},
                 render(<<"{{=##=}}">>, #{})),
   ?_assertMatch({error, #{position := {1, 1},
                           reason := {invalid_delimiters, invalid_character}}},
                 render(<<"{{=<= =>=}}">>, #{}))].

-spec render(binary(), mustache:context()) ->
        {ok, binary()} | {error, mustache:error()}.
render(TemplateString, Context) ->
  render(TemplateString, Context, #{}).

-spec render(binary(), mustache:context(), mustache:options()) ->
        {ok, binary()} | {error, mustache:error()}.
render(TemplateString, Context, Options0) ->
  Options = Options0#{return_binary => true},
  mustache:render(<<"test">>, {string, TemplateString}, Context, Options).
