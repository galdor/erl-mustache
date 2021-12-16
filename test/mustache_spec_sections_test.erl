%% Copyright (c) 2020-2021 Exograd SAS.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(mustache_spec_sections_test).

-include_lib("eunit/include/eunit.hrl").

sections_spec_test_() ->
   [
    %% Truthy sections should have their contents rendered.
    ?_assertEqual({ok, <<"\"This should be rendered.\"">>},
                  render(<<"\"{{#boolean}}This should be rendered.{{/boolean}}\"">>,
                         #{boolean => true},
                         #{})),
    %% Falsey sections should have their contents omitted.
    ?_assertEqual({ok, <<"\"\"">>},
                  render(<<"\"{{#boolean}}This should not be rendered.{{/boolean}}\"">>,
                         #{boolean => false},
                         #{})),
    %% Objects and hashes should be pushed onto the context stack.
    ?_assertEqual({ok, <<"\"Hi Joe.\"">>},
                  render(<<"\"{{#context}}Hi {{name}}.{{/context}}\"">>,
                         #{context => #{name => <<"Joe">>}},
                         #{})),
    %% All elements on the context stack should be accessible.
    ?_assertEqual({ok, <<"1\n121\n12321\n1234321\n123454321\n1234321\n12321\n121\n1\n">>},
                  render(<<"{{#a}}\n{{one}}\n{{#b}}\n{{one}}{{two}}{{one}}\n{{#c}}\n{{one}}{{two}}{{three}}{{two}}{{one}}\n{{#d}}\n{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}\n{{#e}}\n{{one}}{{two}}{{three}}{{four}}{{five}}{{four}}{{three}}{{two}}{{one}}\n{{/e}}\n{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}\n{{/d}}\n{{one}}{{two}}{{three}}{{two}}{{one}}\n{{/c}}\n{{one}}{{two}}{{one}}\n{{/b}}\n{{one}}\n{{/a}}\n">>,
                         #{a => #{one => 1}, b => #{two => 2}, c => #{three => 3}, d => #{four => 4}, e => #{five => 5}},
                         #{})),
    %% Lists should be iterated; list items should visit the context stack.
    ?_assertEqual({ok, <<"\"123\"">>},
                  render(<<"\"{{#list}}{{item}}{{/list}}\"">>,
                         #{list => [#{item => 1}, #{item => 2}, #{item => 3}]},
                         #{})),
    %% Empty lists should behave like falsey values.
    ?_assertEqual({ok, <<"\"\"">>},
                  render(<<"\"{{#list}}Yay lists!{{/list}}\"">>,
                         #{list => []},
                         #{})),
    %% Multiple sections per template should be permitted.
    ?_assertEqual({ok, <<"* first\n* second\n* third\n">>},
                  render(<<"{{#bool}}\n* first\n{{/bool}}\n* {{two}}\n{{#bool}}\n* third\n{{/bool}}\n">>,
                         #{two => <<"second">>, bool => true},
                         #{})),
    %% Nested truthy sections should have their contents rendered.
    ?_assertEqual({ok, <<"| A B C D E |">>},
                  render(<<"| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |">>,
                         #{bool => true},
                         #{})),
    %% Nested falsey sections should be omitted.
    ?_assertEqual({ok, <<"| A  E |">>},
                  render(<<"| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |">>,
                         #{bool => false},
                         #{})),
    %% Failed context lookups should be considered falsey.
    ?_assertEqual({ok, <<"[]">>},
                  render(<<"[{{#missing}}Found key 'missing'!{{/missing}}]">>,
                         #{},
                         #{})),
    %% Implicit iterators should directly interpolate strings.
    ?_assertEqual({ok, <<"\"(a)(b)(c)(d)(e)\"">>},
                  render(<<"\"{{#list}}({{.}}){{/list}}\"">>,
                         #{list => [<<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"e">>]},
                         #{})),
    %% Implicit iterators should cast integers to strings and interpolate.
    ?_assertEqual({ok, <<"\"(1)(2)(3)(4)(5)\"">>},
                  render(<<"\"{{#list}}({{.}}){{/list}}\"">>,
                         #{list => [1, 2, 3, 4, 5]},
                         #{})),
    %% Implicit iterators should cast decimals to strings and interpolate.
    ?_assertEqual({ok, <<"\"(1.1)(2.2)(3.3)(4.4)(5.5)\"">>},
                  render(<<"\"{{#list}}({{.}}){{/list}}\"">>,
                         #{list => [1.1, 2.2, 3.3, 4.4, 5.5]},
                         #{})),
    %% Implicit iterators should allow iterating over nested arrays.
    ?_assertEqual({ok, <<"\"(123)(abc)\"">>},
                  render(<<"\"{{#list}}({{#.}}{{.}}{{/.}}){{/list}}\"">>,
                         #{list => [[1, 2, 3], [<<"a">>, <<"b">>, <<"c">>]]},
                         #{})),
    %% Dotted names should be valid for Section tags.
    ?_assertEqual({ok, <<"\"Here\" == \"Here\"">>},
                  render(<<"\"{{#a.b.c}}Here{{/a.b.c}}\" == \"Here\"">>,
                         #{a => #{b => #{c => true}}},
                         #{})),
    %% Dotted names should be valid for Section tags.
    ?_assertEqual({ok, <<"\"\" == \"\"">>},
                  render(<<"\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\"">>,
                         #{a => #{b => #{c => false}}},
                         #{})),
    %% Dotted names that cannot be resolved should be considered falsey.
    ?_assertEqual({ok, <<"\"\" == \"\"">>},
                  render(<<"\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\"">>,
                         #{a => #{}},
                         #{})),
    %% Sections should not alter surrounding whitespace.
    ?_assertEqual({ok, <<" | 	|	 | \n">>},
                  render(<<" | {{#boolean}}	|	{{/boolean}} | \n">>,
                         #{boolean => true},
                         #{})),
    %% Sections should not alter internal whitespace.
    ?_assertEqual({ok, <<" |  \n  | \n">>},
                  render(<<" | {{#boolean}} {{! Important Whitespace }}\n {{/boolean}} | \n">>,
                         #{boolean => true},
                         #{})),
    %% Single-line sections should not alter surrounding whitespace.
    ?_assertEqual({ok, <<" YES\n GOOD\n">>},
                  render(<<" {{#boolean}}YES{{/boolean}}\n {{#boolean}}GOOD{{/boolean}}\n">>,
                         #{boolean => true},
                         #{})),
    %% Standalone lines should be removed from the template.
    ?_assertEqual({ok, <<"| This Is\n|\n| A Line\n">>},
                  render(<<"| This Is\n{{#boolean}}\n|\n{{/boolean}}\n| A Line\n">>,
                         #{boolean => true},
                         #{})),
    %% Indented standalone lines should be removed from the template.
    ?_assertEqual({ok, <<"| This Is\n|\n| A Line\n">>},
                  render(<<"| This Is\n  {{#boolean}}\n|\n  {{/boolean}}\n| A Line\n">>,
                         #{boolean => true},
                         #{})),
    %% "\r\n" should be considered a newline for standalone tags.
    ?_assertEqual({ok, <<"|\r\n|">>},
                  render(<<"|\r\n{{#boolean}}\r\n{{/boolean}}\r\n|">>,
                         #{boolean => true},
                         #{})),
    %% Standalone tags should not require a newline to precede them.
    ?_assertEqual({ok, <<"#\n/">>},
                  render(<<"  {{#boolean}}\n#{{/boolean}}\n/">>,
                         #{boolean => true},
                         #{})),
    %% Standalone tags should not require a newline to follow them.
    ?_assertEqual({ok, <<"#\n/\n">>},
                  render(<<"#{{#boolean}}\n/\n  {{/boolean}}">>,
                         #{boolean => true},
                         #{})),
    %% Superfluous in-tag whitespace should be ignored.
    ?_assertEqual({ok, <<"|=|">>},
                  render(<<"|{{# boolean }}={{/ boolean }}|">>,
                         #{boolean => true},
                         #{}))
  ].

-spec render(binary(), mustache:context(), mustache:options()) ->
        {ok, binary()} | {error, mustache:error()}.
render(TemplateString, Context, Options0) ->
  Options = Options0#{return_binary => true},
  mustache:render(<<"test">>, {string, TemplateString}, Context, Options).
