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

-module(mustache_spec_inverted_test).

-include_lib("eunit/include/eunit.hrl").

inverted_spec_test_() ->
   [
    %% Falsey sections should have their contents rendered.
    ?_assertEqual({ok, <<"\"This should be rendered.\"">>},
                  render(<<"\"{{^boolean}}This should be rendered.{{/boolean}}\"">>,
                         #{boolean => false},
                         #{})),
    %% Truthy sections should have their contents omitted.
    ?_assertEqual({ok, <<"\"\"">>},
                  render(<<"\"{{^boolean}}This should not be rendered.{{/boolean}}\"">>,
                         #{boolean => true},
                         #{})),
    %% Objects and hashes should behave like truthy values.
    ?_assertEqual({ok, <<"\"\"">>},
                  render(<<"\"{{^context}}Hi {{name}}.{{/context}}\"">>,
                         #{context => #{name => <<"Joe">>}},
                         #{})),
    %% Lists should behave like truthy values.
    ?_assertEqual({ok, <<"\"\"">>},
                  render(<<"\"{{^list}}{{n}}{{/list}}\"">>,
                         #{list => [#{n => 1}, #{n => 2}, #{n => 3}]},
                         #{})),
    %% Empty lists should behave like falsey values.
    ?_assertEqual({ok, <<"\"Yay lists!\"">>},
                  render(<<"\"{{^list}}Yay lists!{{/list}}\"">>,
                         #{list => []},
                         #{})),
    %% Multiple inverted sections per template should be permitted.
    ?_assertEqual({ok, <<"* first\n* second\n* third\n">>},
                  render(<<"{{^bool}}\n* first\n{{/bool}}\n* {{two}}\n{{^bool}}\n* third\n{{/bool}}\n">>,
                         #{two => <<"second">>, bool => false},
                         #{})),
    %% Nested falsey sections should have their contents rendered.
    ?_assertEqual({ok, <<"| A B C D E |">>},
                  render(<<"| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} E |">>,
                         #{bool => false},
                         #{})),
    %% Nested truthy sections should be omitted.
    ?_assertEqual({ok, <<"| A  E |">>},
                  render(<<"| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} E |">>,
                         #{bool => true},
                         #{})),
    %% Failed context lookups should be considered falsey.
    ?_assertEqual({ok, <<"[Cannot find key 'missing'!]">>},
                  render(<<"[{{^missing}}Cannot find key 'missing'!{{/missing}}]">>,
                         #{},
                         #{})),
    %% Dotted names should be valid for Inverted Section tags.
    ?_assertEqual({ok, <<"\"\" == \"\"">>},
                  render(<<"\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"\"">>,
                         #{a => #{b => #{c => true}}},
                         #{})),
    %% Dotted names should be valid for Inverted Section tags.
    ?_assertEqual({ok, <<"\"Not Here\" == \"Not Here\"">>},
                  render(<<"\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"Not Here\"">>,
                         #{a => #{b => #{c => false}}},
                         #{})),
    %% Dotted names that cannot be resolved should be considered falsey.
    ?_assertEqual({ok, <<"\"Not Here\" == \"Not Here\"">>},
                  render(<<"\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"Not Here\"">>,
                         #{a => #{}},
                         #{})),
    %% Inverted sections should not alter surrounding whitespace.
    ?_assertEqual({ok, <<" | 	|	 | \n">>},
                  render(<<" | {{^boolean}}	|	{{/boolean}} | \n">>,
                         #{boolean => false},
                         #{})),
    %% Inverted should not alter internal whitespace.
    ?_assertEqual({ok, <<" |  \n  | \n">>},
                  render(<<" | {{^boolean}} {{! Important Whitespace }}\n {{/boolean}} | \n">>,
                         #{boolean => false},
                         #{})),
    %% Single-line sections should not alter surrounding whitespace.
    ?_assertEqual({ok, <<" NO\n WAY\n">>},
                  render(<<" {{^boolean}}NO{{/boolean}}\n {{^boolean}}WAY{{/boolean}}\n">>,
                         #{boolean => false},
                         #{})),
    %% Standalone lines should be removed from the template.
    ?_assertEqual({ok, <<"| This Is\n|\n| A Line\n">>},
                  render(<<"| This Is\n{{^boolean}}\n|\n{{/boolean}}\n| A Line\n">>,
                         #{boolean => false},
                         #{})),
    %% Standalone indented lines should be removed from the template.
    ?_assertEqual({ok, <<"| This Is\n|\n| A Line\n">>},
                  render(<<"| This Is\n  {{^boolean}}\n|\n  {{/boolean}}\n| A Line\n">>,
                         #{boolean => false},
                         #{})),
    %% "\r\n" should be considered a newline for standalone tags.
    ?_assertEqual({ok, <<"|\r\n|">>},
                  render(<<"|\r\n{{^boolean}}\r\n{{/boolean}}\r\n|">>,
                         #{boolean => false},
                         #{})),
    %% Standalone tags should not require a newline to precede them.
    ?_assertEqual({ok, <<"^\n/">>},
                  render(<<"  {{^boolean}}\n^{{/boolean}}\n/">>,
                         #{boolean => false},
                         #{})),
    %% Standalone tags should not require a newline to follow them.
    ?_assertEqual({ok, <<"^\n/\n">>},
                  render(<<"^{{^boolean}}\n/\n  {{/boolean}}">>,
                         #{boolean => false},
                         #{})),
    %% Superfluous in-tag whitespace should be ignored.
    ?_assertEqual({ok, <<"|=|">>},
                  render(<<"|{{^ boolean }}={{/ boolean }}|">>,
                         #{boolean => false},
                         #{}))
  ].

-spec render(binary(), mustache:context(), mustache:options()) ->
        {ok, binary()} | {error, mustache:error()}.
render(TemplateString, Context, Options0) ->
  Options = Options0#{return_binary => true},
  mustache:render(<<"test">>, {string, TemplateString}, Context, Options).
