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

-module(mustache_spec_interpolation_test).

-include_lib("eunit/include/eunit.hrl").

interpolation_spec_test_() ->
   [
    %% Mustache-free templates should render as-is.
    ?_assertEqual({ok, <<"Hello from {Mustache}!\n">>},
                  render(<<"Hello from {Mustache}!\n">>,
                         #{},
                         #{})),
    %% Unadorned tags should interpolate content into the template.
    ?_assertEqual({ok, <<"Hello, world!\n">>},
                  render(<<"Hello, {{subject}}!\n">>,
                         #{subject => <<"world">>},
                         #{})),
    %% Basic interpolation should be HTML escaped.
    ?_assertEqual({ok, <<"These characters should be HTML escaped: &amp; &quot; &lt; &gt;\n">>},
                  render(<<"These characters should be HTML escaped: {{forbidden}}\n">>,
                         #{forbidden => <<"& \" < >">>},
                         #{})),
    %% Triple mustaches should interpolate without HTML escaping.
    ?_assertEqual({ok, <<"These characters should not be HTML escaped: & \" < >\n">>},
                  render(<<"These characters should not be HTML escaped: {{{forbidden}}}\n">>,
                         #{forbidden => <<"& \" < >">>},
                         #{})),
    %% Ampersand should interpolate without HTML escaping.
    ?_assertEqual({ok, <<"These characters should not be HTML escaped: & \" < >\n">>},
                  render(<<"These characters should not be HTML escaped: {{&forbidden}}\n">>,
                         #{forbidden => <<"& \" < >">>},
                         #{})),
    %% Integers should interpolate seamlessly.
    ?_assertEqual({ok, <<"\"85 miles an hour!\"">>},
                  render(<<"\"{{mph}} miles an hour!\"">>,
                         #{mph => 85},
                         #{})),
    %% Integers should interpolate seamlessly.
    ?_assertEqual({ok, <<"\"85 miles an hour!\"">>},
                  render(<<"\"{{{mph}}} miles an hour!\"">>,
                         #{mph => 85},
                         #{})),
    %% Integers should interpolate seamlessly.
    ?_assertEqual({ok, <<"\"85 miles an hour!\"">>},
                  render(<<"\"{{&mph}} miles an hour!\"">>,
                         #{mph => 85},
                         #{})),
    %% Decimals should interpolate seamlessly with proper significance.
    ?_assertEqual({ok, <<"\"1.21 jiggawatts!\"">>},
                  render(<<"\"{{power}} jiggawatts!\"">>,
                         #{power => 1.21},
                         #{})),
    %% Decimals should interpolate seamlessly with proper significance.
    ?_assertEqual({ok, <<"\"1.21 jiggawatts!\"">>},
                  render(<<"\"{{{power}}} jiggawatts!\"">>,
                         #{power => 1.21},
                         #{})),
    %% Decimals should interpolate seamlessly with proper significance.
    ?_assertEqual({ok, <<"\"1.21 jiggawatts!\"">>},
                  render(<<"\"{{&power}} jiggawatts!\"">>,
                         #{power => 1.21},
                         #{})),
    %% Failed context lookups should default to empty strings.
    ?_assertEqual({ok, <<"I () be seen!">>},
                  render(<<"I ({{cannot}}) be seen!">>,
                         #{},
                         #{})),
    %% Failed context lookups should default to empty strings.
    ?_assertEqual({ok, <<"I () be seen!">>},
                  render(<<"I ({{{cannot}}}) be seen!">>,
                         #{},
                         #{})),
    %% Failed context lookups should default to empty strings.
    ?_assertEqual({ok, <<"I () be seen!">>},
                  render(<<"I ({{&cannot}}) be seen!">>,
                         #{},
                         #{})),
    %% Dotted names should be considered a form of shorthand for sections.
    ?_assertEqual({ok, <<"\"Joe\" == \"Joe\"">>},
                  render(<<"\"{{person.name}}\" == \"{{#person}}{{name}}{{/person}}\"">>,
                         #{person => #{name => <<"Joe">>}},
                         #{})),
    %% Dotted names should be considered a form of shorthand for sections.
    ?_assertEqual({ok, <<"\"Joe\" == \"Joe\"">>},
                  render(<<"\"{{{person.name}}}\" == \"{{#person}}{{{name}}}{{/person}}\"">>,
                         #{person => #{name => <<"Joe">>}},
                         #{})),
    %% Dotted names should be considered a form of shorthand for sections.
    ?_assertEqual({ok, <<"\"Joe\" == \"Joe\"">>},
                  render(<<"\"{{&person.name}}\" == \"{{#person}}{{&name}}{{/person}}\"">>,
                         #{person => #{name => <<"Joe">>}},
                         #{})),
    %% Dotted names should be functional to any level of nesting.
    ?_assertEqual({ok, <<"\"Phil\" == \"Phil\"">>},
                  render(<<"\"{{a.b.c.d.e.name}}\" == \"Phil\"">>,
                         #{a => #{b => #{c => #{d => #{e => #{name => <<"Phil">>}}}}}},
                         #{})),
    %% Any falsey value prior to the last part of the name should yield ''.
    ?_assertEqual({ok, <<"\"\" == \"\"">>},
                  render(<<"\"{{a.b.c}}\" == \"\"">>,
                         #{a => #{}},
                         #{})),
    %% Each part of a dotted name should resolve only against its parent.
    ?_assertEqual({ok, <<"\"\" == \"\"">>},
                  render(<<"\"{{a.b.c.name}}\" == \"\"">>,
                         #{a => #{b => #{}}, c => #{name => <<"Jim">>}},
                         #{})),
    %% The first part of a dotted name should resolve as any other name.
    ?_assertEqual({ok, <<"\"Phil\" == \"Phil\"">>},
                  render(<<"\"{{#a}}{{b.c.d.e.name}}{{/a}}\" == \"Phil\"">>,
                         #{a => #{b => #{c => #{d => #{e => #{name => <<"Phil">>}}}}}, b => #{c => #{d => #{e => #{name => <<"Wrong">>}}}}},
                         #{})),
    %% Interpolation should not alter surrounding whitespace.
    ?_assertEqual({ok, <<"| --- |">>},
                  render(<<"| {{string}} |">>,
                         #{string => <<"---">>},
                         #{})),
    %% Interpolation should not alter surrounding whitespace.
    ?_assertEqual({ok, <<"| --- |">>},
                  render(<<"| {{{string}}} |">>,
                         #{string => <<"---">>},
                         #{})),
    %% Interpolation should not alter surrounding whitespace.
    ?_assertEqual({ok, <<"| --- |">>},
                  render(<<"| {{&string}} |">>,
                         #{string => <<"---">>},
                         #{})),
    %% Standalone interpolation should not alter surrounding whitespace.
    ?_assertEqual({ok, <<"  ---\n">>},
                  render(<<"  {{string}}\n">>,
                         #{string => <<"---">>},
                         #{})),
    %% Standalone interpolation should not alter surrounding whitespace.
    ?_assertEqual({ok, <<"  ---\n">>},
                  render(<<"  {{{string}}}\n">>,
                         #{string => <<"---">>},
                         #{})),
    %% Standalone interpolation should not alter surrounding whitespace.
    ?_assertEqual({ok, <<"  ---\n">>},
                  render(<<"  {{&string}}\n">>,
                         #{string => <<"---">>},
                         #{})),
    %% Superfluous in-tag whitespace should be ignored.
    ?_assertEqual({ok, <<"|---|">>},
                  render(<<"|{{ string }}|">>,
                         #{string => <<"---">>},
                         #{})),
    %% Superfluous in-tag whitespace should be ignored.
    ?_assertEqual({ok, <<"|---|">>},
                  render(<<"|{{{ string }}}|">>,
                         #{string => <<"---">>},
                         #{})),
    %% Superfluous in-tag whitespace should be ignored.
    ?_assertEqual({ok, <<"|---|">>},
                  render(<<"|{{& string }}|">>,
                         #{string => <<"---">>},
                         #{}))
  ].

-spec render(binary(), mustache:context(), mustache:options()) ->
        {ok, binary()} | {error, mustache:error()}.
render(TemplateString, Context, Options0) ->
  Options = Options0#{return_binary => true},
  mustache:render(<<"test">>, {string, TemplateString}, Context, Options).
