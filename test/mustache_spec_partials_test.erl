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

-module(mustache_spec_partials_test).

-include_lib("eunit/include/eunit.hrl").

partials_spec_test_() ->
   [
    %% The greater-than operator should expand to the named partial.
    ?_assertEqual({ok, <<"\"from partial\"">>},
                  render(<<"\"{{>text}}\"">>,
                         #{},
                         #{template_sources => #{<<"text">> => {string, <<"from partial">>}}})),
    %% The empty string should be used when the named partial is not found.
    ?_assertEqual({ok, <<"\"\"">>},
                  render(<<"\"{{>text}}\"">>,
                         #{},
                         #{template_sources => #{}})),
    %% The greater-than operator should operate within the current context.
    ?_assertEqual({ok, <<"\"*content*\"">>},
                  render(<<"\"{{>partial}}\"">>,
                         #{text => <<"content">>},
                         #{template_sources => #{<<"partial">> => {string, <<"*{{text}}*">>}}})),
    %% The greater-than operator should properly recurse.
    ?_assertEqual({ok, <<"X<Y<>>">>},
                  render(<<"{{>node}}">>,
                         #{content => <<"X">>, nodes => [#{content => <<"Y">>, nodes => []}]},
                         #{template_sources => #{<<"node">> => {string, <<"{{content}}<{{#nodes}}{{>node}}{{/nodes}}>">>}}})),
    %% The greater-than operator should not alter surrounding whitespace.
    ?_assertEqual({ok, <<"| 	|	 |">>},
                  render(<<"| {{>partial}} |">>,
                         #{},
                         #{template_sources => #{<<"partial">> => {string, <<"	|	">>}}})),
    %% Whitespace should be left untouched.
    ?_assertEqual({ok, <<"  |  >\n>\n">>},
                  render(<<"  {{data}}  {{> partial}}\n">>,
                         #{data => <<"|">>},
                         #{template_sources => #{<<"partial">> => {string, <<">\n>">>}}})),
    %% "\r\n" should be considered a newline for standalone tags.
    ?_assertEqual({ok, <<"|\r\n>|">>},
                  render(<<"|\r\n{{>partial}}\r\n|">>,
                         #{},
                         #{template_sources => #{<<"partial">> => {string, <<">">>}}})),
    %% Standalone tags should not require a newline to precede them.
    ?_assertEqual({ok, <<"  >\n  >>">>},
                  render(<<"  {{>partial}}\n>">>,
                         #{},
                         #{template_sources => #{<<"partial">> => {string, <<">\n>">>}}})),
    %% Standalone tags should not require a newline to follow them.
    ?_assertEqual({ok, <<">\n  >\n  >">>},
                  render(<<">\n  {{>partial}}">>,
                         #{},
                         #{template_sources => #{<<"partial">> => {string, <<">\n>">>}}})),
    %% Each line of the partial should be indented before rendering.
    ?_assertEqual({ok, <<"\\\n |\n <\n->\n |\n/\n">>},
                  render(<<"\\\n {{>partial}}\n/\n">>,
                         #{content => <<"<\n->">>},
                         #{template_sources => #{<<"partial">> => {string, <<"|\n{{{content}}}\n|\n">>}}})),
    %% Superfluous in-tag whitespace should be ignored.
    ?_assertEqual({ok, <<"|[]|">>},
                  render(<<"|{{> partial }}|">>,
                         #{boolean => true},
                         #{template_sources => #{<<"partial">> => {string, <<"[]">>}}}))
  ].

-spec render(binary(), mustache:context(), mustache:options()) ->
        {ok, binary()} | {error, mustache:error()}.
render(TemplateString, Context, Options0) ->
  Options = Options0#{return_binary => true},
  mustache:render(<<"test">>, {string, TemplateString}, Context, Options).
