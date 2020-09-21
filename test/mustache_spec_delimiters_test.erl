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

-module(mustache_spec_delimiters_test).

-include_lib("eunit/include/eunit.hrl").

delimiters_spec_test_() ->
   [
    %% The equals sign (used on both sides) should permit delimiter changes.
    ?_assertEqual({ok, <<"(Hey!)">>},
                  render(<<"{{=<% %>=}}(<%text%>)">>,
                         #{text => <<"Hey!">>},
                         #{})),
    %% Characters with special meaning regexen should be valid delimiters.
    ?_assertEqual({ok, <<"(It worked!)">>},
                  render(<<"({{=[ ]=}}[text])">>,
                         #{text => <<"It worked!">>},
                         #{})),
    %% Delimiters set outside sections should persist.
    ?_assertEqual({ok, <<"[\n  I got interpolated.\n  |data|\n\n  {{data}}\n  I got interpolated.\n]\n">>},
                  render(<<"[\n{{#section}}\n  {{data}}\n  |data|\n{{/section}}\n\n{{= | | =}}\n|#section|\n  {{data}}\n  |data|\n|/section|\n]\n">>,
                         #{section => true, data => <<"I got interpolated.">>},
                         #{})),
    %% Delimiters set outside inverted sections should persist.
    ?_assertEqual({ok, <<"[\n  I got interpolated.\n  |data|\n\n  {{data}}\n  I got interpolated.\n]\n">>},
                  render(<<"[\n{{^section}}\n  {{data}}\n  |data|\n{{/section}}\n\n{{= | | =}}\n|^section|\n  {{data}}\n  |data|\n|/section|\n]\n">>,
                         #{section => false, data => <<"I got interpolated.">>},
                         #{})),
    %% Delimiters set in a parent template should not affect a partial.
    ?_assertEqual({ok, <<"[ .yes. ]\n[ .yes. ]\n">>},
                  render(<<"[ {{>include}} ]\n{{= | | =}}\n[ |>include| ]\n">>,
                         #{value => <<"yes">>},
                         #{template_sources => #{<<"include">> => {string, <<".{{value}}.">>}}})),
    %% Delimiters set in a partial should not affect the parent template.
    ?_assertEqual({ok, <<"[ .yes.  .yes. ]\n[ .yes.  .|value|. ]\n">>},
                  render(<<"[ {{>include}} ]\n[ .{{value}}.  .|value|. ]\n">>,
                         #{value => <<"yes">>},
                         #{template_sources => #{<<"include">> => {string, <<".{{value}}. {{= | | =}} .|value|.">>}}})),
    %% Surrounding whitespace should be left untouched.
    ?_assertEqual({ok, <<"|  |">>},
                  render(<<"| {{=@ @=}} |">>,
                         #{},
                         #{})),
    %% Whitespace should be left untouched.
    ?_assertEqual({ok, <<" | \n">>},
                  render(<<" | {{=@ @=}}\n">>,
                         #{},
                         #{})),
    %% Standalone lines should be removed from the template.
    ?_assertEqual({ok, <<"Begin.\nEnd.\n">>},
                  render(<<"Begin.\n{{=@ @=}}\nEnd.\n">>,
                         #{},
                         #{})),
    %% Indented standalone lines should be removed from the template.
    ?_assertEqual({ok, <<"Begin.\nEnd.\n">>},
                  render(<<"Begin.\n  {{=@ @=}}\nEnd.\n">>,
                         #{},
                         #{})),
    %% "\r\n" should be considered a newline for standalone tags.
    ?_assertEqual({ok, <<"|\r\n|">>},
                  render(<<"|\r\n{{= @ @ =}}\r\n|">>,
                         #{},
                         #{})),
    %% Standalone tags should not require a newline to precede them.
    ?_assertEqual({ok, <<"=">>},
                  render(<<"  {{=@ @=}}\n=">>,
                         #{},
                         #{})),
    %% Standalone tags should not require a newline to follow them.
    ?_assertEqual({ok, <<"=\n">>},
                  render(<<"=\n  {{=@ @=}}">>,
                         #{},
                         #{})),
    %% Superfluous in-tag whitespace should be ignored.
    ?_assertEqual({ok, <<"||">>},
                  render(<<"|{{= @   @ =}}|">>,
                         #{},
                         #{}))
  ].

-spec render(binary(), mustache:context(), mustache:options()) ->
        {ok, binary()} | {error, mustache:error()}.
render(TemplateString, Context, Options0) ->
  Options = Options0#{return_binary => true},
  mustache:render(<<"test">>, {string, TemplateString}, Context, Options).
