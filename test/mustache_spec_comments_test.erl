%% Copyright (c) 2020-2021 Nicolas Martyanoff <khaelin@gmail.com>.
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

-module(mustache_spec_comments_test).

-include_lib("eunit/include/eunit.hrl").

comments_spec_test_() ->
   [
    %% Comment blocks should be removed from the template.
    ?_assertEqual({ok, <<"1234567890">>},
                  render(<<"12345{{! Comment Block! }}67890">>,
                         #{},
                         #{})),
    %% Multiline comments should be permitted.
    ?_assertEqual({ok, <<"1234567890\n">>},
                  render(<<"12345{{!\n  This is a\n  multi-line comment...\n}}67890\n">>,
                         #{},
                         #{})),
    %% All standalone comment lines should be removed.
    ?_assertEqual({ok, <<"Begin.\nEnd.\n">>},
                  render(<<"Begin.\n{{! Comment Block! }}\nEnd.\n">>,
                         #{},
                         #{})),
    %% All standalone comment lines should be removed.
    ?_assertEqual({ok, <<"Begin.\nEnd.\n">>},
                  render(<<"Begin.\n  {{! Indented Comment Block! }}\nEnd.\n">>,
                         #{},
                         #{})),
    %% "\r\n" should be considered a newline for standalone tags.
    ?_assertEqual({ok, <<"|\r\n|">>},
                  render(<<"|\r\n{{! Standalone Comment }}\r\n|">>,
                         #{},
                         #{})),
    %% Standalone tags should not require a newline to precede them.
    ?_assertEqual({ok, <<"!">>},
                  render(<<"  {{! I'm Still Standalone }}\n!">>,
                         #{},
                         #{})),
    %% Standalone tags should not require a newline to follow them.
    ?_assertEqual({ok, <<"!\n">>},
                  render(<<"!\n  {{! I'm Still Standalone }}">>,
                         #{},
                         #{})),
    %% All standalone comment lines should be removed.
    ?_assertEqual({ok, <<"Begin.\nEnd.\n">>},
                  render(<<"Begin.\n{{!\nSomething's going on here...\n}}\nEnd.\n">>,
                         #{},
                         #{})),
    %% All standalone comment lines should be removed.
    ?_assertEqual({ok, <<"Begin.\nEnd.\n">>},
                  render(<<"Begin.\n  {{!\n    Something's going on here...\n  }}\nEnd.\n">>,
                         #{},
                         #{})),
    %% Inline comments should not strip whitespace
    ?_assertEqual({ok, <<"  12 \n">>},
                  render(<<"  12 {{! 34 }}\n">>,
                         #{},
                         #{})),
    %% Comment removal should preserve surrounding whitespace.
    ?_assertEqual({ok, <<"12345  67890">>},
                  render(<<"12345 {{! Comment Block! }} 67890">>,
                         #{},
                         #{}))
  ].

-spec render(binary(), mustache:context(), mustache:options()) ->
        {ok, binary()} | {error, mustache:error()}.
render(TemplateString, Context, Options0) ->
  Options = Options0#{return_binary => true},
  mustache:render(<<"test">>, {string, TemplateString}, Context, Options).
