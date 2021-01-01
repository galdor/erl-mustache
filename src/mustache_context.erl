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

-module(mustache_context).

-export([find/2]).

-spec find(Path :: binary(), mustache:context_stack()) ->
  {ok, mustache:context_value()} | error.
find(Path, Stack) ->
  Keys = case Path of
           <<".">> -> [<<".">>];
           _ -> binary:split(Path, <<".">>, [global])
         end,
  find_by_keys(Keys, Stack).

-spec find_by_keys([mustache:context_key()], mustache:context_stack()) ->
        {ok, mustache:context_value()} | error.
find_by_keys([], _Stack) ->
  error;
find_by_keys([<<".">>], [Context | _Stack]) ->
  {ok, Context};
find_by_keys([Key | Keys], Stack) ->
  case find_by_key(Key, Stack) of
    {ok, Value} ->
      case Keys of
        [] ->
          {ok, Value};
        _ ->
          find_by_keys(Keys, [Value])
      end;
    error ->
      error
  end.

-spec find_by_key(mustache:context_key(), mustache:context_stack()) ->
  {ok, mustache:context_value()} | error.
find_by_key(_Key, []) ->
  error;
find_by_key(Key, [Context | Stack]) when is_map(Context) ->
  case lookup(Key, Context) of
    {ok, Value} ->
      {ok, Value};
    error ->
      find(Key, Stack)
  end;
find_by_key(Key, [_Context | Stack]) ->
  find_by_key(Key, Stack).

-spec lookup(mustache:context_key(), mustache:context()) ->
        {ok, mustache:context_value()} | error.
lookup(Key, Context) ->
  case maps:find(Key, Context) of
    {ok, Value} ->
      {ok, Value};
    error ->
      case maps:find(binary_to_atom(Key), Context) of
        {ok, Value} ->
          {ok, Value};
        error ->
          case maps:find(unicode:characters_to_list(Key), Context) of
            {ok, Value} ->
              {ok, Value};
            error ->
              error
          end
      end
  end.
