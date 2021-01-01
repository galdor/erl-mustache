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

-module(mustache_lexer).

-export([read/1, is_token_standalone/1]).

-export_type([token/0, token_type/0]).

-type token() :: {token_type(), Value :: binary(), mustache:position(),
                  [token_flag()]}.
-type token_flag() :: standalone.

-type token_type() :: text
                    | variable
                    | unescaped_variable
                    | section_start
                    | inverted_section_start
                    | section_end
                    | comment
                    | partial
                    | delimiters.

-type lexer() :: #{data := binary(),
                   position := mustache:position(),
                   delimiters := mustache:delimiters(),
                   blank_line := boolean()}.

-spec read(binary()) -> {ok, [token()]} | {error, mustache:error()}.
read(Data) ->
  try
    Lexer = #{data => Data,
              position => {1, 1},
              delimiters => mustache:default_delimiters(),
              blank_line => true},
    {ok, read_tokens(Lexer)}
  catch
    throw:{error, Error} ->
      {error, Error}
  end.

-spec read_tokens(lexer()) -> [token()].
read_tokens(Lexer) ->
  read_tokens(Lexer, []).

-spec read_tokens(lexer(), [token()]) -> [token()].
read_tokens(Lexer, Tokens) ->
  case read_token(Lexer) of
    {Lexer2, Token} ->
      case is_token_standalone(Token) of
        true ->
          Tokens2 = case Tokens of
                      [] ->
                        [Token];
                      [{text, Text, Position, Flags} | Rest] ->
                        Text2 = string:trim(Text, trailing, " \t"),
                        [Token | [{text, Text2, Position, Flags} | Rest]];
                      _ ->
                        [Token | Tokens]
                    end,
          read_tokens(skip_until_eol(Lexer2), Tokens2);
        false ->
          read_tokens(Lexer2, [Token | Tokens])
      end;
    eof ->
      lists:reverse(Tokens)
  end.

-spec read_token(lexer()) -> {lexer(), token()} | eof.
read_token(#{data := <<>>}) ->
  eof;
read_token(Lexer = #{data := Data,
                     delimiters := {Start, _}}) ->
  case string:prefix(Data, Start) of
    nomatch ->
      read_text(Lexer);
    _ ->
      read_tag(Lexer)
  end.

-spec read_text(lexer()) -> {lexer(), token()}.
read_text(Lexer = #{data := Data,
                    delimiters := {Start, _},
                    position := Position}) ->
  Text = case binary:split(Data, Start) of
           [T, _] -> T;
           [T] -> T
         end,
  Token = {text, Text, Position, []},
  {skip(Lexer, byte_size(Text)), Token}.

-spec read_tag(lexer()) -> {lexer(), token() | token()}.
read_tag(Lexer = #{delimiters := {Start, End},
                   position := TagStart,
                   blank_line := BlankLine}) ->
  Lexer2 = skip(Lexer, byte_size(Start)),
  {Lexer3, TagType} = read_tag_type(Lexer2),
  TagEnd = tag_end(TagType, End),
  #{data := Data3} = Lexer3,
  %% We do not search for the end of the tag beyond the current line if it is
  %% not a comment.
  Scope = case TagType of
            $! ->
              {0, byte_size(Data3)};
            _ ->
              case binary:match(Data3, [<<"\n">>, <<"\r\n">>]) of
                {EOL, _} ->
                  {0, EOL};
                nomatch ->
                  {0, byte_size(Data3)}
              end
          end,
  {Name, Lexer4} = case binary:split(Data3, TagEnd, [{scope, Scope}]) of
                     [<<>>, _] ->
                       throw({error, #{position => TagStart,
                                       reason => empty_tag}});
                     [Value, _] ->
                       N = byte_size(Value) + byte_size(TagEnd),
                       {string:trim(Value, both, " \t"), skip(Lexer3, N)};
                     [_] ->
                       throw({error, #{position => TagStart,
                                       reason => truncated_tag}})
                   end,
  #{data := Data4} = Lexer4,
  case TagType of
    $= ->
      case parse_delimiters(Name) of
        {ok, Delimiters} ->
          Token = {delimiters, Name, TagStart, []},
          {Lexer4#{delimiters => Delimiters},
           maybe_standalone(BlankLine, Data4, Token)};
        {error, Reason} ->
          throw({error, #{position => TagStart,
                          reason => {invalid_delimiters, Reason}}})
      end;
    _ ->
      TokenType = tag_token_type(TagType),
      Token = {TokenType, Name, TagStart, []},
      {Lexer4, maybe_standalone(BlankLine, Data4, Token)}
  end.

-spec maybe_standalone(BlankLine :: boolean(), Data :: binary(), token()) ->
        token().
maybe_standalone(false, _, Token) ->
  Token;
maybe_standalone(_, _, Token = {variable, _, _, _}) ->
  Token;
maybe_standalone(_, _, Token = {unescaped_variable, _, _, _}) ->
  Token;
maybe_standalone(_, Data, Token = {Type, Value, Position, Flags}) ->
  case is_blank_line(Data) of
    true ->
      {Type, Value, Position, [standalone | Flags]};
    false ->
      Token
  end.

-spec read_tag_type(lexer()) -> {lexer(), byte()}.
read_tag_type(Lexer = #{data := <<C, _/binary>>}) when
    C =:= $=; C =:= ${; C =:= $&; C =:= $#;
    C =:= $^; C =:= $/; C =:= $!; C =:= $> ->
  {skip_whitespace(skip(Lexer, 1)), C};
read_tag_type(Lexer) ->
  {Lexer, 0}.

-spec tag_token_type(TagType :: byte()) -> token_type().
tag_token_type(0) -> variable;
tag_token_type(${) -> unescaped_variable;
tag_token_type($&) -> unescaped_variable;
tag_token_type($#) -> section_start;
tag_token_type($^) -> inverted_section_start;
tag_token_type($/) -> section_end;
tag_token_type($!) -> comment;
tag_token_type($>) -> partial.

-spec tag_end(TagType :: byte(), End :: binary()) -> binary().
tag_end($=, End) -> <<$=, End/binary>>;
tag_end(${, End) -> <<$}, End/binary>>;
tag_end(_, End) -> End.

-spec skip_whitespace(lexer()) -> lexer().
skip_whitespace(Lexer = #{data := <<C, _/binary>>}) when
    C =:= $\s; C =:= $\t; C =:= $\n ->
  skip1(Lexer);
skip_whitespace(Lexer) ->
  Lexer.

-spec skip_until_eol(lexer()) -> lexer().
skip_until_eol(Lexer = #{data := <<$\r, $\n, _/binary>>}) ->
  skip(Lexer, 2);
skip_until_eol(Lexer = #{data := <<$\n, _/binary>>}) ->
  skip1(Lexer);
skip_until_eol(Lexer = #{data := <<_, Data/binary>>}) ->
  skip_until_eol(Lexer#{data => Data});
skip_until_eol(Lexer = #{data := <<>>}) ->
  Lexer.

-spec skip(lexer(), non_neg_integer()) -> lexer().
skip(Lexer, 0) ->
  Lexer;
skip(Lexer, N) ->
  skip(skip1(Lexer), N-1).

-spec skip1(lexer()) -> lexer().
skip1(Lexer = #{data := <<$\n, Data/binary>>, position := {Line, _}}) ->
  Lexer#{data => Data, position => {Line+1, 1}, blank_line => true};
skip1(Lexer = #{data := <<C, Data/binary>>, position := {Line, Column},
                blank_line := BlankLine}) ->
  Lexer#{data => Data, position => {Line, Column+1},
         blank_line => BlankLine and ((C =:= $\s) or (C =:= $\t))}.

-spec parse_delimiters(binary()) ->
        {ok, mustache:delimiters()} |
        {error, mustache:invalid_delimiter_error_reason()}.
parse_delimiters(Data) ->
  case binary:split(Data, [<<" ">>, <<"\t">>]) of
    [Start0, End0] ->
      Start = string:trim(Start0, both, " \t"),
      End = string:trim(End0, both, " \t"),
      case validate_delimiter(Start) of
        ok ->
          case validate_delimiter(End) of
            ok ->
              {ok, {Start, End}};
            {error, Reason} ->
              {error, Reason}
          end;
        {error, Reason} ->
          {error, Reason}
      end;
    [_] ->
      {error, invalid_format}
  end.

-spec validate_delimiter(binary()) ->
        ok | {error, mustache:invalid_delimiter_error_reason()}.
validate_delimiter(Value) ->
  case binary:match(Value, <<"=">>) of
    nomatch ->
      ok;
    _ ->
      {error, invalid_character}
  end.

-spec is_blank_line(binary()) -> boolean().
is_blank_line(<<>>) ->
  true;
is_blank_line(<<$\n, _/binary>>) ->
  true;
is_blank_line(<<$\r, $\n, _/binary>>) ->
  true;
is_blank_line(<<C, Data/binary>>) when C =:= $\s; C =:= $\t ->
  is_blank_line(Data);
is_blank_line(<<_, _/binary>>) ->
  false.

-spec is_token_standalone(token()) -> boolean().
is_token_standalone({_, _, _, Flags}) ->
  lists:member(standalone, Flags).
