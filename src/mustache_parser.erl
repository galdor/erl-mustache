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

-module(mustache_parser).

-export([parse/1]).

-export_type([ast/0, ast_node/0, ast_nodes/0, ast_node_type/0]).

-type ast() :: ast_nodes().

-type ast_node() :: #{type := ast_node_type(),
                      value := binary(),
                      position := mustache:position(),
                      children => ast_nodes(),
                      standalone => boolean()}.
-type ast_nodes() :: [ast_node()].

-type ast_node_type() :: text
                       | variable
                       | unescaped_variable
                       | section
                       | inverted_section
                       | comment
                       | partial.

-spec parse(binary()) -> {ok, ast()} | {error, mustache:error()}.
parse(Data) ->
  case mustache_lexer:read(Data) of
    {ok, Tokens} ->
      case parse_tokens(Tokens, undefined, []) of
        {ok, Nodes, []} ->
          {ok, Nodes};
        {error, Error} ->
          {error, Error}
      end;
    {error, Error} ->
      {error, Error}
  end.

-spec parse_tokens(Tokens, Section, ast_nodes()) ->
        {ok, ast_nodes(), Tokens} | {error, mustache:error()} when
    Section :: ast_node() | undefined,
    Tokens :: [mustache_lexer:token()].

parse_tokens([], undefined, Nodes) ->
  {ok, lists:reverse(Nodes), []};

parse_tokens([], #{value := Name, position := Position}, _Nodes) ->
  {error, #{position => Position,
            reason => {truncated_section, Name}}};

parse_tokens([{delimiters, _Value, _Position, _Flags} | Tokens],
             Section, Nodes) ->
  parse_tokens(Tokens, Section, Nodes);

parse_tokens([Token = {Type, Value, Position, _Flags} | Tokens],
             Section, Nodes) when
    Type =:= text;
    Type =:= variable;
    Type =:= unescaped_variable;
    Type =:= comment;
    Type =:= partial ->
  Node = #{type => Type, value => Value, position => Position,
           standalone => mustache_lexer:is_token_standalone(Token)},
  parse_tokens(Tokens, Section, [Node | Nodes]);

parse_tokens([Token = {section_start, Name, Position, _Flags} | Tokens],
             Section, Nodes) ->
  Node0 = #{type => section, value => Name, position => Position,
            standalone => mustache_lexer:is_token_standalone(Token)},
  case parse_tokens(Tokens, Node0, []) of
    {ok, Children, Tokens2} ->
      Node = Node0#{children => Children},
      parse_tokens(Tokens2, Section, [Node | Nodes]);
    {error, Error} ->
      {error, Error}
  end;

parse_tokens([Token = {inverted_section_start, Name, Position, _Flags} |
              Tokens],
             Section, Nodes) ->
  Node0 = #{type => inverted_section, value => Name, position => Position,
            standalone => mustache_lexer:is_token_standalone(Token)},
  case parse_tokens(Tokens, Node0, []) of
    {ok, Children, Tokens2} ->
      Node = Node0#{children => Children},
      parse_tokens(Tokens2, Section, [Node | Nodes]);
    {error, Error} ->
      {error, Error}
  end;

parse_tokens([{section_end, Name, _, _Flags} | Tokens],
             #{value := Name}, Nodes) ->
  {ok, lists:reverse(Nodes), Tokens};

parse_tokens([{section_end, EndName, EndPosition, _Flags} | _Tokens],
             #{value := Name}, _Nodes) ->
  {error, #{position => EndPosition,
            reason => {section_name_mismatch, EndName, Name}}}.
