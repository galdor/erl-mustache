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

-module(mustache_generator).

-export([generate/2, generate/3]).

-spec generate(mustache:template(), mustache:context()) ->
    {ok, iodata()} | {error, mustache:error()}.
generate(Template, Context) ->
  generate(Template, Context, #{}).

-spec generate(mustache:template(), mustache:context(), mustache:options()) ->
    {ok, iodata()} | {error, mustache:error()}.
generate(#{name := Name, full_name := FullName, ast := AST},
         Context, Options) ->
  try
    Stack = [Context],
    Data0 = generate_nodes(AST, Stack, Options),
    Data = case maps:get(return_binary, Options, false) of
             true -> iolist_to_binary(Data0);
             false -> Data0
           end,
    {ok, Data}
  catch
    throw:{error, Error} ->
      {error, Error#{template_name => Name,
                     template_full_name => FullName}}
  end.

-spec generate_nodes(mustache_parser:ast_nodes(), mustache:context_stack(),
                     mustache:options()) ->
        iodata().
generate_nodes(Nodes, Stack, Options) ->
  lists:map(fun (Node) ->
                generate_node(Node, Stack, Options)
            end, Nodes).

-spec generate_node(mustache_parser:ast_node(), mustache:context_stack(),
                    mustache:options()) -> iodata().

generate_node(#{type := text, value := Text}, _Stack, _Options) ->
  Text;

generate_node(Node = #{type := variable, value := Name}, Stack, Options) ->
  case mustache_context:find(Name, Stack) of
    {ok, Value} ->
      Data = stringify(Value),
      case maps:get(disable_html_escaping, Options, false) of
        true ->
          Data;
        false ->
          mustache_html:escape(iolist_to_binary(Data))
      end;
    error ->
      handle_unknown_variable(Node, Options)
  end;

generate_node(Node = #{type := unescaped_variable, value := Name},
              Stack, Options) ->
  case mustache_context:find(Name, Stack) of
    {ok, Value} ->
      stringify(Value);
    error ->
      handle_unknown_variable(Node, Options)
  end;

generate_node(Node = #{type := section, value := Name, children := Children},
              Stack, Options) ->
  case mustache_context:find(Name, Stack) of
    {ok, Value} ->
      List = case Value of
               false -> [];
               _ when is_list(Value) -> Value;
               _ -> [Value]
             end,
      F = fun (V, Data) ->
              Datum = generate_nodes(Children, [V | Stack], Options),
              [Datum | Data]
          end,
      ChildrenData = lists:foldl(F, [], List),
      lists:reverse(ChildrenData);
    error ->
      handle_unknown_variable(Node, Options)
  end;

generate_node(#{type := inverted_section, value := Name, children := Children},
              Stack, Options) ->
  case mustache_context:find(Name, Stack) of
    {ok, Value} ->
      List = case Value of
               false -> [];
               <<>> -> [];
               #{} when map_size(Value) =:= 0 -> [];
               _ when is_list(Value) -> Value;
               _ -> [Value]
             end,
      case List of
        [] ->
          generate_nodes(Children, Stack, Options);
        _ ->
          []
      end;
    error ->
      generate_nodes(Children, Stack, Options)
  end;

generate_node(#{type := comment}, _Stack, _Options) ->
  [];

generate_node(Node = #{type := partial, value := Name,
                       position := Position}, Stack, Options) ->
  {_, Column} = Position,
  Indent = case maps:get(standalone, Node, false) of
             true -> Column - 1;
             false -> 0
           end,
  Options2 = Options#{indent => Indent},
  case mustache:load_template(Name, Options2) of
    {ok, #{name := PartialName, full_name := PartialFullName,
           ast := PartialAST}} ->
      try
        generate_nodes(PartialAST, Stack, Options2)
      catch
        throw:{error, Error} ->
          Error2 = Error#{template_name => PartialName,
                          template_full_name => PartialFullName},
          Error3 = mustache_errors:unindent_error_position(Error2, Indent),
          throw({error, #{tag_name => Name,
                          position => Position,
                          reason => invalid_partial,
                          partial_error => Error3}})
      end;
    {error, Error = #{reason := template_not_found}} ->
      case maps:get(error_on_unknown_partial, Options, false) of
        true->
          %% When a template was not found, the error has no position, so we
          %% do not try to unindent.
          throw({error, #{tag_name => Name,
                          position => Position,
                          reason => unknown_partial,
                          partial_error => Error}});
        false ->
          []
      end;
    {error, Error} ->
      case maps:get(error_on_invalid_partial, Options, false) of
        true->
          Error2 = mustache_errors:unindent_error_position(Error, Indent),
          throw({error, #{tag_name => Name,
                          position => Position,
                          reason => invalid_partial,
                          partial_error => Error2}});
        false ->
          []
      end
  end.

-spec handle_unknown_variable(mustache_parser:ast_node(), mustache:options()) ->
        iodata().
handle_unknown_variable(#{value := Name, position := Position}, Options) ->
  case maps:get(error_on_unknown_variable, Options, false) of
    true->
      throw({error, #{position => Position,
                      reason => unknown_variable,
                      tag_name => Name}});
    false ->
      []
  end.

-spec stringify(mustache:context_value()) -> iodata().
stringify(Value) when is_binary(Value) ->
  Value;
stringify(Value) ->
  io_lib:format(<<"~0tp">>, [Value]).
