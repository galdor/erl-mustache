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

-module(mustache).

-export([default_delimiters/0,
         load_template/2, load_template/3,
         render_template/2, render_template/3,
         render/2, render/3, render/4]).

-export_type([template/0, template_name/0, template_full_name/0,
              template_source/0,
              context_stack/0, context/0, context_key/0, context_value/0,
              options/0,
              error/0, error_reason/0, invalid_delimiter_error_reason/0,
              delimiters/0,
              position/0]).

-type template() :: #{name := template_name(),
                      full_name := template_full_name(),
                      ast := mustache_parser:ast()}.

-type template_name() :: binary().
-type template_full_name() :: binary().

-type template_source() :: default
                         | {file, file:name_all()}
                         | {string, unicode:chardata()}.

-type context_stack() :: [context()].
-type context() :: #{context_key() := context_value()}.
-type context_key() :: binary() | string() | atom().
-type context_value() :: context()
                       | term()
                       | {data, iodata()}.

-type options() :: #{template_directory => file:name_all(),
                     template_sources =>
                       #{template_name() := template_source()},
                     disable_html_escaping => boolean(),
                     error_on_unknown_variable => boolean(),
                     error_on_unknown_partial => boolean(),
                     error_on_invalid_partial => boolean(),
                     return_binary => boolean(),
                     indent => non_neg_integer(),
                     enable_cache => boolean(),
                     cache_ref => mustache_cache:ref()}.

-type error() :: #{template_name => template_name(),
                   template_full_name => template_full_name(),
                   position => position(),
                   tag_name => binary(),
                   reason := error_reason(),
                   partial_error => error()}.

-type error_reason() :: template_not_found
                      | {template_loading_error, Reason :: term()}
                      | {invalid_delimiter, invalid_delimiter_error_reason()}
                      | {truncated_section, Name :: binary()}
                      | {section_name_mismatch,
                         Name :: binary(), ExpectedName :: binary()}
                      | unknown_variable
                      | unknown_partial
                      | invalid_partial.

-type invalid_delimiter_error_reason() :: invalid_format
                                        | invalid_character.

-type delimiters() :: {Start :: binary(), End :: binary()}.

-type position() :: {Line :: pos_integer(), Column :: pos_integer()}.

-spec default_delimiters() -> delimiters().
default_delimiters() ->
  {<<"{{">>, <<"}}">>}.

-spec load_template(template_name(), options()) ->
        {ok, template()} | {error, error()}.
load_template(Name, Options) ->
  load_template(Name, default, Options).

-spec load_template(template_name(), template_source(), options()) ->
        {ok, template()} | {error, error()}.
load_template(Name, Source, Options) ->
  case maps:get(enable_cache, Options, false) of
    true ->
      Ref = maps:get(cache_ref, Options, mustache_cache),
      mustache_cache:get_template(Ref, Name, Source, Options);
    false->
      do_load_template(Name, Source, Options)
  end.

-spec do_load_template(template_name(), template_source(), options()) ->
        {ok, template()} | {error, error()}.
do_load_template(Name, default, Options) ->
  Sources = maps:get(template_sources, Options, #{}),
  case maps:find(Name, Sources) of
    {ok, default} ->
      %% Avoid recursive calls
      error({invalid_template_source, Name, default});
    {ok, Source} ->
      do_load_template(Name, Source, Options);
    error ->
      Path = <<Name/binary, ".mustache">>,
      do_load_template(Name, {file, Path}, Options)
  end;
do_load_template(Name, {file, Path0}, Options) ->
  Path = case filename:pathtype(Path0) of
           absolute ->
             Path0;
           _ ->
             Directory = maps:get(template_directory, Options, <<".">>),
             filename:join(Directory, Path0)
         end,
  case file:read_file(Path) of
    {ok, Data} ->
      case do_load_template(Name, {string, Data}, Options) of
        {ok, Template} ->
          {ok, Template#{full_name => Path}};
        {error, Error} ->
          {error, Error#{template_full_name => Path}}
      end;
    {error, enoent} ->
      {error, #{template_name => Name,
                template_full_name => Path,
                reason => template_not_found}};
    {error, Reason} ->
      {error, #{template_name => Name,
                template_full_name => Path,
                reason => {template_loading_error, Reason}}}
  end;
do_load_template(Name, {string, String0}, Options) ->
  FullName = Name,
  case unicode:characters_to_binary(String0) of
    String when is_binary(String) ->
      String2 = indent(String, maps:get(indent, Options, 0)),
      case mustache_parser:parse(String2) of
        {ok, AST} ->
          Template = #{name => Name, full_name => FullName, ast => AST},
          {ok, Template};
        {error, Error} ->
          {error, Error#{template_name => Name,
                         template_full_name => FullName}}
      end;
    {Type, _, Data} when Type =:= error; Type =:= incomplete ->
      {error, #{template_name => Name,
                template_full_name => FullName,
                reason => {template_loading_error, {invalid_string, Data}}}}
  end.

-spec render_template(template(), context()) ->
        {ok, iodata()} | {error, error()}.
render_template(Template, Context) ->
  render_template(Template, Context, #{}).

-spec render_template(template(), context(), options()) ->
        {ok, iodata()} | {error, error()}.
render_template(Template, Context, Options) ->
  mustache_generator:generate(Template, Context, Options).

-spec render(template_name(), context()) ->
        {ok, iodata()} | {error, error()}.
render(Name, Context) ->
  render(Name, Context, #{}).

-spec render(template_name(),
             template_source() | context(),
             context() | options()) ->
        {ok, iodata()} | {error, error()}.
render(Name, Source, Context) when is_tuple(Source) ->
  render(Name, Source, Context, #{});
render(Name, Context, Options) when is_map(Context) ->
  render(Name, default, Context, Options).

-spec render(template_name(), template_source(), context(), options()) ->
        {ok, iodata()} | {error, error()}.
render(Name, Source, Context, Options) ->
  case load_template(Name, Source, Options) of
    {ok, Template} ->
      render_template(Template, Context, Options);
    {error, Error} ->
      {error, Error}
  end.

-spec indent(iodata(), non_neg_integer()) -> binary().
indent(Data, 0) ->
  Data;
indent(Data, N) ->
  Padding = io_lib:format(<<"~*s">>, [N, ""]),
  re:replace(Data, "^", Padding, [multiline, global, {return, binary}]).
