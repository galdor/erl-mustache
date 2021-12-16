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

-module(mustache_cache).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/1, start_link/2, stop/1,
         get_template/3, get_template/4]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([options/0, name/0, ref/0]).

-type options() :: #{}.

-type name() :: {local, term()} | {global, term()} | {via, atom(), term()}.
-type ref() :: term() | {term(), atom()} | {global, term()}
             | {via, atom(), term()} | pid().

-type state() :: #{options := options(),
                   table := ets:tid()}.

-spec start_link(name() | options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Options) when is_map(Options) ->
  gen_server:start_link(?MODULE, [Options], []);
start_link(Name) ->
  start_link(Name, #{}).

-spec start_link(name(), options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Name, Options) ->
  gen_server:start_link(Name, ?MODULE, [Options], []).

-spec stop(ref()) -> ok.
stop(Ref) ->
  gen_server:stop(Ref).

init([Options]) ->
  logger:update_process_metadata(#{domain => [mustache, cache]}),
  Table = ets:new(mustache_cache, [set]),
  State = #{options => Options,
            table => Table},
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

-spec get_template(ref(), mustache:template_name(), mustache:options()) ->
        {ok, mustache:template()} | {error, mustache:error()}.
get_template(Ref, Name, Options) ->
  gen_server:call(Ref, {get_template, Name, Options}, infinity).

-spec get_template(ref(), mustache:template_name(), mustache:template_source(),
                   mustache:options()) ->
        {ok, mustache:template()} | {error, mustache:error()}.
get_template(Ref, Name, Source, Options) ->
  gen_server:call(Ref, {get_template, Name, Source, Options}, infinity).

handle_call({get_template, Name, Options}, _From, State) ->
  {reply, do_get_template(State, Name, default, Options), State};

handle_call({get_template, Name, Source, Options}, _From, State) ->
  {reply, do_get_template(State, Name, Source, Options), State};

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p~n", [Msg, From]),
  {noreply, State}.

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p~n", [Msg]),
  {noreply, State}.

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p~n", [Msg]),
  {noreply, State}.

-spec do_get_template(state(), mustache:template_name(),
                      mustache:template_source(), mustache:options()) ->
        {ok, mustache:template()} | {error, mustache:error()}.
do_get_template(#{table := Table}, Name, Source, Options) ->
  case ets:lookup(Table, Name) of
    [{_, Template}] ->
      {ok, Template};
    [] ->
      Options2 = Options#{enable_cache => false}, % avoid recursive lookups
      case mustache:load_template(Name, Source, Options2) of
        {ok, Template} ->
          ets:insert(Table, {Name, Template}),
          {ok, Template};
        {error, Error} ->
          {error, Error}
      end
  end.
