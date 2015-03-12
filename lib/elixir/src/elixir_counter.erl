-module(elixir_counter).
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3, next/0]).
-behaviour(gen_server).

-define(timeout, 30000).    %% 30 seconds
-define(limit, 4294967295). %% 2^32 - 1

next() ->
  gen_server:call(?MODULE, next, ?timeout).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, 0, []).

init(Counter) ->
  {ok, Counter}.

handle_call(next, _From, Counter) ->
  {reply, Counter, bump(Counter)};
handle_call(Request, _From, Counter) ->
  {stop, {badcall, Request}, Counter}.

handle_cast(Request, Counter) ->
  {stop, {badcast, Request}, Counter}.

handle_info(_Request, Counter) ->
  {noreply, Counter}.

terminate(_Reason, _Counter) ->
  ok.

code_change(_Old, Counter, _Extra) ->
  {ok, Counter}.

bump(Counter) when Counter < ?limit ->
  Counter + 1;
bump(_Counter) ->
  0.
