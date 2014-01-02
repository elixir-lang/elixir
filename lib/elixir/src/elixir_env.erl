-module(elixir_env).
-include("elixir.hrl").
-export([ex_to_env/1, env_to_scope/1, env_to_scope_with_vars/2, env_to_ex/1]).
-export([cache/1, get_cached/1]).
-export([mergea/2, mergev/2]).
-define(tracker, 'Elixir.Kernel.LexicalTracker').

%% Conversion in between #elixir_env, #elixir_scope and Macro.Env

env_to_ex({ Line, #elixir_env{} = Env }) ->
  erlang:setelement(1, Env#elixir_env{line=Line}, 'Elixir.Macro.Env').

ex_to_env(Env) when element(1, Env) == 'Elixir.Macro.Env' ->
  erlang:setelement(1, Env, elixir_env).

env_to_scope(#elixir_env{module=Module,file=File,function=Function,context=Context}) ->
  #elixir_scope{module=Module,file=File,function=Function,context=Context}.

env_to_scope_with_vars(#elixir_env{} = Env, Vars) ->
  (env_to_scope(Env))#elixir_scope{
    vars=orddict:from_list(Vars),
    counter=[{'_',length(Vars)}]
  }.

%% SCOPE MERGING

%% Receives two scopes and return a new scope based on the second
%% with their variables merged.

mergev(E1, E2) ->
  E2#elixir_env{
    vars=ordsets:union(E1#elixir_env.vars, E2#elixir_env.vars)
  }.

%% Receives two scopes and return the later scope
%% keeping the variables from the first (counters,
%% imports and everything else are passed forward).

mergea(E1, E2) ->
  E2#elixir_env{vars=E1#elixir_env.vars}.

%% Caching

cache(#elixir_env{} = RE) ->
  E = RE#elixir_env{line=nil,vars=[]},
  case E#elixir_env.lexical_tracker of
    nil -> escape(E);
    Pid -> { Pid, ?tracker:cache(Pid, E) }
  end;
cache(ExEnv) ->
  cache(ex_to_env(ExEnv)).

get_cached({Pid,Ref}) -> ?tracker:get_cached(Pid, Ref);
get_cached(Env) -> Env.

escape(E) ->
  { Escaped, _ } = elixir_quote:escape(E, false), Escaped.
