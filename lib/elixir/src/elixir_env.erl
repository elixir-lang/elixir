-module(elixir_env).
-include("elixir.hrl").
-export([ex_to_env/1, env_to_scope/1, env_to_scope_with_vars/2, env_to_ex/1]).
-export([mergea/2, mergev/2, merge_vars/2, merge_opt_vars/2]).

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
mergev(E1, E2) when is_list(E1) ->
  E2#elixir_env{
    vars=merge_vars(E1, E2#elixir_env.vars),
    export_vars=merge_opt_vars(E1, E2#elixir_env.export_vars)
  };
mergev(E1, E2) ->
  E2#elixir_env{
    vars=merge_vars(E1#elixir_env.vars, E2#elixir_env.vars),
    export_vars=merge_opt_vars(E1#elixir_env.export_vars, E2#elixir_env.export_vars)
  }.

%% Receives two scopes and return the later scope
%% keeping the variables from the first (imports
%% and everything else are passed forward).

mergea(E1, E2) ->
  E2#elixir_env{vars=E1#elixir_env.vars}.

merge_vars(V1, V2) -> ordsets:union(V1, V2).

merge_opt_vars(_V1, nil) -> nil;
merge_opt_vars(nil, _V2) -> nil;
merge_opt_vars(V1, V2)   -> ordsets:union(V1, V2).
