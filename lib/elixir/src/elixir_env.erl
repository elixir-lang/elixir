-module(elixir_env).
-include("elixir.hrl").
-export([ex_to_env/1, env_to_scope/1, env_to_scope_with_vars/2,
         ex_to_scope/1, scope_to_ex/1, env_to_ex/1, scope_to_env/1]).
-export([mergea/2, mergev/2]).

%% Conversion in between #elixir_env, #elixir_scope and Macro.Env

env_to_ex(#elixir_env{} = Env) ->
  erlang:setelement(1, Env, 'Elixir.Macro.Env').

ex_to_env(Env) when element(1, Env) == 'Elixir.Macro.Env' ->
  erlang:setelement(1, Env, elixir_env).

env_to_scope(#elixir_env{module=Module,file=File,
    function=Function,aliases=Aliases,context=Context,
    requires=Requires,macros=Macros,functions=Functions,
    context_modules=ContextModules,macro_aliases=MacroAliases,
    macro_counter=MacroCounter,lexical_tracker=Lexical,local=Local}) ->
  #elixir_scope{module=Module,file=File,
    function=Function,aliases=Aliases,context=Context,
    requires=Requires,macros=Macros,functions=Functions,
    context_modules=ContextModules,macro_aliases=MacroAliases,
    macro_counter=MacroCounter,lexical_tracker=Lexical,local=Local}.

env_to_scope_with_vars(#elixir_env{} = Env, Vars) ->
  (env_to_scope(Env))#elixir_scope{
    vars=orddict:from_list(Vars),
    counter=[{'',length(Vars)}]
  }.

scope_to_ex(#elixir_env{} = E) ->
  env_to_ex(E);
scope_to_ex({ Line, #elixir_scope{module=Module,file=File,
    function=Function,aliases=Aliases,context=Context,
    requires=Requires,macros=Macros,functions=Functions,
    context_modules=ContextModules,macro_aliases=MacroAliases,
    macro_counter=MacroCounter,vars=Vars,lexical_tracker=Lexical,
    local=Local} }) when is_integer(Line) ->
  { 'Elixir.Macro.Env', Module, File, Line, Function, Context, Requires, Aliases,
    Functions, Macros, MacroAliases, MacroCounter, ContextModules,
    [Pair || { Pair, _ } <- Vars], Lexical, Local }.

ex_to_scope(Env) ->
  env_to_scope(ex_to_env(Env)).

scope_to_env(#elixir_scope{} = S) ->
  ex_to_env(scope_to_ex({ 0, S })).

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
