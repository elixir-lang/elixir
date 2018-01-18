-module(elixir_env).
-include("elixir.hrl").
-export([new/0, linify/1, env_to_scope/1, env_to_scope_with_vars/2, reset_vars/1]).
-export([mergea/2, mergev/2]).

new() ->
  #{'__struct__' => 'Elixir.Macro.Env',
    module => nil,                         %% the current module
    file => <<"nofile">>,                  %% the current filename
    line => 1,                             %% the current line
    function => nil,                       %% the current function
    context => nil,                        %% can be match, guard or nil
    requires => [],                        %% a set with modules required
    aliases => [],                         %% a list of aliases by new -> old names
    functions => [],                       %% a list with functions imported from module
    macros => [],                          %% a list with macros imported from module
    macro_aliases => [],                   %% keep aliases defined inside a macro
    context_modules => [],                 %% modules defined in the current context
    lexical_tracker => nil,                %% holds the lexical tracker PID
    vars => [],                            %% a set of defined variables
    prematch_vars => nil,                  %% a set of variables defined before the current match
    match_vars => warn}.                   %% handling of new variables

linify({Line, Env}) ->
  Env#{line := Line};
linify(#{} = Env) ->
  Env.

env_to_scope(#{file := File, context := Context}) ->
  #elixir_erl{file=File, context=Context}.

env_to_scope_with_vars(Env, Vars) ->
  Map = maps:from_list(Vars),
  (env_to_scope(Env))#elixir_erl{
    vars=Map, counter=#{'_' => map_size(Map)}
  }.

reset_vars(Env) ->
  Env#{vars := []}.

%% SCOPE MERGING

%% Receives two scopes and return a new scope based on the second
%% with their variables merged.
mergev(E1, E2) when is_list(E1) ->
  E2#{vars := merge_vars(E1, ?key(E2, vars))};
mergev(E1, E2) ->
  E2#{vars := merge_vars(?key(E1, vars), ?key(E2, vars))}.

%% Receives two scopes and return the later scope
%% keeping the variables from the first (imports
%% and everything else are passed forward).

mergea(E1, E2) ->
  E2#{vars := ?key(E1, vars)}.

merge_vars(V1, V2) ->
  ordsets:union(V1, V2).
