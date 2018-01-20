-module(elixir_env).
-include("elixir.hrl").
-export([
  new/0, linify/1, with_vars/2, reset_vars/1,
  env_to_scope/1, env_to_scope_with_vars/2,
  mergea/2, mergev/2
]).

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
    vars => [],                            %% a set of defined variables
    match_vars => warn,                    %% handling of new variables
    current_vars => #{},                   %% a map with current vars
    prematch_vars => nil,                  %% a copy of variables defined before the current match
    lexical_tracker => nil}.               %% holds the lexical tracker PID}.

linify({Line, Env}) ->
  Env#{line := Line};
linify(#{} = Env) ->
  Env.

with_vars(Env, Vars) ->
  CurrentVars = maps:from_list([{Var, {0, used}} || Var <- Vars]),
  Env#{vars := Vars, current_vars := CurrentVars}.

env_to_scope(#{context := Context}) ->
  #elixir_erl{context=Context}.

env_to_scope_with_vars(Env, Vars) ->
  Map = maps:from_list(Vars),
  (env_to_scope(Env))#elixir_erl{
    vars=Map, counter=#{'_' => map_size(Map)}
  }.

reset_vars(Env) ->
  Env#{vars := [], current_vars := #{}}.

%% SCOPE MERGING

%% Receives two scopes and return a new scope based on the second
%% with their variables merged.
mergev(#{vars := V1, current_vars := CV1}, #{vars := V2, current_vars := CV2} = E2) ->
  E2#{
    vars := merge_vars(V1, V2),
    current_vars := merge_current_vars(CV1, CV2)
  }.

%% Receives two scopes and return the later scope
%% keeping the variables from the first (imports
%% and everything else are passed forward).

mergea(E1, E2) ->
  E2#{vars := ?key(E1, vars)}.

merge_vars(V1, V2) ->
  ordsets:union(V1, V2).

merge_current_vars(V, V) -> V;
merge_current_vars(V1, V2) ->
  maps:fold(fun(K, M2, Acc) ->
    V =
      case Acc of
        #{K := M1} when M1 > M2 -> M1;
        _ -> M2
      end,
    maps:put(K, V, Acc)
  end, V1, V2).