-module(elixir_env).
-include("elixir.hrl").
-export([new/0, linify/1, env_to_scope/1, env_to_scope_with_vars/2]).
-export([mergea/2, mergev/2, merge_vars/2, merge_opt_vars/2]).

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
    lexical_tracker => nil,                %% holds the lexical tracker PID
    export_vars => nil}.                   %% a set of variables to be exported in some constructs

linify({Line, Env}) ->
  Env#{line := Line}.

env_to_scope(#{file := File, context := Context}) ->
  #elixir_erl{file=File, context=Context}.

env_to_scope_with_vars(Env, Vars) ->
  Map = maps:from_list(Vars),
  (env_to_scope(Env))#elixir_erl{
    vars=Map, counter=#{'_' => map_size(Map)}
  }.

%% SCOPE MERGING

%% Receives two scopes and return a new scope based on the second
%% with their variables merged.
mergev(E1, E2) when is_list(E1) ->
  E2#{
    vars := merge_vars(E1, ?m(E2, vars)),
    export_vars := merge_opt_vars(E1, ?m(E2, export_vars))
  };
mergev(E1, E2) ->
  E2#{
    vars := merge_vars(?m(E1, vars), ?m(E2, vars)),
    export_vars := merge_opt_vars(?m(E1, export_vars), ?m(E2, export_vars))
  }.

%% Receives two scopes and return the later scope
%% keeping the variables from the first (imports
%% and everything else are passed forward).

mergea(E1, E2) ->
  E2#{vars := ?m(E1, vars)}.

merge_vars(V1, V2) -> ordsets:union(V1, V2).

merge_opt_vars(_V1, nil) -> nil;
merge_opt_vars(nil, _V2) -> nil;
merge_opt_vars(V1, V2)   -> ordsets:union(V1, V2).
