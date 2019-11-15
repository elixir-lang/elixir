-module(elixir_env).
-include("elixir.hrl").
-export([
  new/0, linify/1, with_vars/2, reset_vars/1,
  env_to_scope/1, env_to_scope_with_vars/2,
  reset_unused_vars/1, check_unused_vars/1,
  merge_and_check_unused_vars/2,
  trace/2, format_error/1,
  reset_read/2, prepare_write/1, close_write/2
]).

new() ->
  #{
    '__struct__' => 'Elixir.Macro.Env',
    module => nil,                                    %% the current module
    file => <<"nofile">>,                             %% the current filename
    line => 1,                                        %% the current line
    function => nil,                                  %% the current function
    context => nil,                                   %% can be match, guard or nil
    aliases => [],                                    %% a list of aliases by new -> old names
    requires => elixir_dispatch:default_requires(),   %% a set with modules required
    functions => elixir_dispatch:default_functions(), %% a list with functions imported from module
    macros => elixir_dispatch:default_macros(),       %% a list with macros imported from module
    macro_aliases => [],                              %% keep aliases defined inside a macro
    context_modules => [],                            %% modules defined in the current context
    vars => [],                                       %% a set of defined variables
    current_vars => {#{}, false},                     %% a tuple with maps of read and optional write current vars
    unused_vars => {#{}, 0},                          %% a map of unused vars and a version counter for vars
    prematch_vars => warn,                            %% controls behaviour outside and inside matches
    lexical_tracker => nil,                           %% lexical tracker PID
    contextual_vars => [],                            %% available contextual variables
    tracers => []                                     %% available compilation tracers
  }.

trace(Event, #{tracers := Tracers} = E) ->
  [ok = Tracer:trace(Event, E) || Tracer <- Tracers],
  ok.

linify({Line, Env}) ->
  Env#{line := Line};
linify(#{} = Env) ->
  Env.

with_vars(Env, Vars) ->
  NumVars = length(Vars),
  VarVersions = lists:zip(Vars, lists:seq(0, NumVars - 1)),
  Read = maps:from_list(VarVersions),
  Env#{vars := Vars, current_vars := {Read, false}, unused_vars := {#{}, NumVars}}.

env_to_scope(#{context := Context}) ->
  #elixir_erl{context=Context}.

env_to_scope_with_vars(Env, Vars) ->
  Map = maps:from_list(Vars),
  (env_to_scope(Env))#elixir_erl{
    vars={Map, false},
    counter=#{'_' => map_size(Map)}
  }.

reset_vars(Env) ->
  Env#{vars := [], current_vars := {#{}, false}, unused_vars := {#{}, 0}}.

%% SCOPE MERGING

reset_read(#{current_vars := {_, Write}} = E, #{current_vars := {Read, _}}) ->
  E#{current_vars := {Read, Write}}.

prepare_write(#{current_vars := {Read, _}} = E) ->
  E#{current_vars := {Read, Read}}.

close_write(#{current_vars := {_Read, Write}} = E, #{current_vars := {_, false}}) ->
  E#{current_vars := {Write, false}};
close_write(#{current_vars := {_Read, Write}} = E, #{current_vars := {_, UpperWrite}}) ->
  E#{current_vars := {Write, merge_vars(UpperWrite, Write)}}.

merge_vars(V, V) ->
  V;
merge_vars(V1, V2) ->
  maps:fold(fun(K, M2, Acc) ->
    case Acc of
      #{K := M1} when M1 >= M2 -> Acc;
      _ -> Acc#{K => M2}
    end
  end, V1, V2).

%% UNUSED VARS

reset_unused_vars(#{unused_vars := {_Unused, Version}} = E) ->
  E#{unused_vars := {#{}, Version}}.

check_unused_vars(#{unused_vars := {Unused, _Version}} = E) ->
  [elixir_errors:form_warn([{line, Line}], E, ?MODULE, {unused_var, Name}) ||
    {{{Name, _}, _}, Line} <- maps:to_list(Unused), Line /= false, not_underscored(Name)],
  E.

merge_and_check_unused_vars(E, #{unused_vars := {ClauseUnused, Version}}) ->
  #{current_vars := {Read, _}, unused_vars := {Unused, _Version}} = E,
  E#{unused_vars := {merge_and_check_unused_vars(Read, Unused, ClauseUnused, E), Version}}.

merge_and_check_unused_vars(Current, Unused, ClauseUnused, E) ->
  maps:fold(fun({Var, Count} = Key, ClauseValue, Acc) ->
    case Current of
      %% The parent knows it, so we have to propagate up.
      #{Var := CurrentCount} when Count =< CurrentCount ->
        Acc#{Key => ClauseValue};

      %% The parent doesn't know it and we didn't use it
      #{} when ClauseValue /= false ->
        {{Name, _}, _} = Key,

        case not_underscored(Name) of
          true ->
            Warn = {unused_var, Name},
            elixir_errors:form_warn([{line, ClauseValue}], E, ?MODULE, Warn);

          false ->
            ok
        end,

        Acc;

      %% The parent doesn't know it and we used it
      #{} ->
        Acc
    end
  end, Unused, ClauseUnused).

not_underscored(Name) ->
  case atom_to_list(Name) of
    "_" ++ _ -> false;
    _ -> true
  end.

format_error({unused_var, Name}) ->
  io_lib:format("variable \"~ts\" is unused (if the variable is not meant to be used, prefix it with an underscore)", [Name]).
