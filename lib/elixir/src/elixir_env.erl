-module(elixir_env).
-include("elixir.hrl").
-export([
  new/0, linify/1, with_vars/2, reset_vars/1,
  env_to_scope/1, env_to_scope_with_vars/2,
  check_unused_vars/1, merge_and_check_unused_vars/2,
  mergea/2, mergev/2, format_error/1
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
    unused_vars => #{},                    %% a map with unused variables
    current_vars => #{},                   %% a map with current variables
    prematch_vars => warn,                 %% behaviour outside and inside matches
    lexical_tracker => nil,                %% holds the lexical tracker PID
    contextual_vars => []}.                %% holds available contextual variables

linify({Line, Env}) ->
  Env#{line := Line};
linify(#{} = Env) ->
  Env.

with_vars(Env, Vars) ->
  CurrentVars = maps:from_list([{Var, 0} || Var <- Vars]),
  Env#{vars := Vars, current_vars := CurrentVars}.

env_to_scope(#{context := Context}) ->
  #elixir_erl{context=Context}.

env_to_scope_with_vars(Env, Vars) ->
  Map = maps:from_list(Vars),
  (env_to_scope(Env))#elixir_erl{
    vars=Map, counter=#{'_' => map_size(Map)}
  }.

reset_vars(Env) ->
  Env#{vars := [], current_vars := #{}, unused_vars := #{}}.

%% SCOPE MERGING

%% Receives two scopes and return a new scope based on the second
%% with their variables merged.
%% Unrolled for performance reasons.
mergev(#{unused_vars := U1, current_vars := C1},
       #{unused_vars := U2, current_vars := C2} = E2) ->
  if
    C1 =/= C2 ->
      if
        U1 =/= U2 ->
          C = merge_vars(C1, C2),
          E2#{vars := maps:keys(C), unused_vars := merge_vars(U1, U2), current_vars := C};
        true ->
          C = merge_vars(C1, C2),
          E2#{vars := maps:keys(C), current_vars := C}
      end;

    U1 =/= U2 ->
      E2#{unused_vars := merge_vars(U1, U2)};

    true ->
      E2
  end.

%% Receives two scopes and return the later scope
%% keeping the variables from the first (imports
%% and everything else are passed forward).
%% Unrolled for performance reasons.
mergea(#{unused_vars := U1, current_vars := C1, vars := V1},
       #{unused_vars := U2, current_vars := C2} = E2) ->
  if
    C1 =/= C2 ->
      if
        U1 =/= U2 ->
          E2#{vars := V1, unused_vars := U1, current_vars := C1};
        true ->
          E2#{vars := V1, current_vars := C1}
      end;
    U1 =/= U2 ->
      E2#{unused_vars := U1};
    true ->
      E2
  end.

merge_vars(V1, V2) ->
  maps:fold(fun(K, M2, Acc) ->
    case Acc of
      #{K := M1} when M1 >= M2 -> Acc;
      _ -> Acc#{K => M2}
    end
  end, V1, V2).


%% UNUSED VARS

check_unused_vars(#{unused_vars := Unused} = E) ->
  [elixir_errors:form_warn([{line, Line}], ?key(E, file), ?MODULE, {unused_var, Name}) ||
    {{{Name, _}, _}, Line} <- maps:to_list(Unused), Line /= false, not_underscored(Name)],
  E.

merge_and_check_unused_vars(#{unused_vars := Unused} = E, #{unused_vars := ClauseUnused}) ->
  E#{unused_vars := merge_and_check_unused_vars(Unused, ClauseUnused, E)}.

merge_and_check_unused_vars(Unused, ClauseUnused, E) ->
  maps:fold(fun(Key, ClauseValue, Acc) ->
    case ClauseValue of
      %% The variable was used...
      false ->
        case Acc of
          %% So we propagate if it was not yet used
          #{Key := Value} when Value /= false ->
            Acc#{Key := false};

          %% Otherwise we don't know it or it was already used
          _ ->
            Acc
        end;

      %% The variable was not used...
      _ ->
        case Acc of
          %% If we know it, there is nothing to propagate
          #{Key := _} ->
            Acc;

          %% Otherwise we must warn
          _ ->
            {{Name, _}, _} = Key,

            case not_underscored(Name) of
              true ->
                Warn = {unused_var, Name},
                elixir_errors:form_warn([{line, ClauseValue}], ?key(E, file), ?MODULE, Warn);

              false ->
                ok
            end,

            Acc
        end
    end
  end, Unused, ClauseUnused).

not_underscored(Name) ->
  case atom_to_list(Name) of
    "_" ++ _ -> false;
    _ -> true
  end.

format_error({unused_var, Name}) ->
  io_lib:format("variable \"~ts\" is unused (if the variable is not meant to be used, prefix it with an underscore)", [Name]).
