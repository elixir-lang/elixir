% Holds the logic responsible for defining overridable functions and handling super.
-module(elixir_def_overridable).
-export([setup/1, overridable/1, overridable/2, kind_and_name/2, super/2, store_pending/1,
         ensure_defined/4, format_error/1]).
-include("elixir.hrl").
-define(attr, {elixir, overridable}).

setup(Module) ->
  overridable(Module, #{}).

overridable(Module) ->
  ets:lookup_element(elixir_module:data_table(Module), ?attr, 2).

overridable(Module, Value) ->
  ets:insert(elixir_module:data_table(Module), {?attr, Value}).

%% Check if an overridable function is defined.

ensure_defined(Meta, Module, Tuple, S) ->
  Overridable = overridable(Module),
  case maps:find(Tuple, Overridable) of
    {ok, {_, _, _, _}} -> ok;
    _ -> elixir_errors:form_error(Meta, S#elixir_scope.file, ?MODULE, {no_super, Module, Tuple})
  end.

%% Builds a function name based on how many times the function
%% has been overridden.

name({Name, _} = _Function, Count) when is_integer(Count) ->
  elixir_utils:atom_concat([Name, " (overridable ", Count, ")"]).

%% Returns the kind (def, defp, and so on) of the given overridable function
%% and its overridable name.

kind_and_name(Module, FunctionOrMacro) ->
  Overridable = overridable(Module),
  {Count, Clause, _, _} = maps:get(FunctionOrMacro, Overridable),
  {{{def, _Function}, Kind, _Line, _File, _Check, _Location, _Defaults}, _Clauses} = Clause,
  {Kind, name(FunctionOrMacro, Count)}.

%% Store

store(Module, Function, Hidden) ->
  Overridable = overridable(Module),
  case maps:get(Function, Overridable) of
    {_Count, _Clause, _Neighbours, true} ->
      ok;
    {Count, Clause, Neighbours, false} ->
      overridable(Module, maps:put(Function, {Count, Clause, Neighbours, true}, Overridable)),
      {{{def, {Name, Arity}}, Kind, Line, File, _Check,
       Location, {Defaults, _HasBody, _LastDefaults}}, Clauses} = Clause,

      {FinalKind, FinalName} =
        case Hidden of
          false -> {Kind, Name};
          true -> {to_private(Kind), name(Function, Count)}
        end,

      case elixir_compiler:get_opt(internal) of
        false ->
          'Elixir.Module.LocalsTracker':reattach(Module, Kind, {Name, Arity}, Neighbours);
        true ->
          ok
      end,

      Def = {function, Line, FinalName, Arity, Clauses},
      elixir_def:store_each(false, FinalKind, File, Location, Module, Defaults, Def)
  end.

super(Module, Function) ->
  store(Module, Function, true).

store_pending(Module) ->
  _ = [store(Module, X, false) || {X, {_, _, _, false}} <- maps:to_list(overridable(Module)),
    not 'Elixir.Module':'defines?'(Module, X)],
  ok.

to_private(def) -> defp;
to_private(defp) -> defp;
to_private(defmacro) -> defmacrop;
to_private(defmacrop) -> defmacrop.

%% Error handling

format_error({no_super, Module, {Name, Arity}}) ->
  Bins   = [format_fa(X) || {X, {_, _, _, _}} <- maps:to_list(overridable(Module))],
  Joined = 'Elixir.Enum':join(Bins, <<", ">>),
  io_lib:format("no super defined for ~ts/~B in module ~ts. Overridable functions available are: ~ts",
    [Name, Arity, elixir_aliases:inspect(Module), Joined]).

format_fa({Name, Arity}) ->
  A = atom_to_binary(Name, utf8),
  B = integer_to_binary(Arity),
  <<A/binary, $/, B/binary>>.
