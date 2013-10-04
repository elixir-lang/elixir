% Holds the logic responsible for defining overridable functions and handling super.
-module(elixir_def_overridable).
-export([store_pending/1, is_defined/2, ensure_defined/4,
  name/2, store/3, format_error/1]).
-include("elixir.hrl").

overridable(Module) ->
  ets:lookup_element(elixir_module:data_table(Module), '__overridable', 2).

overridable(Module, Value) ->
  ets:insert(elixir_module:data_table(Module), { '__overridable', Value }).

%% Check if an overridable function is defined.

is_defined(Module, Tuple) ->
  Overridable = overridable(Module),
  case orddict:find(Tuple, Overridable) of
    { ok, { _, _, _, _ } } -> true;
    _ -> false
  end.

ensure_defined(Meta, Module, Tuple, S) ->
  case is_defined(Module, Tuple) of
    true -> ok;
    _    -> elixir_errors:form_error(Meta, S#elixir_scope.file, ?MODULE, { no_super, Module, Tuple })
  end.

%% Gets the name based on the function and stored overridables

name(Module, Function) ->
  name(Module, Function, overridable(Module)).

name(_Module, { Name, _ } = Function, Overridable) ->
  { Count, _, _, _ } = orddict:fetch(Function, Overridable),
  ?atom_concat([Name, " (overridable ", Count, ")"]).

%% Store

store(Module, Function, GenerateName) ->
  Overridable = overridable(Module),
  case orddict:fetch(Function, Overridable) of
    { _Count, _Clause, _Neighbours, true } -> ok;
    { Count, Clause, Neighbours, false } ->
      overridable(Module, orddict:store(Function, { Count, Clause, Neighbours, true }, Overridable)),
      { { { Name, Arity }, Kind, Line, File, _Check, Location, Defaults }, Clauses } = Clause,

      { FinalKind, FinalName } = case GenerateName of
        true  -> { defp, name(Module, Function, Overridable) };
        false -> { Kind, Name }
      end,

      'Elixir.Module.DispatchTracker':reattach(Module, Kind, { Name, Arity }, Neighbours),

      Def = { function, Line, FinalName, Arity, Clauses },
      elixir_def:store_each(false, FinalKind, File, Location,
        elixir_def:table(Module), elixir_def:clauses_table(Module), Defaults, Def)
  end.

%% Store pending declarations that were not manually made concrete.

store_pending(Module) ->
  [store(Module, X, false) || { X, { _, _, _, false } } <- overridable(Module),
    not 'Elixir.Module':'defines?'(Module, X)].

%% Error handling

format_error({ no_super, Module, { Name, Arity } }) ->
  Bins   = [format_fa(X) || { X, { _, _, _, _ } } <- overridable(Module)],
  Joined = 'Elixir.Enum':join(Bins, <<", ">>),
  io_lib:format("no super defined for ~ts/~B in module ~ts. Overridable functions available are: ~ts",
    [Name, Arity, elixir_errors:inspect(Module), Joined]).

format_fa({ Name, Arity }) ->
  A = atom_to_binary(Name, utf8),
  B = integer_to_binary(Arity),
  << A/binary, $/, B/binary >>.