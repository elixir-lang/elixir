% Holds the logic responsible for defining overridable functions and handling super.
-module(elixir_def_overridable).
-export([store_pending/1, is_defined/2, ensure_defined/4,
  assign_args/3, retrieve_args/3, name/2, store/3, format_error/1]).
-include("elixir.hrl").
-compile({parse_transform, elixir_transform}).

overridable(Module) ->
  ets:lookup_element(elixir_module:data_table(Module), '__overridable', 2).

overridable(Module, Value) ->
  ets:insert(elixir_module:data_table(Module), { '__overridable', Value }).

%% Check if an overridable function is defined.

is_defined(Module, Tuple) ->
  Overridable = overridable(Module),
  case orddict:find(Tuple, Overridable) of
    { ok, { _, _, _ } } -> true;
    _ -> false
  end.

ensure_defined(Line, Module, Tuple, S) ->
  case is_defined(Module, Tuple) of
    true -> [];
    _    -> elixir_errors:form_error(Line, S#elixir_scope.file, ?MODULE, { no_super, Module, Tuple })
  end.

%% Retrieve args defined for the given arity.

retrieve_args(Line, Arity, S) ->
  {
    [ { var, Line, super_arg(X) } || X <- lists:seq(1, Arity) ],
    S#elixir_scope{name_args=true}
  }.

%% Assign pseudo variables to the given vars.

assign_args(Line, Args, S) ->
  { FArgs, _ } = lists:mapfoldl(fun(X, Acc) -> assign_args(Line, X, Acc, S) end, 1, Args),
  FArgs.

assign_args(Line, X, Acc, _) ->
  Match = { match, Line, X, { var, Line, super_arg(Acc) } },
  { Match, Acc + 1 }.

super_arg(Counter) ->
  ?ELIXIR_ATOM_CONCAT(['_@S', Counter]).

%% Gets the name based on the function and stored overridables

name(Module, Function) ->
  name(Module, Function, overridable(Module)).

name(_Module, { Name, _ } = Function, Overridable) ->
  { Count, _, _ } = orddict:fetch(Function, Overridable),
  ?ELIXIR_ATOM_CONCAT([Name, " (overridable ", Count, ")"]).

%% Store

store(Module, Function, GenerateName) ->
  Overridable = overridable(Module),
  case orddict:fetch(Function, Overridable) of
    { _Count, _Clause, true } -> ok;
    { Count, Clause, false } ->
      overridable(Module, orddict:store(Function, { Count, Clause, true }, Overridable)),
      { { Name, Arity }, Kind, Line, File, _Check, Location, Defaults, Clauses } = Clause,

      { FinalKind, FinalName } = case GenerateName of
        true  -> { defp, name(Module, Function, Overridable) };
        false -> { Kind, Name }
      end,

      Def = { function, Line, FinalName, Arity, Clauses },
      elixir_def:store_each(false, FinalKind, File, Location,
        elixir_def:table(Module), Defaults, Def)
  end.

%% Store pending declarations that were not manually made concrete.

store_pending(Module) ->
  [store(Module, X, false) || { X, { _, _, false } } <- overridable(Module),
    not 'Elixir.Module':'defines?'(Module, X)].

%% Error handling

format_error({ no_super, Module, { Name, Arity } }) ->
  Bins   = [ format_fa(X) || { X, { _, _, _ } } <- overridable(Module)],
  Joined = 'Elixir.Enum':join(Bins, <<", ">>),
  io_lib:format("no super defined for ~s/~B in module ~s. Overridable functions available are: ~s",
    [Name, Arity, elixir_errors:inspect(Module), Joined]).

format_fa({ Name, Arity }) ->
  A = atom_to_binary(Name, utf8),
  B = list_to_binary(integer_to_list(Arity)),
  << A/binary, $/, B/binary >>.