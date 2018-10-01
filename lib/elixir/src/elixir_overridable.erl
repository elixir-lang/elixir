% Holds the logic responsible for defining overridable functions and handling super.
-module(elixir_overridable).
-export([setup/1, overridable/1, overridable/2, super/4, store_pending/1, format_error/1]).
-include("elixir.hrl").
-define(attr, {elixir, overridable}).

%% TODO: Use DataSet/DataBag for overridables
setup({DataSet, _DataBag}) ->
  ets:insert(DataSet, {?attr, #{}}).

overridable(Module) ->
  {Set, _} = elixir_module:data_tables(Module),
  ets:lookup_element(Set, ?attr, 2).

overridable(Module, Value) ->
  {Set, _} = elixir_module:data_tables(Module),
  ets:insert(Set, {?attr, Value}).

super(Meta, Module, Function, E) ->
  case store(Module, Function, true) of
    {_, _, _} = KindNameMeta ->
      KindNameMeta;
    error ->
      elixir_errors:form_error(Meta, ?key(E, file), ?MODULE, {no_super, Module, Function})
  end.

store_pending(Module) ->
  {Set, _} = elixir_module:data_tables(Module),

  [begin
    {_, _, _} = store(Module, Pair, false),
    Pair
   end || {Pair, {_, _, _, false}} <- maps:to_list(overridable(Module)),
          not ets:member(Set, {def, Pair})].

%% Private

store(Module, Function, Hidden) ->
  Overridable = overridable(Module),
  case maps:find(Function, Overridable) of
    {ok, {Count, Def, Neighbours, Overridden}} ->
      {{{def, {Name, Arity}}, Kind, Meta, File, _Check,
       {Defaults, _HasBody, _LastDefaults}}, Clauses} = Def,

      {FinalKind, FinalName, FinalArity, FinalClauses} =
        case Hidden of
          false ->
            {Kind, Name, Arity, Clauses};
          true when Kind == defmacro; Kind == defmacrop ->
            {defmacrop, name(Name, Count), Arity, Clauses};
          true ->
            {defp, name(Name, Count), Arity, Clauses}
        end,

      Tuple = {FinalName, FinalArity},

      case Overridden of
        false ->
          overridable(Module, maps:put(Function, {Count, Def, Neighbours, true}, Overridable)),
          elixir_def:store_definition(false, FinalKind, Meta, FinalName, FinalArity,
                                      File, Module, Defaults, FinalClauses),
          elixir_locals:reattach(Tuple, FinalKind, Module, Function, Neighbours, Meta);
        true ->
          ok
      end,

      {FinalKind, FinalName, Meta};
    error ->
      error
  end.

name(Name, Count) when is_integer(Count) ->
  list_to_atom(atom_to_list(Name) ++ " (overridable " ++ integer_to_list(Count) ++ ")").

%% Error handling

format_error({no_super, Module, {Name, Arity}}) ->
  Bins   = [format_fa(X) || {X, {_, _, _, _}} <- maps:to_list(overridable(Module))],
  Joined = 'Elixir.Enum':join(Bins, <<", ">>),
  io_lib:format("no super defined for ~ts/~B in module ~ts. Overridable functions available are: ~ts",
    [Name, Arity, elixir_aliases:inspect(Module), Joined]).

format_fa({Name, Arity}) ->
  A = 'Elixir.Code.Identifier':inspect_as_function(Name),
  B = integer_to_binary(Arity),
  <<A/binary, $/, B/binary>>.
