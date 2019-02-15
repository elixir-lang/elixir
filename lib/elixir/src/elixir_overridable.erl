% Holds the logic responsible for defining overridable functions and handling super.
-module(elixir_overridable).
-export([overridable_for/2, record_overridable/4, super/4, store_not_overriden/1, format_error/1]).
-include("elixir.hrl").
-define(overriden_pos, 5).

overridables_for(Module) ->
  {_, Bag} = elixir_module:data_tables(Module),
  ets:lookup(Bag, overridable).

overridable_for(Module, Tuple) ->
  {Set, _} = elixir_module:data_tables(Module),

  case ets:lookup(Set, {overridable, Tuple}) of
    [Overridable] -> Overridable;
    [] -> not_overridable
  end.

record_overridable(Module, Tuple, Def, Neighbours) ->
  {Set, Bag} = elixir_module:data_tables(Module),

  case ets:insert_new(Set, {{overridable, Tuple}, 1, Def, Neighbours, false}) of
    true ->
      ets:insert(Bag, {overridable, Tuple});
    false ->
      [{_, Count, _, _, _}] = ets:lookup(Set, {overridable, Tuple}),
      ets:insert(Set, {{overridable, Tuple}, Count + 1, Def, Neighbours, false})
  end,

  ok.

super(Meta, Module, Tuple, E) ->
  {Set, _} = elixir_module:data_tables(Module),

  case ets:lookup(Set, {overridable, Tuple}) of
    [Overridable] ->
      store(Set, Module, Tuple, Overridable, true);
    [] ->
      elixir_errors:form_error(Meta, ?key(E, file), ?MODULE, {no_super, Module, Tuple})
  end.

store_not_overriden(Module) ->
  {Set, Bag} = elixir_module:data_tables(Module),

  [begin
    [Overridable] = ets:lookup(Set, {overridable, Tuple}),
    {_, _, _} = store(Set, Module, Tuple, Overridable, false),
    Tuple
  end || {overridable, Tuple} <- ets:lookup(Bag, overridable),
          not ets:member(Set, {def, Tuple})].

%% Private

store(Set, Module, Tuple, {_, Count, Def, Neighbours, Overriden}, Hidden) ->
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

  case Overriden of
    false ->
      ets:update_element(Set, {overridable, Tuple}, {?overriden_pos, true}),
      elixir_def:store_definition(false, FinalKind, Meta, FinalName, FinalArity,
                                  File, Module, Defaults, FinalClauses),
      elixir_locals:reattach({FinalName, FinalArity}, FinalKind, Module, Tuple, Neighbours, Meta);
    true ->
      ok
  end,

  {FinalKind, FinalName, Meta}.

name(Name, Count) when is_integer(Count) ->
  list_to_atom(atom_to_list(Name) ++ " (overridable " ++ integer_to_list(Count) ++ ")").

%% Error handling

format_error({no_super, Module, {Name, Arity}}) ->
  Bins   = [format_fa(Tuple) || {overridable, Tuple} <- overridables_for(Module)],
  Joined = 'Elixir.Enum':join(Bins, <<", ">>),
  io_lib:format("no super defined for ~ts/~B in module ~ts. Overridable functions available are: ~ts",
    [Name, Arity, elixir_aliases:inspect(Module), Joined]).

format_fa({Name, Arity}) ->
  A = 'Elixir.Code.Identifier':inspect_as_function(Name),
  B = integer_to_binary(Arity),
  <<A/binary, $/, B/binary>>.
