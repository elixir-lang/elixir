% Holds the logic responsible for defining overridable functions and handling super.
-module(elixir_overridable).
-export([overridable_for/2, record_overridable/4, super/4, store_not_overriden/1, format_error/1]).
-include("elixir.hrl").
-define(overriden_pos, 5).

overridables_for(Module) ->
  {_, Bag} = elixir_module:data_tables(Module),
  ets:lookup(Bag, overridables).

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
      ets:insert(Bag, {overridables, Tuple});
    false ->
      [{_, Count, PreviousDef, _, _}] = ets:lookup(Set, {overridable, Tuple}),
      {{_, Kind, Meta, File, _, _}, _} = Def,
      {{_, PreviousKind, _, _, _, _}, _} = PreviousDef,

      case is_valid_kind(Kind, PreviousKind) of
        true ->
          ets:insert(Set, {{overridable, Tuple}, Count + 1, Def, Neighbours, false});
        false ->
          elixir_errors:form_error(Meta, File, ?MODULE, {bad_kind, Module, Tuple, Kind})
      end
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

  lists:foreach(fun({_, Tuple}) ->
    [Overridable] = ets:lookup(Set, {overridable, Tuple}),

    case ets:lookup(Set, {def, Tuple}) of
      [] ->
        store(Set, Module, Tuple, Overridable, false);
      [{_, Kind, Meta, File, _, _}] ->
        {{_, OverridableKind, _, _, _, _}, _} = element(3, Overridable),

        case is_valid_kind(Kind, OverridableKind) of
          true -> ok;
          false -> elixir_errors:form_error(Meta, File, ?MODULE, {bad_kind, Module, Tuple, Kind})
        end
    end
  end, ets:lookup(Bag, overridables)).

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

is_valid_kind(NewKind, PreviousKind) ->
  is_macro(NewKind) =:= is_macro(PreviousKind).

is_macro(defmacro) -> true;
is_macro(defmacrop) -> true;
is_macro(_) -> false.

%% Error handling
format_error({bad_kind, Module, {Name, Arity}, Kind}) ->
  case is_macro(Kind) of
    true ->
      io_lib:format("cannot override function (def, defp) ~ts/~B in module ~ts as a macro (defmacro, defmacrop)",
        [Name, Arity, elixir_aliases:inspect(Module)]);
    false ->
      io_lib:format("cannot override macro (defmacro, defmacrop) ~ts/~B in module ~ts as a function (def, defp)",
        [Name, Arity, elixir_aliases:inspect(Module)])
  end;

format_error({no_super, Module, {Name, Arity}}) ->
  Bins   = [format_fa(Tuple) || {_, Tuple} <- overridables_for(Module)],
  Joined = 'Elixir.Enum':join(Bins, <<", ">>),
  io_lib:format("no super defined for ~ts/~B in module ~ts. Overridable functions available are: ~ts",
    [Name, Arity, elixir_aliases:inspect(Module), Joined]).

format_fa({Name, Arity}) ->
  A = 'Elixir.Code.Identifier':inspect_as_function(Name),
  B = integer_to_binary(Arity),
  <<A/binary, $/, B/binary>>.
