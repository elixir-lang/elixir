-module(elixir_transform).
-export([parse_transform/2]).

parse_transform(Forms, _) ->
  do_transform(Forms).

do_transform({ atom, Line, Atom }) ->
  case atom_to_list(Atom) of
    "Elixir." ++ _ = List ->
      Module = [case T of
        $. -> $-;
        _  -> T
      end || T <- List],
      { atom, Line, list_to_atom(Module) };
    _ ->
      { atom, Line, Atom }
  end;

do_transform({ Name, Item1 }) ->
  { Name, do_transform(Item1) };

do_transform({ Name, Item1, Item2 }) ->
  { Name, do_transform(Item1), do_transform(Item2) };

do_transform({ Name, Item1, Item2, Item3 }) ->
  { Name, do_transform(Item1), do_transform(Item2), do_transform(Item3) };

do_transform({ Name, Item1, Item2, Item3, Item4 }) ->
  { Name, do_transform(Item1), do_transform(Item2), do_transform(Item3), do_transform(Item4) };

do_transform({ Name, Item1, Item2, Item3, Item4, Item5 }) ->
  { Name, do_transform(Item1), do_transform(Item2), do_transform(Item3), do_transform(Item4), do_transform(Item5) };

do_transform(List) when is_list(List) ->
  [do_transform(X) || X <- List];

do_transform(Other) when not is_tuple(Other) ->
  Other.