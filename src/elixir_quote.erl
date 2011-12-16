-module(elixir_quote).
-export([translate/2]).
-include("elixir.hrl").

translate({ unquote, Line, [Expr] }, S) ->
  elixir_translator:translate_each(Expr, S);

translate({ Left, Line, Right }, S) ->
  { TLeft, LS } = translate(Left, S),
  { TRight, RS } = translate(Right, LS),
  Tuple = { tuple, Line, [TLeft, { integer, Line, Line }, TRight] },
  { Tuple, RS };

translate(List, S) when is_list(List) -> 
  elixir_tree_helpers:build_list(fun translate/2, List, 0, S);

translate(Number, S) when is_integer(Number) ->
  { { integer, 0, Number }, S };

translate(Number, S) when is_float(Number) ->
  { { float, 0, Number }, S };

translate(Atom, S) when is_atom(Atom) ->
  { { atom, 0, Atom }, S }.