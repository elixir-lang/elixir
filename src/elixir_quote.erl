-module(elixir_quote).
-export([translate/2, translate_each/2]).
-include("elixir.hrl").

translate(Forms, S) ->
  lists:mapfoldl(fun translate_each/2, S, Forms).

translate_each({ unquote, Line, [Expr] }, S) ->
  elixir_translator:translate_each(Expr, S);

translate_each({ Left, Line, Right }, S) ->
  { TLeft, LS } = translate_each(Left, S),
  { TRight, RS } = translate_each(Right, LS),

  % We need to remove line numbers from quoted exprs otherwise
  % the line number quotes in the macro will get mixed with the
  % original exprs line numbers given to the macro as arguments.
  Tuple = { tuple, Line, [TLeft, { integer, Line, 0 }, TRight] },
  { Tuple, RS };

translate_each({ Left, Right }, S) ->
  { TLeft, LS } = translate_each(Left, S),
  { TRight, RS } = translate_each(Right, LS),
  { { tuple, 0, [TLeft, TRight] }, RS };

translate_each(List, S) when is_list(List) ->
  elixir_tree_helpers:build_list(fun translate_each/2, List, 0, S);

translate_each(Number, S) when is_integer(Number) ->
  { { integer, 0, Number }, S };

translate_each(Number, S) when is_float(Number) ->
  { { float, 0, Number }, S };

translate_each(Atom, S) when is_atom(Atom) ->
  { { atom, 0, Atom }, S }.