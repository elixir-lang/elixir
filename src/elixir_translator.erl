-module(elixir_translator).
-export([translate/2]).
-include("elixir.hrl").

translate(Forms, S) ->
  lists:mapfoldl(fun translate_each/2, S, Forms).

translate_each({ Op, Line, Left, Right }, S) when Op == '+'; Op == '-'; Op == '*'; Op == '/' ->
  { TLeft, _ }  = translate_each(Left, S),
  { TRight, _ } = translate_each(Right, S),
  { { op, Line, Op, TLeft, TRight }, S };

translate_each(Number, S) when is_integer(Number) ->
  { { integer, 0, Number }, S };

translate_each(Number, S) when is_float(Number) ->
  { { float, 0, Number }, S };

translate_each(Atom, S) when is_atom(Atom) ->
  { { atom, 0, Atom }, S }.