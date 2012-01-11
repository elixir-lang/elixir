-module(elixir_quote).
-export([translate/2, translate_each/2]).
-include("elixir.hrl").

translate(Forms, S) ->
  lists:mapfoldl(fun translate_each/2, S, Forms).

translate_each({ unquote, _Line, [Expr] }, S) ->
  elixir_translator:translate_each(Expr, S);

translate_each({ Left, Line, Right }, S) when is_atom(Left), is_atom(Right) -> %% Variables
  Tuple = { tuple, Line, [{ atom, Line, Left }, { integer, Line, 0 }, { atom, Line, quoted }] },
  { Tuple, S };

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
  splice(List, [], [], S);

translate_each(Number, S) when is_integer(Number) ->
  { { integer, 0, Number }, S };

translate_each(Number, S) when is_float(Number) ->
  { { float, 0, Number }, S };

translate_each(Atom, S) when is_atom(Atom) ->
  { { atom, 0, Atom }, S };

translate_each(Bitstring, S) when is_bitstring(Bitstring) ->
  { elixir_tree_helpers:abstract_syntax(Bitstring), S }.

% Loop through the list finding each unquote_splice entry.

splice([{ unquote_splice, _, [Args] }|T], Buffer, Acc, S) ->
  { NewAcc, NewS } = from_buffer_to_acc(Buffer, Acc, S),
  { TArgs, TS } = elixir_translator:translate_each(Args, NewS),
  splice(T, [], [TArgs|NewAcc], TS);

splice([H|T], Buffer, Acc, S) ->
  splice(T, [H|Buffer], Acc, S);

splice([], Buffer, Acc, S) ->
  { NewAcc, NewS } = from_buffer_to_acc(Buffer, Acc, S),
  case NewAcc of
    [List] -> { List, NewS };
    _ ->
      List = elixir_tree_helpers:build_simple_reverse_list(0, NewAcc),
      { ?ELIXIR_WRAP_CALL(0, lists, append, [List]), NewS }
  end.

from_buffer_to_acc([], Acc, S) ->
  { Acc, S };

from_buffer_to_acc(Buffer, Acc, S) ->
  { New, NewS } = elixir_tree_helpers:build_reverse_list(fun translate_each/2, Buffer, 0, S),
  { [New|Acc], NewS }.