-module(elixir_quote).
-export([translate/3, translate_each/3, linify/2]).
-include("elixir.hrl").

%% Apply the line from site call on quoted contents.

linify(Line, { Left, _, Right }) ->
  { linify(Line, Left), Line, linify(Line, Right) };

linify(Line, Tuple) when is_tuple(Tuple) ->
  list_to_tuple(linify(Line, tuple_to_list(Tuple)));

linify(Line, List) when is_list(List) ->
  [linify(Line, X) || X <- List];

linify(_, Else) -> Else.

%% Translation

translate(Forms, Marker, S) ->
  lists:mapfoldl(fun(X, Acc) -> translate_each(X, Marker, Acc) end, S, Forms).

translate_each({ unquote, _Line, [Expr] }, _Marker, S) ->
  elixir_translator:translate_each(Expr, S);

translate_each({ Left, Line, Right }, Marker, S) when is_atom(Left), is_atom(Right) -> %% Variables
  Tuple = { tuple, Line, [{ atom, Line, Left }, { integer, Line, 0 }, { atom, Line, Marker }] },
  { Tuple, S };

translate_each({ Left, Line, Right }, Marker, S) ->
  { TLeft, LS } = translate_each(Left, Marker, S),
  { TRight, RS } = translate_each(Right, Marker, LS),

  % We need to remove line numbers from quoted exprs otherwise
  % the line number quotes in the macro will get mixed with the
  % original exprs line numbers given to the macro as arguments.
  Tuple = { tuple, Line, [TLeft, { integer, Line, 0 }, TRight] },
  { Tuple, RS };

translate_each({ Left, Right }, Marker, S) ->
  { TLeft, LS } = translate_each(Left, Marker, S),
  { TRight, RS } = translate_each(Right, Marker, LS),
  { { tuple, 0, [TLeft, TRight] }, RS };

translate_each(List, Marker, S) when is_list(List) ->
  splice(List, Marker, [], [], S);

translate_each(Number, _Marker, S) when is_integer(Number) ->
  { { integer, 0, Number }, S };

translate_each(Number, _Marker, S) when is_float(Number) ->
  { { float, 0, Number }, S };

translate_each(Atom, _Marker, S) when is_atom(Atom) ->
  { { atom, 0, Atom }, S };

translate_each(Bitstring, _Marker, S) when is_bitstring(Bitstring) ->
  { elixir_tree_helpers:abstract_syntax(Bitstring), S }.

% Loop through the list finding each unquote_splicing entry.

splice([{ unquote_splicing, _, [Args] }|T], Marker, Buffer, Acc, S) ->
  { NewAcc, NewS } = from_buffer_to_acc(Buffer, Marker, Acc, S),
  { TArgs, TS } = elixir_translator:translate_each(Args, NewS),
  splice(T, Marker, [], [TArgs|NewAcc], TS);

splice([H|T], Marker, Buffer, Acc, S) ->
  splice(T, Marker, [H|Buffer], Acc, S);

splice([], Marker, Buffer, Acc, S) ->
  { NewAcc, NewS } = from_buffer_to_acc(Buffer, Marker, Acc, S),
  case NewAcc of
    [List] -> { List, NewS };
    _ ->
      List = elixir_tree_helpers:build_simple_reverse_list(0, NewAcc),
      { ?ELIXIR_WRAP_CALL(0, lists, append, [List]), NewS }
  end.

from_buffer_to_acc([], _Marker, Acc, S) ->
  { Acc, S };

from_buffer_to_acc(Buffer, Marker, Acc, S) ->
  { New, NewS } = elixir_tree_helpers:build_reverse_list(
    fun(X, AccS) -> translate_each(X, Marker, AccS) end, Buffer, 0, S),
  { [New|Acc], NewS }.