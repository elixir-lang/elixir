-module(elixir_quote).
-export([translate/4, translate_each/4, linify/2]).
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

translate(Forms, Marker, Alternate, S) ->
  lists:mapfoldl(fun(X, Acc) -> translate_each(X, Marker, Alternate, Acc) end, S, Forms).

translate_each({ unquote, _Line, [Expr] }, _Marker, _Alternate, S) ->
  elixir_translator:translate_each(Expr, S);

translate_each({ Left, Line, Right }, Marker, Alternate, S) when is_atom(Left), is_atom(Right) -> %% Variables
  Final = case Right of
    quoted -> quoted;
    _ -> Marker
  end,
  Tuple = { tuple, Line, [{ atom, Line, Left }, { integer, Line, Alternate }, { atom, Line, Final }] },
  { Tuple, S };

translate_each({ Left, Line, Right }, Marker, Alternate, S) ->
  { TLeft, LS } = translate_each(Left, Marker, Alternate, S),
  { TRight, RS } = translate_each(Right, Marker, Alternate, LS),

  % We need to remove line numbers from quoted exprs otherwise
  % the line number quotes in the macro will get mixed with the
  % original exprs line numbers given to the macro as arguments.
  Tuple = { tuple, Line, [TLeft, { integer, Line, Alternate }, TRight] },
  { Tuple, RS };

translate_each({ Left, Right }, Marker, Alternate, S) ->
  { TLeft, LS } = translate_each(Left, Marker, Alternate, S),
  { TRight, RS } = translate_each(Right, Marker, Alternate, LS),
  { { tuple, 0, [TLeft, TRight] }, RS };

translate_each(List, Marker, Alternate, S) when is_list(List) ->
  splice(List, Marker, Alternate, [], [], S);

translate_each(Number, _Marker, Alternate, S) when is_integer(Number) ->
  { { integer, Alternate, Number }, S };

translate_each(Number, _Marker, Alternate, S) when is_float(Number) ->
  { { float, Alternate, Number }, S };

translate_each(Atom, _Marker, Alternate, S) when is_atom(Atom) ->
  { { atom, Alternate, Atom }, S };

translate_each(Bitstring, _Marker, _Alternate, S) when is_bitstring(Bitstring) ->
  { elixir_tree_helpers:abstract_syntax(Bitstring), S }.

% Loop through the list finding each unquote_splicing entry.

splice([{ unquote_splicing, _, [Args] }|T], Marker, Alternate, Buffer, Acc, S) ->
  { NewAcc, NewS } = from_buffer_to_acc(Buffer, Marker, Alternate, Acc, S),
  { TArgs, TS } = elixir_translator:translate_each(Args, NewS),
  splice(T, Marker, Alternate, [], [TArgs|NewAcc], TS);

splice([H|T], Marker, Alternate, Buffer, Acc, S) ->
  splice(T, Marker, Alternate, [H|Buffer], Acc, S);

splice([], Marker, Alternate, Buffer, Acc, S) ->
  { NewAcc, NewS } = from_buffer_to_acc(Buffer, Marker, Alternate, Acc, S),
  case NewAcc of
    [] ->
      { { nil, Alternate }, NewS };
    [List] ->
      { List, NewS };
    _ ->
      List = elixir_tree_helpers:build_simple_reverse_list(0, NewAcc),
      { ?ELIXIR_WRAP_CALL(0, lists, append, [List]), NewS }
  end.

from_buffer_to_acc([], _Marker, _Alternate, Acc, S) ->
  { Acc, S };

from_buffer_to_acc(Buffer, Marker, Alternate, Acc, S) ->
  { New, NewS } = elixir_tree_helpers:build_reverse_list(
    fun(X, AccS) -> translate_each(X, Marker, Alternate, AccS) end, Buffer, 0, S),
  { [New|Acc], NewS }.