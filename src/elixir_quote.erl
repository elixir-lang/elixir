-module(elixir_quote).
-export([translate/3, translate_each/3, linify/2]).
-include("elixir.hrl").

%% Apply the line from site call on quoted contents.

linify(Line, { Left, 0, Right }) ->
  { linify(Line, Left), Line, linify(Line, Right) };

linify(Line, Tuple) when is_tuple(Tuple) ->
  list_to_tuple(linify(Line, tuple_to_list(Tuple)));

linify(Line, List) when is_list(List) ->
  [linify(Line, X) || X <- List];

linify(_, Else) -> Else.

%% Translation

translate(Forms, Q, S) ->
  lists:mapfoldl(fun(X, Acc) -> translate_each(X, Q, Acc) end, S, Forms).

translate_each({ unquote, _Line, [Expr] }, #elixir_quote{unquote=true}, S) ->
  elixir_translator:translate_each(Expr, S);

translate_each({ Left, Line, nil }, Q, S) when is_atom(Left) ->
  Tuple = { tuple, Line, [
    { atom, Line, Left },
    { integer, Line, Q#elixir_quote.line },
    { atom, Line, Q#elixir_quote.marker }
  ] },
  { Tuple, S };

translate_each({ Left, Line, Right }, Q, S) ->
  { TLeft, LS } = translate_each(Left, Q, S),
  { TRight, RS } = translate_each(Right, Q, LS),

  % We need to remove line numbers from quoted exprs otherwise
  % the line number quotes in the macro will get mixed with the
  % original exprs line numbers given to the macro as arguments.
  Tuple = { tuple, Line, [TLeft, { integer, Line, Q#elixir_quote.line }, TRight] },
  { Tuple, RS };

translate_each({ Left, Right }, Q, S) ->
  { TLeft, LS } = translate_each(Left, Q, S),
  { TRight, RS } = translate_each(Right, Q, LS),
  { { tuple, 0, [TLeft, TRight] }, RS };

translate_each(List, Q, S) when is_list(List) ->
  splice(List, Q, [], [], S);

translate_each(Number, Q, S) when is_integer(Number) ->
  { { integer, Q#elixir_quote.line, Number }, S };

translate_each(Number, Q, S) when is_float(Number) ->
  { { float, Q#elixir_quote.line, Number }, S };

translate_each(Atom, Q, S) when is_atom(Atom) ->
  { { atom, Q#elixir_quote.line, Atom }, S };

translate_each(Bitstring, _Q, S) when is_bitstring(Bitstring) ->
  { elixir_tree_helpers:abstract_syntax(Bitstring), S }.

% Loop through the list finding each unquote_splicing entry.

splice([{ unquote_splicing, _, [Args] }|T], #elixir_quote{unquote=true} = Q, Buffer, Acc, S) ->
  { NewAcc, NewS } = from_buffer_to_acc(Buffer, Q, Acc, S),
  { TArgs, TS } = elixir_translator:translate_each(Args, NewS),
  splice(T, Q, [], [TArgs|NewAcc], TS);

splice([H|T], Q, Buffer, Acc, S) ->
  splice(T, Q, [H|Buffer], Acc, S);

splice([], Q, Buffer, Acc, S) ->
  { NewAcc, NewS } = from_buffer_to_acc(Buffer, Q, Acc, S),
  case NewAcc of
    [] ->
      { { nil, Q#elixir_quote.line }, NewS };
    [List] ->
      { List, NewS };
    _ ->
      List = elixir_tree_helpers:build_simple_reverse_list(0, NewAcc),
      { ?ELIXIR_WRAP_CALL(0, lists, append, [List]), NewS }
  end.

from_buffer_to_acc([], _Q, Acc, S) ->
  { Acc, S };

from_buffer_to_acc(Buffer, Q, Acc, S) ->
  { New, NewS } = elixir_tree_helpers:build_reverse_list(
    fun(X, AccS) -> translate_each(X, Q, AccS) end, Buffer, 0, S),
  { [New|Acc], NewS }.