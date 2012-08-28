-module(elixir_quote).
-export([quote/3, linify/2]).
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

quote({ unquote, _Line, [Expr] }, #elixir_quote{unquote=true}, S) ->
  elixir_translator:translate_each(Expr, S);

quote({ { '.', Line, [Left, unquote] }, _Line, [Expr] }, Q, S) ->
  Rewritten = { '.', Line, [Left, { unquote, Line, [Expr] }] },
  quote(Rewritten, Q, S);

quote({ Left, Line, nil }, Q, S) when is_atom(Left) ->
  Tuple = { tuple, Line, [
    { atom, Line, Left },
    { integer, Line, line(Line, Q) },
    { atom, Line, Q#elixir_quote.marker }
  ] },
  { Tuple, S };

quote({ Left, Line, Right }, Q, S) ->
  { TLeft, LS } = quote(Left, Q, S),
  { TRight, RS } = quote(Right, Q, LS),

  %% We need to remove line numbers from quoted exprs
  %% otherwise the line number quotes in the macro will
  %% get mixed with the unquoted contents
  Tuple = { tuple, Line, [TLeft, { integer, Line, line(Line, Q) }, TRight] },
  { Tuple, RS };

% Handle two item tuples but still allow them to be spliced.
quote({ Left, Right }, Q, S) when
  not is_tuple(Left)  orelse (element(1, Left) /= unquote_splicing),
  not is_tuple(Right) orelse (element(1, Right) /= unquote_splicing) ->
  { TLeft, LS } = quote(Left, Q, S),
  { TRight, RS } = quote(Right, Q, LS),
  { { tuple, line(0, Q), [TLeft, TRight] }, RS };

quote({ Left, Right }, Q, S) ->
  quote({ '{}', 0, [Left, Right] }, Q, S);

quote(List, Q, S) when is_list(List) ->
  splice(List, Q, [], [], S);

quote(Number, Q, S) when is_integer(Number) ->
  { { integer, line(0, Q), Number }, S };

quote(Number, Q, S) when is_float(Number) ->
  { { float, line(0, Q), Number }, S };

quote(Atom, Q, S) when is_atom(Atom) ->
  { { atom, line(0, Q), Atom }, S };

quote(Bitstring, _Q, S) when is_bitstring(Bitstring) ->
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
      { { nil, line(0, Q) }, NewS };
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
    fun(X, AccS) -> quote(X, Q, AccS) end, Buffer, 0, S),
  { [New|Acc], NewS }.

line(Line, #elixir_quote{line=keep})  -> Line;
line(_Line, #elixir_quote{line=Line}) -> Line.