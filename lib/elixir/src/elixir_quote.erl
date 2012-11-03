-module(elixir_quote).
-export([quote/3, linify/2, join_quoted/5]).
-include("elixir.hrl").

%% Apply the line from site call on quoted contents.

linify(Line, { Left, 0, Right }) ->
  { linify(Line, Left), Line, linify(Line, Right) };

linify(Line, Tuple) when is_tuple(Tuple) ->
  list_to_tuple(linify(Line, tuple_to_list(Tuple)));

linify(Line, List) when is_list(List) ->
  [linify(Line, X) || X <- List];

linify(_, Else) -> Else.

%% Join quoted checks for quote arguments at runtime
%% in order to properly insert them into the tree
join_quoted(Line, Left, { '__aliases__', _, Args }, nil, _File) ->
  { '__aliases__', Line, [Left|Args] };

join_quoted(Line, Left, Right, nil, _File) when is_atom(Right) ->
  case atom_to_list(Right) of
    "Elixir-" ++ _ ->
      { '__aliases__', Line, [Left, Right] };
    _ ->
      { { '.', Line, [Left, Right] }, Line, [] }
  end;

join_quoted(Line, Left, Right, Args, _File) when is_atom(Right) ->
  { { '.', Line, [Left, Right] }, Line, Args };

join_quoted(Line, _Left, _Right, _Args, File) ->
  elixir_errors:syntax_error(Line, File, "expected unquote after dot to return an atom or an alias").

%% Translation

quote({ 'unquote_splicing', Line, _ } = Expr, #elixir_quote{unquote=true} = Q, S) ->
  do_quote({ '__block__', Line, [Expr] }, Q, S);

quote(Else, Q, S) ->
  do_quote(Else, Q, S).

do_quote({ unquote, _Line, [Expr] }, #elixir_quote{unquote=true}, S) ->
  elixir_translator:translate_each(Expr, S);

do_quote({ { { '.', Line, [Left, unquote] }, _, [Expr] }, _, Args }, #elixir_quote{unquote=true} = Q, S) ->
  All = [Left, { unquote, Line, [Expr] }, Args, S#elixir_scope.file],
  { TAll, TS } = lists:mapfoldl(fun(X, Acc) -> do_quote(X, Q, Acc) end, S, All),
  { ?ELIXIR_WRAP_CALL(Line, elixir_quote, join_quoted, [line(Line, Q)|TAll]), TS };

do_quote({ { '.', Line, [Left, unquote] }, _, [Expr] }, #elixir_quote{unquote=true} = Q, S) ->
  All = [Left, { unquote, Line, [Expr] }, nil, S#elixir_scope.file],
  { TAll, TS } = lists:mapfoldl(fun(X, Acc) -> do_quote(X, Q, Acc) end, S, All),
  { ?ELIXIR_WRAP_CALL(Line, elixir_quote, join_quoted, [line(Line, Q)|TAll]), TS };

do_quote({ Left, Line, nil }, Q, S) when is_atom(Left) ->
  Tuple = { tuple, Line, [
    { atom, Line, Left },
    line(Line, Q),
    { atom, Line, Q#elixir_quote.marker }
  ] },
  { Tuple, S };

do_quote({ Left, Line, Right }, Q, S) ->
  { TLeft, LS } = do_quote(Left, Q, S),
  { TRight, RS } = do_quote(Right, Q, LS),

  %% We need to remove line numbers from quoted exprs
  %% otherwise the line number quotes in the macro will
  %% get mixed with the unquoted contents
  Tuple = { tuple, Line, [TLeft, line(Line, Q), TRight] },
  { Tuple, RS };

% Handle two item tuples but still allow them to be spliced.
do_quote({ Left, Right }, Q, S) when
  not is_tuple(Left)  orelse (element(1, Left) /= unquote_splicing),
  not is_tuple(Right) orelse (element(1, Right) /= unquote_splicing) ->
  { TLeft, LS } = do_quote(Left, Q, S),
  { TRight, RS } = do_quote(Right, Q, LS),
  { { tuple, int_line(Q), [TLeft, TRight] }, RS };

do_quote({ Left, Right }, Q, S) ->
  do_quote({ '{}', int_line(Q), [Left, Right] }, Q, S);

do_quote(List, Q, S) when is_list(List) ->
  splice(List, Q, [], [], S);

do_quote(Number, Q, S) when is_integer(Number) ->
  { { integer, int_line(Q), Number }, S };

do_quote(Number, Q, S) when is_float(Number) ->
  { { float, int_line(Q), Number }, S };

do_quote(Atom, Q, S) when is_atom(Atom) ->
  { { atom, int_line(Q), Atom }, S };

do_quote(Bitstring, _Q, S) when is_bitstring(Bitstring) ->
  { elixir_tree_helpers:abstract_syntax(Bitstring), S }.

% Loop through the list finding each unquote_splicing entry.

splice([{ unquote_splicing, _, [Args] }|T], #elixir_quote{unquote=true} = Q, Buffer, Acc, S) ->
  { NewAcc, NewS } = from_buffer_to_acc(Buffer, Q, Acc, S),
  { TArgs, TS } = elixir_translator:translate_each(Args, NewS),
  splice(T, Q, [], [TArgs|NewAcc], TS);

splice([{ '|', Line, [{ unquote_splicing, _, [_] } = Left, Right] }], #elixir_quote{unquote=true} = Q, Buffer, Acc, S) ->
  { TLeft, SL }  = splice([Left], Q, Buffer, Acc, S),
  { TRight, SR } = do_quote(Right, Q, SL),
  { { op, Line, '++', TLeft, TRight }, SR };

splice([H|T], Q, Buffer, Acc, S) ->
  splice(T, Q, [H|Buffer], Acc, S);

splice([], Q, Buffer, Acc, S) ->
  { NewAcc, NewS } = from_buffer_to_acc(Buffer, Q, Acc, S),
  case NewAcc of
    [] ->
      { { nil, int_line(Q) }, NewS };
    [List] ->
      { List, NewS };
    _ ->
      List = elixir_tree_helpers:build_simple_reverse_list(int_line(Q), NewAcc),
      { ?ELIXIR_WRAP_CALL(int_line(Q), lists, append, [List]), NewS }
  end.

from_buffer_to_acc([], _Q, Acc, S) ->
  { Acc, S };

from_buffer_to_acc(Buffer, Q, Acc, S) ->
  { New, NewS } = elixir_tree_helpers:build_reverse_list(
    fun(X, AccS) -> do_quote(X, Q, AccS) end, Buffer, int_line(Q), S),
  { [New|Acc], NewS }.

line(Line, #elixir_quote{line=keep})  -> { integer, Line, Line };
line(_Line, #elixir_quote{line=Line}) -> Line.

int_line(Q) ->
  case line(0, Q) of
    { integer, _, Line } -> Line;
    _ -> 0
  end.