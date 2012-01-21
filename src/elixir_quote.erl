-module(elixir_quote).
-export([quote/2, linify/2]).
-include("elixir.hrl").

%% Add lines to quoted contents

linify(Line, Forms) when is_list(Forms) ->
  [linify(Line, X) || X <- Forms];

linify(Line, { Left, 0, Right }) ->
  LLeft  = linify(Line, Left),
  LRight = linify(Line, Right),
  { LLeft, Line, LRight };

linify(Line, Tuple) when is_tuple(Tuple) ->
  list_to_tuple(linify(Line, tuple_to_list(Tuple)));

linify(_Line, Else) -> Else.

%% Quoting

quote({ unquote, _Line, [Expr] }, S) ->
  elixir_translator:translate_each(Expr, S);

quote({ Left, Line, Right }, S) when is_atom(Left), is_atom(Right) -> %% Variables
  Tuple = { tuple, Line, [{ atom, Line, Left }, { integer, Line, 0 }, { atom, Line, quoted }] },
  { Tuple, S };

quote({ Left, Line, Right }, S) ->
  { TLeft, LS } = quote(Left, S),
  { TRight, RS } = quote(Right, LS),

  % We need to remove line numbers from quoted exprs otherwise
  % the line number quotes in the macro will get mixed with the
  % original exprs line numbers given to the macro as arguments.
  Tuple = { tuple, Line, [TLeft, { integer, Line, 0 }, TRight] },
  { Tuple, RS };

quote({ Left, Right }, S) ->
  { TLeft, LS } = quote(Left, S),
  { TRight, RS } = quote(Right, LS),
  { { tuple, 0, [TLeft, TRight] }, RS };

quote(List, S) when is_list(List) ->
  splice(List, [], [], S);

quote(Number, S) when is_integer(Number) ->
  { { integer, 0, Number }, S };

quote(Number, S) when is_float(Number) ->
  { { float, 0, Number }, S };

quote(Atom, S) when is_atom(Atom) ->
  { { atom, 0, Atom }, S };

quote(Bitstring, S) when is_bitstring(Bitstring) ->
  { elixir_tree_helpers:abstract_syntax(Bitstring), S }.

% Loop through the list finding each unquote_splicing entry.

splice([{ unquote_splicing, _, [Args] }|T], Buffer, Acc, S) ->
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
  { New, NewS } = elixir_tree_helpers:build_reverse_list(fun quote/2, Buffer, 0, S),
  { [New|Acc], NewS }.