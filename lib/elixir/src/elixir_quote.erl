-module(elixir_quote).
-export([quote/3, linify/2, unquote/5, user_quote/2]).
-include("elixir.hrl").

%% Apply the line from site call on quoted contents.
linify(Line, Exprs) when is_integer(Line) ->
  do_linify(Line, Exprs).

do_linify(Line, { Left, Meta, Right }) ->
  NewMeta = case ?line(Meta) of
    0 -> lists:keystore(line, 1, Meta, { line, Line });
    _ -> Meta
  end,
  { do_linify(Line, Left), NewMeta, do_linify(Line, Right) };

do_linify(Line, { Left, Right }) ->
  { do_linify(Line, Left), do_linify(Line, Right) };

do_linify(Line, List) when is_list(List) ->
  [do_linify(Line, X) || X <- List];

do_linify(_, Else) -> Else.

%% Some expressions cannot be unquoted at compilation time.
%% This function is responisble for doing runtime unquoting.
unquote(Meta, Left, { '__aliases__', _, Args }, nil, _File) ->
  { '__aliases__', Meta, [Left|Args] };

unquote(Meta, Left, Right, nil, _File) when is_atom(Right) ->
  case atom_to_list(Right) of
    "Elixir-" ++ _ ->
      { '__aliases__', Meta, [Left, Right] };
    _ ->
      { { '.', Meta, [Left, Right] }, Meta, [] }
  end;

unquote(Meta, Left, Right, Args, _File) when is_atom(Right) ->
  { { '.', Meta, [Left, Right] }, Meta, Args };

unquote(Meta, _Left, _Right, _Args, File) ->
  elixir_errors:syntax_error(Meta, File, "expected unquote after dot to return an atom or an alias").

%% Similar to quote, but the given code is meant to come directly from the user.
%% Basically, lines are kept and hygiene mechanisms are disabled.
user_quote(Expr, S) ->
  quote(Expr, #elixir_quote{
    line=keep,
    vars_hygiene=nil,
    aliases_hygiene=false,
    imports_hygiene=false,
    unquote=true
  }, S).

%% Quotes an expression into Erlang's AST

quote({ 'unquote_splicing', Meta, [_] } = Expr, #elixir_quote{unquote=true} = Q, S) ->
  do_quote({ '__block__', Meta, [Expr] }, Q, S);

quote(Else, Q, S) ->
  do_quote(Else, Q, S).

do_quote({ quote, _, Args } = Tuple, #elixir_quote{unquote=true} = Q, S) when length(Args) == 1; length(Args) == 2 ->
  do_quote_tuple(Tuple, Q#elixir_quote{unquote=false}, S);

do_quote({ unquote, _Meta, [Expr] }, #elixir_quote{unquote=true}, S) ->
  elixir_translator:translate_each(Expr, S);

do_quote({ 'alias!', _Meta, [Expr] }, Q, S) ->
  do_quote(Expr, Q#elixir_quote{aliases_hygiene=false}, S);

do_quote({ '__aliases__', Meta, [H|T] } = Alias, #elixir_quote{aliases_hygiene=true} = Q, S) when is_atom(H) and (H /= 'Elixir') ->
  Annotation = case elixir_aliases:expand(Alias, S#elixir_scope.aliases, S#elixir_scope.macro_aliases) of
    Atom when is_atom(Atom) -> Atom;
    Aliases when is_list(Aliases) -> false
  end,

  Line = ?line(Meta),
  { TAliases, SA } = do_quote([H|T], Q, S),

  { { tuple, Line, [
    { atom, Line, '__aliases__' },
    meta([{alias,Annotation}|Meta], Q),
    TAliases
  ] }, SA };

do_quote({ { { '.', Meta, [Left, unquote] }, _, [Expr] }, _, Args }, #elixir_quote{unquote=true} = Q, S) ->
  All  = [Left, { unquote, Meta, [Expr] }, Args, S#elixir_scope.file],
  { TAll, TS } = lists:mapfoldl(fun(X, Acc) -> do_quote(X, Q, Acc) end, S, All),
  { ?wrap_call(?line(Meta), elixir_quote, unquote, [meta(Meta, Q)|TAll]), TS };

do_quote({ { '.', Meta, [Left, unquote] }, _, [Expr] }, #elixir_quote{unquote=true} = Q, S) ->
  All = [Left, { unquote, Meta, [Expr] }, nil, S#elixir_scope.file],
  { TAll, TS } = lists:mapfoldl(fun(X, Acc) -> do_quote(X, Q, Acc) end, S, All),
  { ?wrap_call(?line(Meta), elixir_quote, unquote, [meta(Meta, Q)|TAll]), TS };

do_quote({ Left, Meta, nil }, Q, S) when is_atom(Left) ->
  Line  = ?line(Meta),
  Tuple = { tuple, Line, [
    { atom, Line, Left },
    meta(Meta, Q),
    { atom, Line, Q#elixir_quote.vars_hygiene }
  ] },
  { Tuple, S };

do_quote({ Name, Meta, ArgsOrAtom } = Tuple, #elixir_quote{imports_hygiene=true} = Q, S) when is_atom(Name) ->
  Arity = case is_atom(ArgsOrAtom) of
    true  -> 0;
    false -> length(ArgsOrAtom)
  end,

  case (lists:keyfind(import, 1, Meta) == false) andalso
      elixir_dispatch:find_import(Meta, Name, Arity, S) of
    false    -> do_quote_tuple(Tuple, Q, S);
    Receiver -> do_quote_tuple({ Name, [{import,Receiver}|Meta], ArgsOrAtom }, Q, S)
  end;

do_quote({ _, _, _ } = Tuple, Q, S) ->
  do_quote_tuple(Tuple, Q, S);

% Handle two item tuples but still allow them to be spliced.
do_quote({ Left, Right }, Q, S) when
  not is_tuple(Left)  orelse (element(1, Left) /= unquote_splicing),
  not is_tuple(Right) orelse (element(1, Right) /= unquote_splicing) ->
  { TLeft, LS } = do_quote(Left, Q, S),
  { TRight, RS } = do_quote(Right, Q, LS),
  { { tuple, line(Q), [TLeft, TRight] }, RS };

do_quote({ Left, Right }, Q, S) ->
  do_quote({ '{}', [{line,line(Q)}], [Left, Right] }, Q, S);

do_quote(List, Q, S) when is_list(List) ->
  splice(List, Q, [], [], S);

do_quote(Number, Q, S) when is_integer(Number) ->
  { { integer, line(Q), Number }, S };

do_quote(Number, Q, S) when is_float(Number) ->
  { { float, line(Q), Number }, S };

do_quote(Atom, Q, S) when is_atom(Atom) ->
  { { atom, line(Q), Atom }, S };

do_quote(Bitstring, _Q, S) when is_bitstring(Bitstring) ->
  { elixir_tree_helpers:abstract_syntax(Bitstring), S }.

do_quote_tuple({ Left, Meta, Right }, Q, S) ->
  { TLeft, LS } = do_quote(Left, Q, S),
  { TRight, RS } = do_quote(Right, Q, LS),

  %% We need to remove line numbers from quoted exprs
  %% otherwise the line number quotes in the macro will
  %% get mixed with the unquoted contents
  Tuple = { tuple, ?line(Meta), [TLeft, meta(Meta, Q), TRight] },
  { Tuple, RS }.

% Loop through the list finding each unquote_splicing entry.

splice([{ unquote_splicing, _, [Args] }|T], #elixir_quote{unquote=true} = Q, Buffer, Acc, S) ->
  { NewAcc, NewS } = from_buffer_to_acc(Buffer, Q, Acc, S),
  { TArgs, TS } = elixir_translator:translate_each(Args, NewS),
  splice(T, Q, [], [TArgs|NewAcc], TS);

splice([{ '|', Meta, [{ unquote_splicing, _, [_] } = Left, Right] }], #elixir_quote{unquote=true} = Q, Buffer, Acc, S) ->
  { TLeft, SL }  = splice([Left], Q, Buffer, Acc, S),
  { TRight, SR } = do_quote(Right, Q, SL),
  { { op, ?line(Meta), '++', TLeft, TRight }, SR };

splice([H|T], Q, Buffer, Acc, S) ->
  splice(T, Q, [H|Buffer], Acc, S);

splice([], Q, Buffer, Acc, S) ->
  { NewAcc, NewS } = from_buffer_to_acc(Buffer, Q, Acc, S),
  case NewAcc of
    [] ->
      { { nil, line(Q) }, NewS };
    [List] ->
      { List, NewS };
    _ ->
      List = elixir_tree_helpers:build_simple_reverse_list(line(Q), NewAcc),
      { ?wrap_call(line(Q), lists, append, [List]), NewS }
  end.

from_buffer_to_acc([], _Q, Acc, S) ->
  { Acc, S };

from_buffer_to_acc(Buffer, Q, Acc, S) ->
  { New, NewS } = elixir_tree_helpers:build_reverse_list(
    fun(X, AccS) -> do_quote(X, Q, AccS) end, Buffer, line(Q), S),
  { [New|Acc], NewS }.

meta(Meta, #elixir_quote{line=keep}) ->
  elixir_tree_helpers:abstract_syntax(Meta);
meta(Meta, #elixir_quote{line=nil}) ->
  elixir_tree_helpers:abstract_syntax(lists:keydelete(line, 1, Meta));
meta(Meta, #elixir_quote{line=Line} = Q) ->
  { cons,
    line(Q),
    { tuple, line(Q), [{ atom, line(Q), line }, Line] },
    elixir_tree_helpers:abstract_syntax(lists:keydelete(line, 1, Meta))
  }.

line(#elixir_quote{line={ integer, _, Line }}) -> Line;
line(_) -> 0.