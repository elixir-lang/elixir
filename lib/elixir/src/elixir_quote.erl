%% Implements Elixir quote.
-module(elixir_quote).
-export([escape/2, erl_escape/3, erl_quote/4, linify/2, unquote/6, join/5]).
-include("elixir.hrl").

%% Apply the line from site call on quoted contents.
linify(Line, Exprs) when is_integer(Line) ->
  do_linify(Line, Exprs).

do_linify(Line, { Left, Meta, Right }) ->
  NewMeta = case ?line(Meta) of
    0 -> keystore(line, Meta, Line);
    _ -> Meta
  end,
  { do_linify(Line, Left), NewMeta, do_linify(Line, Right) };

do_linify(Line, { Left, Right }) ->
  { do_linify(Line, Left), do_linify(Line, Right) };

do_linify(Line, List) when is_list(List) ->
  [do_linify(Line, X) || X <- List];

do_linify(_, Else) -> Else.

%% Some expressions cannot be unquoted at compilation time.
%% This function is responsible for doing runtime unquoting.
unquote(_File, _Line, Meta, Left, { '__aliases__', _, Args }, nil) ->
  { '__aliases__', Meta, [Left|Args] };

unquote(_File, _Line, Meta, Left, Right, nil) when is_atom(Right) ->
  case atom_to_list(Right) of
    "Elixir." ++ _ ->
      { '__aliases__', Meta, [Left, Right] };
    _ ->
      { { '.', Meta, [Left, Right] }, Meta, [] }
  end;

unquote(_File, _Line, Meta, Left, { Right, _, Context }, nil) when is_atom(Right), is_atom(Context) ->
  { { '.', Meta, [Left, Right] }, Meta, [] };

unquote(_File, _Line, Meta, Left, { Right, _, Args }, nil) when is_atom(Right) ->
  { { '.', Meta, [Left, Right] }, Meta, Args };

unquote(File, Line, _Meta, _Left, Right, nil) ->
  elixir_errors:syntax_error(Line, File, "expected unquote after dot to return an atom, "
    "an alias or a quoted call, got: ~ts", ['Elixir.Macro':to_string(Right)]);

unquote(_File, _Line, Meta, Left, Right, Args) when is_atom(Right) ->
  { { '.', Meta, [Left, Right] }, Meta, Args };

unquote(_File, _Line, Meta, Left, { Right, _, Context }, Args) when is_atom(Right), is_atom(Context) ->
  { { '.', Meta, [Left, Right] }, Meta, Args };

unquote(File, Line, _Meta, _Left, Right, _Args) ->
  elixir_errors:syntax_error(Line, File, "expected unquote after dot with args to return an atom "
    "or a quoted call, got: ~ts", ['Elixir.Macro':to_string(Right)]).

join(_File, _Line, Left, Right, Rest) when is_list(Left), is_list(Right), is_list(Rest) ->
  Rest ++ Left ++ Right;

join(_File, _Line, Left, Right, Rest)  ->
  [H|T] = lists:reverse(Rest ++ Left),
  lists:reverse([{ '|', [], [H, Right] }|T]).

%% Escapes the given expression. It is similar to quote, but
%% lines are kept and hygiene mechanisms are disabled.
escape(Expr, Unquote) ->
  quote(Expr, nil, #elixir_quote{
    line=keep,
    vars_hygiene=false,
    aliases_hygiene=false,
    imports_hygiene=false,
    unquote=Unquote,
    escape=true
  }, nil).

erl_escape(Expr, Unquote, S) ->
  { QExpr, TQ } = escape(Expr, Unquote),
  { TExpr, TS } = elixir_translator:translate_each(QExpr, S),
  { TExpr, TQ, TS }.

%% Quotes an expression and return its quoted Elixir AST.

quote(Expr, nil, Q, S) ->
  do_quote(Expr, Q, S);

quote(Expr, Binding, Q, S) ->
  Context = Q#elixir_quote.context,

  Vars = [ { '{}', [],
    [ '=', [], [
      { '{}', [], [K, [], Context] },
      V
    ] ]
  } || { K, V } <- Binding],

  { TExprs, TQ } = do_quote(Expr, Q, S),
  { { '{}',[], ['__block__',[], Vars ++ [TExprs] ] }, TQ }.

erl_quote(Expr, Binding, Q, S) ->
  { QExpr, TQ } = quote(Expr, Binding, Q, S),
  { TExpr, TS } = elixir_translator:translate_each(QExpr, S),
  { TExpr, TQ, TS }.

%% Actual quoting and helpers

do_quote({ quote, _, Args } = Tuple, #elixir_quote{unquote=true} = Q, S) when length(Args) == 1; length(Args) == 2 ->
  { TTuple, TQ } = do_quote_tuple(Tuple, Q#elixir_quote{unquote=false}, S),
  { TTuple, TQ#elixir_quote{unquote=true} };

do_quote({ unquote, _Meta, [Expr] }, #elixir_quote{unquote=true} = Q, _) ->
  { Expr, Q#elixir_quote{unquoted=true} };

%% Context mark

do_quote({ Def, Meta, Args }, #elixir_quote{escape=false} = Q, S) when ?defs(Def); Def == defmodule; Def == alias; Def == import ->
  NewMeta = keystore(context, Meta, Q#elixir_quote.context),
  do_quote_tuple({ Def, NewMeta, Args }, Q, S);

do_quote({ { '.', _, [_, Def] } = Target, Meta, Args }, #elixir_quote{escape=false} = Q, S) when ?defs(Def); Def == defmodule; Def == alias; Def == import ->
  NewMeta = keystore(context, Meta, Q#elixir_quote.context),
  do_quote_tuple({ Target, NewMeta, Args }, Q, S);

%% Aliases

do_quote({ 'alias!', _Meta, [Expr] }, #elixir_quote{aliases_hygiene=true} = Q, S) ->
  { TExpr, TQ } = do_quote(Expr, Q#elixir_quote{aliases_hygiene=false}, S),
  { TExpr, TQ#elixir_quote{aliases_hygiene=true} };

do_quote({ '__aliases__', Meta, [H|T] } = Alias, #elixir_quote{aliases_hygiene=true} = Q, S) when is_atom(H) and (H /= 'Elixir') ->
  Annotation = case elixir_aliases:expand(Alias, S#elixir_scope.aliases, S#elixir_scope.macro_aliases) of
    Atom when is_atom(Atom) -> Atom;
    Aliases when is_list(Aliases) -> false
  end,
  AliasMeta = keystore(alias, Meta, Annotation),
  do_quote_tuple({ '__aliases__', AliasMeta, [H|T] }, Q, S);

%% Vars

do_quote({ Left, Meta, nil }, #elixir_quote{vars_hygiene=true} = Q, S) when is_atom(Left) ->
  do_quote_tuple({ Left, Meta, Q#elixir_quote.context }, Q, S);

%% Unquote

do_quote({ { { '.', Meta, [Left, unquote] }, _, [Expr] }, _, Args }, #elixir_quote{unquote=true} = Q, S) ->
  do_quote_call(Left, Meta, Expr, Args, Q, S);

do_quote({ { '.', Meta, [Left, unquote] }, _, [Expr] }, #elixir_quote{unquote=true} = Q, S) ->
  do_quote_call(Left, Meta, Expr, nil, Q, S);

%% Imports

do_quote({ '&', Meta, [{ '/', _, [{F, _, C}, A]}] = Args },
    #elixir_quote{imports_hygiene=true} = Q, S) when is_atom(F), is_integer(A), is_atom(C) ->
  do_quote_fa('&', Meta, Args, F, A, Q, S);

do_quote({ function, Meta, [{ '/', _, [{F, _, C}, A]}] = Args },
    #elixir_quote{imports_hygiene=true} = Q, S) when is_atom(F), is_integer(A), is_atom(C) ->
  do_quote_fa(function, Meta, Args, F, A, Q, S);

do_quote({ { '.', _, [_, function] } = Target, Meta, [{ '/', _, [{F, _, C}, A]}] = Args },
    #elixir_quote{imports_hygiene=true} = Q, S) when is_atom(F), is_integer(A), is_atom(C) ->
  do_quote_fa(Target, Meta, Args, F, A, Q, S);

do_quote({ Name, Meta, ArgsOrAtom }, #elixir_quote{imports_hygiene=true} = Q, S) when is_atom(Name) ->
  Arity = case is_atom(ArgsOrAtom) of
    true  -> 0;
    false -> length(ArgsOrAtom)
  end,

  case (keyfind(import, Meta) == false) andalso
      elixir_dispatch:find_import(Meta, Name, Arity, S) of
    false ->
      AmbMeta =
        case (Arity == 1) andalso keyfind(ambiguous_op, Meta) of
          { ambiguous_op, nil } -> keystore(ambiguous_op, Meta, Q#elixir_quote.context);
          _ -> Meta
        end,
      do_quote_tuple({ Name, AmbMeta, ArgsOrAtom }, Q, S);
    Receiver ->
      ImportMeta = keystore(import,
        keystore(context, Meta, Q#elixir_quote.context),
        Receiver),
      do_quote_tuple({ Name, ImportMeta, ArgsOrAtom }, Q, S)
  end;

do_quote({ _, _, _ } = Tuple, #elixir_quote{escape=false} = Q, S) ->
  do_quote_tuple(Tuple, Q, S);

%% Literals

do_quote({ Left, Right }, #elixir_quote{unquote=true} = Q, S) when
    is_tuple(Left)  andalso (element(1, Left) == unquote_splicing);
    is_tuple(Right) andalso (element(1, Right) == unquote_splicing) ->
  do_quote({ '{}', [], [Left, Right] }, Q, S);

do_quote({ Left, Right }, Q, S) ->
  { TLeft, LQ }  = do_quote(Left, Q, S),
  { TRight, RQ } = do_quote(Right, LQ, S),
  { { TLeft, TRight }, RQ };

do_quote(Tuple, #elixir_quote{escape=true} = Q, S) when is_tuple(Tuple) ->
  { TT, TQ } = do_quote(tuple_to_list(Tuple), Q, S),
  { { '{}', [], TT }, TQ };

do_quote(List, Q, S) when is_list(List) ->
  do_splice(lists:reverse(List), Q, S);

do_quote(Other, Q, _) ->
  { Other, Q }.

%% Quote helpers

do_quote_call(Left, Meta, Expr, Args, Q, S) ->
  All  = [?line(Meta), meta(Meta, Q), Left, { unquote, Meta, [Expr] }, Args],
  { TAll, TQ } = lists:mapfoldl(fun(X, Acc) -> do_quote(X, Acc, S) end, Q, All),
  { { { '.', Meta, [elixir_quote, unquote] }, Meta, [{ '__FILE__', [], nil }|TAll] }, TQ }.

do_quote_fa(Target, Meta, Args, F, A, Q, S) ->
  NewMeta =
    case (keyfind(import_fa, Meta) == false) andalso
         elixir_dispatch:find_import(Meta, F, A, S) of
      false    -> Meta;
      Receiver -> keystore(import_fa, Meta, { Receiver, Q#elixir_quote.context })
    end,
  do_quote_tuple({ Target, NewMeta, Args }, Q, S).

do_quote_tuple({ Left, Meta, Right }, Q, S) ->
  { TLeft, LQ }  = do_quote(Left, Q, S),
  { TRight, RQ } = do_quote(Right, LQ, S),
  { { '{}', [], [TLeft, meta(Meta, Q), TRight] }, RQ }.

meta(Meta, #elixir_quote{line=keep}) ->
  Meta;
meta(Meta, #elixir_quote{line=nil}) ->
  keydelete(line, Meta);
meta(Meta, #elixir_quote{line=Line}) ->
  keystore(line, Meta, Line).

keyfind(Key, Meta) ->
  lists:keyfind(Key, 1, Meta).
keydelete(Key, Meta) ->
  lists:keydelete(Key, 1, Meta).
keystore(Key, Meta, Value) ->
  lists:keystore(Key, 1, Meta, { Key, Value }).

%% Quote splicing

do_splice([{ '|', Meta, [{ unquote_splicing, _, [Left] }, Right] }|T], #elixir_quote{unquote=true} = Q, S) ->
  %% Process the remaining entries on the list.
  %% For [1, 2, 3, unquote_splicing(arg)|tail], this will quote
  %% 1, 2 and 3, which could even be unquotes.
  { TT, QT } = do_splice(T, Q, S, [], []),
  { TR, QR } = do_quote(Right, QT, S),

  %% Do the joining at runtime when we are aware of the values.
  Args = [{ '__FILE__', [], nil }, ?line(Meta), Left, TR, TT],
  { { { '.', Meta, [elixir_quote, join] }, Meta, Args }, QR#elixir_quote{unquoted=true} };

do_splice(List, Q, S) ->
  do_splice(List, Q, S, [], []).

do_splice([{ unquote_splicing, _, [Expr] }|T], #elixir_quote{unquote=true} = Q, S, Buffer, Acc) ->
  do_splice(T, Q#elixir_quote{unquoted=true}, S, [], do_splice_join(do_splice_join(Expr, Buffer), Acc));

do_splice([H|T], Q, S, Buffer, Acc) ->
  { TH, TQ } = do_quote(H, Q, S),
  do_splice(T, TQ, S, [TH|Buffer], Acc);

do_splice([], Q, _S, Buffer, Acc) ->
  { do_splice_join(Buffer, Acc), Q }.

do_splice_join(Left, [])    -> Left;
do_splice_join([], Right)   -> Right;
do_splice_join(Left, Right) -> { { '.', [], ['Elixir.Kernel', '++'] }, [], [Left, Right] }.
