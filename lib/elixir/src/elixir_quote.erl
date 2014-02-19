-module(elixir_quote).
-export([escape/2, linify/2, linify/3, linify_with_context_counter/3, quote/4]).
-export([dot/6, tail_list/3, list/2]). %% Quote callbacks

-include("elixir.hrl").
-define(defs(Kind), Kind == def; Kind == defp; Kind == defmacro; Kind == defmacrop).
-define(lexical(Kind), Kind == import; Kind == alias; Kind == '__aliases__').
-compile({ inline, [keyfind/2, keystore/3, keydelete/2, keyreplace/3, keynew/3] }).

%% Apply the line from site call on quoted contents.
%% Receives a Key to look for the default line as argument.
linify(Line, Exprs) when is_integer(Line) ->
  do_linify(Line, line, nil, Exprs).

linify(Line, Key, Exprs) when is_integer(Line) ->
  do_linify(Line, Key, nil, Exprs).

%% Same as linify but also considers the context counter.
linify_with_context_counter(Line, Var, Exprs) when is_integer(Line) ->
  do_linify(Line, line, Var, Exprs).

do_linify(Line, Key, { Receiver, Counter } = Var, { Left, Meta, Receiver })
    when is_atom(Left), is_list(Meta), Left /= '_' ->
  do_tuple_linify(Line, Key, Var, keynew(counter, Meta, Counter), Left, Receiver);

do_linify(Line, Key, { _, Counter } = Var, { Lexical, [_|_] = Meta, [_|_] = Args }) when ?lexical(Lexical) ->
  do_tuple_linify(Line, Key, Var, keynew(counter, Meta, Counter), Lexical, Args);

do_linify(Line, Key, Var, { Left, Meta, Right }) when is_list(Meta) ->
  do_tuple_linify(Line, Key, Var, Meta, Left, Right);

do_linify(Line, Key, Var, { Left, Right }) ->
  { do_linify(Line, Key, Var, Left), do_linify(Line, Key, Var, Right) };

do_linify(Line, Key, Var, List) when is_list(List) ->
  [do_linify(Line, Key, Var, X) || X <- List];

do_linify(_, _, _, Else) -> Else.

do_tuple_linify(Line, Key, Var, Meta, Left, Right) ->
  { do_linify(Line, Key, Var, Left),
    do_linify_meta(Line, Key, Meta),
    do_linify(Line, Key, Var, Right) }.

do_linify_meta(0, line, Meta) ->
  Meta;
do_linify_meta(Line, line, Meta) ->
  case keyfind(line, Meta) of
    { line, Int } when is_integer(Int), Int /= 0 ->
      Meta;
    _ ->
      keystore(line, Meta, Line)
  end;
do_linify_meta(Line, Key, Meta) ->
  case keyfind(Key, Meta) of
    { Key, Int } when is_integer(Int), Int /= 0 ->
      keyreplace(Key, Meta, { line, Int });
    _ ->
      do_linify_meta(Line, line, Meta)
  end.

%% Some expressions cannot be unquoted at compilation time.
%% This function is responsible for doing runtime unquoting.
dot(Meta, Left, Right, Args, Context, File) ->
  annotate(dot(Meta, Left, Right, Args), Context, File).

dot(Meta, Left, { '__aliases__', _, Args }, nil) ->
  { '__aliases__', Meta, [Left|Args] };

dot(Meta, Left, Right, nil) when is_atom(Right) ->
  case atom_to_list(Right) of
    "Elixir." ++ _ ->
      { '__aliases__', Meta, [Left, Right] };
    _ ->
      { { '.', Meta, [Left, Right] }, Meta, [] }
  end;

dot(Meta, Left, { Right, _, Context }, nil) when is_atom(Right), is_atom(Context) ->
  { { '.', Meta, [Left, Right] }, Meta, [] };

dot(Meta, Left, { Right, _, Args }, nil) when is_atom(Right) ->
  { { '.', Meta, [Left, Right] }, Meta, Args };

dot(_Meta, _Left, Right, nil) ->
  argument_error(<<"expected unquote after dot to return an atom, an alias or a quoted call, got: ",
                   ('Elixir.Macro':to_string(Right))/binary>>);

dot(Meta, Left, Right, Args) when is_atom(Right) ->
  { { '.', Meta, [Left, Right] }, Meta, Args };

dot(Meta, Left, { Right, _, Context }, Args) when is_atom(Right), is_atom(Context) ->
  { { '.', Meta, [Left, Right] }, Meta, Args };

dot(_Meta, _Left, Right, _Args) ->
  argument_error(<<"expected unquote after dot with args to return an atom or a quoted call, got: ",
                   ('Elixir.Macro':to_string(Right))/binary>>).

list(Left, Right) when is_list(Right) ->
  validate_list(Left),
  Left ++ Right.

tail_list(Left, Right, Tail) when is_list(Right), is_list(Tail) ->
  validate_list(Left),
  Tail ++ Left ++ Right;

tail_list(Left, Right, Tail) when is_list(Left) ->
  validate_list(Left),
  [H|T] = lists:reverse(Tail ++ Left),
  lists:reverse([{ '|', [], [H, Right] }|T]).

validate_list(List) when is_list(List) ->
  ok;
validate_list(List) when not is_list(List) ->
  argument_error(<<"expected a list with quoted expressions in unquote_splicing/1, got: ",
                   ('Elixir.Kernel':inspect(List))/binary>>).

argument_error(Message) ->
  error('Elixir.ArgumentError':exception([{message,Message}])).

%% Annotates the AST with context and other info

annotate({ Def, Meta, [{ H, M, A }|T] }, Context, File) when ?defs(Def) ->
  %% Store the context information in the first element of the
  %% definition tuple so we can access it later on.
  MM = keystore(context, keystore(file, M, File), Context),
  { Def, Meta, [{ H, MM, A }|T] };
annotate({ { '.', _, [_, Def] } = Target, Meta, [{ H, M, A }|T] }, Context, File) when ?defs(Def) ->
  MM = keystore(context, keystore(file, M, File), Context),
  { Target, Meta, [{ H, MM, A }|T] };

annotate({ Lexical, Meta, [_|_] = Args }, Context, _File) when Lexical == import; Lexical == alias ->
  NewMeta = keystore(context, keydelete(counter, Meta), Context),
  { Lexical, NewMeta, Args };
annotate(Tree, _Context, _File) -> Tree.

%% Escapes the given expression. It is similar to quote, but
%% lines are kept and hygiene mechanisms are disabled.
escape(Expr, Unquote) ->
  { Res, Q } = quote(Expr, nil, #elixir_quote{
    line=true,
    keep=false,
    vars_hygiene=false,
    aliases_hygiene=false,
    imports_hygiene=false,
    unquote=Unquote,
    escape=true
  }, nil),
  { Res, Q#elixir_quote.unquoted }.

%% Quotes an expression and return its quoted Elixir AST.

quote(Expr, nil, Q, E) ->
  do_quote(Expr, Q, E);

quote(Expr, Binding, Q, E) ->
  Context = Q#elixir_quote.context,

  Vars = [ { '{}', [],
    [ '=', [], [
      { '{}', [], [K, [], Context] },
      V
    ] ]
  } || { K, V } <- Binding],

  { TExprs, TQ } = do_quote(Expr, Q, E),
  { { '{}',[], ['__block__',[], Vars ++ [TExprs] ] }, TQ }.

%% Actual quoting and helpers

do_quote({ quote, _, Args } = Tuple, #elixir_quote{unquote=true} = Q, E) when length(Args) == 1; length(Args) == 2 ->
  { TTuple, TQ } = do_quote_tuple(Tuple, Q#elixir_quote{unquote=false}, E),
  { TTuple, TQ#elixir_quote{unquote=true} };

do_quote({ unquote, _Meta, [Expr] }, #elixir_quote{unquote=true} = Q, _) ->
  { Expr, Q#elixir_quote{unquoted=true} };

%% Aliases

do_quote({ '__aliases__', Meta, [H|T] } = Alias, #elixir_quote{aliases_hygiene=true} = Q, E) when is_atom(H) and (H /= 'Elixir') ->
  Annotation = case elixir_aliases:expand(Alias, E#elixir_env.aliases,
                      E#elixir_env.macro_aliases, E#elixir_env.lexical_tracker) of
    Atom when is_atom(Atom) -> Atom;
    Aliases when is_list(Aliases) -> false
  end,
  AliasMeta = keystore(alias, keydelete(counter, Meta), Annotation),
  do_quote_tuple({ '__aliases__', AliasMeta, [H|T] }, Q, E);

%% Vars

do_quote({ Left, Meta, nil }, #elixir_quote{vars_hygiene=true} = Q, E) when is_atom(Left) ->
  do_quote_tuple({ Left, Meta, Q#elixir_quote.context }, Q, E);

%% Unquote

do_quote({ { { '.', Meta, [Left, unquote] }, _, [Expr] }, _, Args }, #elixir_quote{unquote=true} = Q, E) ->
  do_quote_call(Left, Meta, Expr, Args, Q, E);

do_quote({ { '.', Meta, [Left, unquote] }, _, [Expr] }, #elixir_quote{unquote=true} = Q, E) ->
  do_quote_call(Left, Meta, Expr, nil, Q, E);

%% Imports

do_quote({ '&', Meta, [{ '/', _, [{F, _, C}, A]}] = Args },
    #elixir_quote{imports_hygiene=true} = Q, E) when is_atom(F), is_integer(A), is_atom(C) ->
  do_quote_fa('&', Meta, Args, F, A, Q, E);

do_quote({ Name, Meta, ArgsOrAtom }, #elixir_quote{imports_hygiene=true} = Q, E) when is_atom(Name) ->
  Arity = case is_atom(ArgsOrAtom) of
    true  -> 0;
    false -> length(ArgsOrAtom)
  end,

  NewMeta = case (keyfind(import, Meta) == false) andalso
      elixir_dispatch:find_import(Meta, Name, Arity, E) of
    false ->
      case (Arity == 1) andalso keyfind(ambiguous_op, Meta) of
        { ambiguous_op, nil } -> keystore(ambiguous_op, Meta, Q#elixir_quote.context);
        _ -> Meta
      end;
    Receiver ->
      keystore(import, keystore(context, Meta, Q#elixir_quote.context), Receiver)
  end,

  Annotated = annotate({ Name, NewMeta, ArgsOrAtom }, Q#elixir_quote.context, file(E, Q)),
  do_quote_tuple(Annotated, Q, E);

do_quote({ _, _, _ } = Tuple, #elixir_quote{escape=false} = Q, E) ->
  Annotated = annotate(Tuple, Q#elixir_quote.context, file(E, Q)),
  do_quote_tuple(Annotated, Q, E);

%% Literals

do_quote({ Left, Right }, #elixir_quote{unquote=true} = Q, E) when
    is_tuple(Left)  andalso (element(1, Left) == unquote_splicing);
    is_tuple(Right) andalso (element(1, Right) == unquote_splicing) ->
  do_quote({ '{}', [], [Left, Right] }, Q, E);

do_quote({ Left, Right }, Q, E) ->
  { TLeft, LQ }  = do_quote(Left, Q, E),
  { TRight, RQ } = do_quote(Right, LQ, E),
  { { TLeft, TRight }, RQ };

do_quote(Map, #elixir_quote{escape=true} = Q, E) when is_map(Map) ->
  { TT, TQ } = do_quote(maps:to_list(Map), Q, E),
  { { '%{}', [], TT }, TQ };

do_quote(Tuple, #elixir_quote{escape=true} = Q, E) when is_tuple(Tuple) ->
  { TT, TQ } = do_quote(tuple_to_list(Tuple), Q, E),
  { { '{}', [], TT }, TQ };

do_quote(List, #elixir_quote{escape=true} = Q, E) when is_list(List) ->
  % The improper case is pretty inefficient, but improper lists are are.
  case reverse_improper(List) of
    { L }       -> do_splice(L, Q, E);
    { L, R }    ->
      { TL, QL } = do_splice(L, Q, E, [], []),
      { TR, QR } = do_quote(R, QL, E),
      { update_last(TL, fun(X) -> { '|', [], [X, TR] } end), QR }
  end;
do_quote(List, Q, E) when is_list(List) ->
    do_splice(lists:reverse(List), Q, E);

do_quote(Other, Q, _) ->
  { Other, Q }.

%% Quote helpers

do_quote_call(Left, Meta, Expr, Args, Q, E) ->
  All  = [meta(Meta, Q), Left, { unquote, Meta, [Expr] }, Args,
          Q#elixir_quote.context, file(E, Q)],
  { TAll, TQ } = lists:mapfoldl(fun(X, Acc) -> do_quote(X, Acc, E) end, Q, All),
  { { { '.', Meta, [elixir_quote, dot] }, Meta, TAll }, TQ }.

do_quote_fa(Target, Meta, Args, F, A, Q, E) ->
  NewMeta =
    case (keyfind(import_fa, Meta) == false) andalso
         elixir_dispatch:find_import(Meta, F, A, E) of
      false    -> Meta;
      Receiver -> keystore(import_fa, Meta, { Receiver, Q#elixir_quote.context })
    end,
  do_quote_tuple({ Target, NewMeta, Args }, Q, E).

do_quote_tuple({ Left, Meta, Right }, Q, E) ->
  { TLeft, LQ }  = do_quote(Left, Q, E),
  { TRight, RQ } = do_quote(Right, LQ, E),
  { { '{}', [], [TLeft, meta(Meta, Q), TRight] }, RQ }.

file(#elixir_env{file=File}, #elixir_quote{keep=true}) -> File;
file(_, _) -> nil.

meta(Meta, #elixir_quote{keep=true}) ->
  [case KV of { line, V } -> { keep, V }; _ -> KV end || KV <- Meta];
meta(Meta, #elixir_quote{line=true}) ->
  Meta;
meta(Meta, #elixir_quote{line=false}) ->
  keydelete(line, Meta);
meta(Meta, #elixir_quote{line=Line}) ->
  keystore(line, Meta, Line).

reverse_improper(L) -> reverse_improper(L, []).
reverse_improper([], Acc) -> { Acc };
reverse_improper([H|T], Acc) when is_list(T) -> reverse_improper(T, [H|Acc]);
reverse_improper([H|T], Acc) -> { [H|Acc], T }.

update_last([], _) -> [];
update_last([H], F) -> [F(H)];
update_last([H|T], F) -> [H|update_last(T,F)].

keyfind(Key, Meta) ->
  lists:keyfind(Key, 1, Meta).
keydelete(Key, Meta) ->
  lists:keydelete(Key, 1, Meta).
keystore(_Key, Meta, nil) ->
  Meta;
keystore(Key, Meta, Value) ->
  lists:keystore(Key, 1, Meta, { Key, Value }).
keyreplace(Key, Meta, { Key, _V }) ->
  Meta;
keyreplace(Key, Meta, Tuple) ->
  lists:keyreplace(Key, 1, Meta, Tuple).
keynew(Key, Meta, Value) ->
  case keyfind(Key, Meta) of
    { Key, _ } -> Meta;
    _ -> keystore(Key, Meta, Value)
  end.

%% Quote splicing

do_splice([{ '|', Meta, [{ unquote_splicing, _, [Left] }, Right] }|T], #elixir_quote{unquote=true} = Q, E) ->
  %% Process the remaining entries on the list.
  %% For [1, 2, 3, unquote_splicing(arg)|tail], this will quote
  %% 1, 2 and 3, which could even be unquotes.
  { TT, QT } = do_splice(T, Q, E, [], []),
  { TR, QR } = do_quote(Right, QT, E),
  { do_runtime_list(Meta, tail_list, [Left, TR, TT]), QR#elixir_quote{unquoted=true} };

do_splice(List, Q, E) ->
  do_splice(List, Q, E, [], []).

do_splice([{ unquote_splicing, Meta, [Expr] }|T], #elixir_quote{unquote=true} = Q, E, Buffer, Acc) ->
  do_splice(T, Q#elixir_quote{unquoted=true}, E, [], do_runtime_list(Meta, list, [Expr, do_join(Buffer, Acc)]));

do_splice([H|T], Q, E, Buffer, Acc) ->
  { TH, TQ } = do_quote(H, Q, E),
  do_splice(T, TQ, E, [TH|Buffer], Acc);

do_splice([], Q, _E, Buffer, Acc) ->
  { do_join(Buffer, Acc), Q }.

do_join(Left, [])    -> Left;
do_join([], Right)   -> Right;
do_join(Left, Right) -> { { '.', [], [erlang, '++'] }, [], [Left, Right] }.

do_runtime_list(Meta, Fun, Args) ->
  { { '.', Meta, [elixir_quote, Fun] }, Meta, Args }.
