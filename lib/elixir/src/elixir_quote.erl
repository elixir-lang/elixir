-module(elixir_quote).
-export([escape/3, linify/3, linify_with_context_counter/3, quote/5, has_unquotes/1]).
-export([dot/5, tail_list/3, list/2]). %% Quote callbacks

-include("elixir.hrl").
-define(defs(Kind), Kind == def; Kind == defp; Kind == defmacro; Kind == defmacrop; Kind == '@').
-define(lexical(Kind), Kind == import; Kind == alias; Kind == require).
-compile({inline, [keyfind/2, keystore/3, keydelete/2, keynew/3, do_tuple_linify/5]}).

%% Apply the line from site call on quoted contents.
%% Receives a Key to look for the default line as argument.
linify(0, _Key, Exprs) ->
  Exprs;
linify(Line, Key, Exprs) when is_integer(Line) ->
  LinifyMeta = linify_meta(Line, Key),
  do_linify(LinifyMeta, Exprs, nil).

%% Same as linify but also considers the context counter.
linify_with_context_counter(Line, Var, Exprs) when is_integer(Line) ->
  LinifyMeta = linify_meta(Line, line),
  do_linify(LinifyMeta, Exprs, Var).

do_linify(LinifyMeta, {quote, Meta, [_ | _] = Args}, {Receiver, Counter} = Var)
    when is_list(Meta) ->
  NewMeta =
    case keyfind(context, Meta) == {context, Receiver} of
      true -> keynew(counter, Meta, Counter);
      false -> Meta
    end,
  do_tuple_linify(LinifyMeta, NewMeta, quote, Args, Var);

do_linify(LinifyMeta, {Left, Meta, Receiver}, {Receiver, Counter} = Var)
    when is_atom(Left), is_list(Meta), Left /= '_' ->
  do_tuple_linify(LinifyMeta, keynew(counter, Meta, Counter), Left, Receiver, Var);

do_linify(LinifyMeta, {Lexical, Meta, [_ | _] = Args}, {_, Counter} = Var)
    when ?lexical(Lexical); Lexical == '__aliases__' ->
  do_tuple_linify(LinifyMeta, keynew(counter, Meta, Counter), Lexical, Args, Var);

do_linify(LinifyMeta, {Left, Meta, Right}, Var) when is_list(Meta) ->
  do_tuple_linify(LinifyMeta, Meta, Left, Right, Var);

do_linify(LinifyMeta, {Left, Right}, Var) ->
  {do_linify(LinifyMeta, Left, Var), do_linify(LinifyMeta, Right, Var)};

do_linify(LinifyMeta, List, Var) when is_list(List) ->
  [do_linify(LinifyMeta, X, Var) || X <- List];

do_linify(_, Else, _) -> Else.

do_tuple_linify(LinifyMeta, Meta, Left, Right, Var) ->
  {do_linify(LinifyMeta, Left, Var), LinifyMeta(Meta), do_linify(LinifyMeta, Right, Var)}.

linify_meta(0, line) -> fun(Meta) -> Meta end;
linify_meta(Line, line) -> fun(Meta) -> keynew(line, Meta, Line) end;
linify_meta(Line, keep) ->
  fun(Meta) ->
    case lists:keytake(keep, 1, Meta) of
      {value, {keep, {_, Int}}, MetaNoFile} ->
        [{line, Int} | keydelete(line, MetaNoFile)];
      _ ->
        keynew(line, Meta, Line)
    end
  end.

%% Some expressions cannot be unquoted at compilation time.
%% This function is responsible for doing runtime unquoting.
dot(Meta, Left, Right, Args, Context) ->
  annotate(dot(Meta, Left, Right, Args), Context).

dot(Meta, Left, {'__aliases__', _, Args}, nil) ->
  {'__aliases__', Meta, [Left | Args]};

dot(Meta, Left, Right, nil) when is_atom(Right) ->
  case atom_to_list(Right) of
    "Elixir." ++ _ ->
      {'__aliases__', Meta, [Left, Right]};
    _ ->
      {{'.', Meta, [Left, Right]}, Meta, []}
  end;

dot(Meta, Left, {Right, _, Context}, nil) when is_atom(Right), is_atom(Context) ->
  {{'.', Meta, [Left, Right]}, Meta, []};

dot(Meta, Left, {Right, _, Args}, nil) when is_atom(Right) ->
  {{'.', Meta, [Left, Right]}, Meta, Args};

dot(_Meta, _Left, Right, nil) ->
  argument_error(<<"expected unquote after dot to return an atom, an alias or a quoted call, got: ",
                   ('Elixir.Macro':to_string(Right))/binary>>);

dot(Meta, Left, Right, Args) when is_atom(Right) ->
  {{'.', Meta, [Left, Right]}, Meta, Args};

dot(Meta, Left, {Right, _, Context}, Args) when is_atom(Right), is_atom(Context) ->
  {{'.', Meta, [Left, Right]}, Meta, Args};

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
  [H | T] = lists:reverse(Tail ++ Left),
  lists:reverse([{'|', [], [H, Right]} | T]).

validate_list(List) when is_list(List) ->
  ok;
validate_list(List) when not is_list(List) ->
  argument_error(<<"expected a list with quoted expressions in unquote_splicing/1, got: ",
                   ('Elixir.Kernel':inspect(List))/binary>>).

argument_error(Message) ->
  error('Elixir.ArgumentError':exception([{message, Message}])).

%% Annotates the AST with context and other info.
%%
%% Note we need to delete the counter because linify
%% adds the counter recursively, even inside quoted
%% expressions, so we need to clean up the forms to
%% allow them to get a new counter on the next expansion.

annotate({Def, Meta, [{H, M, A} | T]}, Context) when ?defs(Def) ->
  {Def, Meta, [{H, keystore(context, M, Context), A} | T]};
annotate({{'.', _, [_, Def]} = Target, Meta, [{H, M, A} | T]}, Context) when ?defs(Def) ->
  {Target, Meta, [{H, keystore(context, M, Context), A} | T]};

annotate({Lexical, Meta, [_ | _] = Args}, Context) when ?lexical(Lexical) ->
  NewMeta = keystore(context, keydelete(counter, Meta), Context),
  {Lexical, NewMeta, Args};
annotate(Tree, _Context) -> Tree.

has_unquotes({unquote, _, [_]}) -> true;
has_unquotes({unquote_splicing, _, [_]}) -> true;
has_unquotes({{'.', _, [_, unquote]}, _, [_]}) -> true;
has_unquotes({Var, _, Ctx}) when is_atom(Var), is_atom(Ctx) -> false;
has_unquotes({Name, _, Args}) when is_list(Args) ->
  has_unquotes(Name) orelse lists:any(fun has_unquotes/1, Args);
has_unquotes({Left, Right}) ->
  has_unquotes(Left) orelse has_unquotes(Right);
has_unquotes(List) when is_list(List) ->
  lists:any(fun has_unquotes/1, List);
has_unquotes(_Other) -> false.

%% Escapes the given expression. It is similar to quote, but
%% lines are kept and hygiene mechanisms are disabled.
escape(Expr, Kind, Unquote) ->
  do_quote(Expr, #elixir_quote{
    line=true,
    file=nil,
    vars_hygiene=false,
    aliases_hygiene=false,
    imports_hygiene=false,
    unquote=Unquote
  }, Kind).

%% Quotes an expression and return its quoted Elixir AST.

quote(_Meta, {unquote_splicing, _, [_]}, _Binding, #elixir_quote{unquote=true}, _) ->
  argument_error(<<"unquote_splicing only works inside arguments and block contexts, "
    "wrap it in parens if you want it to work with one-liners">>);

quote(_Meta, Expr, nil, Q, E) ->
  do_quote(Expr, Q, E);

quote(Meta, Expr, Binding, Q, E) ->
  Context = Q#elixir_quote.context,
  VarMeta = [Pair || {K, _} = Pair <- Meta, K == counter],

  Vars = [ {'{}', [],
    [ '=', [], [
      {'{}', [], [K, VarMeta, Context]},
      V
    ] ]
  } || {K, V} <- Binding],

  TExprs = do_quote(Expr, Q, E),
  {'{}', [], ['__block__', [], Vars ++ [TExprs]]}.

%% Actual quoting and helpers

do_quote({quote, Meta, [Arg]}, Q, E) ->
  TArg = do_quote(Arg, Q#elixir_quote{unquote=false}, E),

  NewMeta = case Q of
    #elixir_quote{vars_hygiene=true, context=Context} -> keystore(context, Meta, Context);
    _ -> Meta
  end,

  {'{}', [], [quote, meta(NewMeta, Q), [TArg]]};

do_quote({quote, Meta, [Opts, Arg]}, Q, E) ->
  TOpts = do_quote(Opts, Q, E),
  TArg = do_quote(Arg, Q#elixir_quote{unquote=false}, E),

  NewMeta = case Q of
    #elixir_quote{vars_hygiene=true, context=Context} -> keystore(context, Meta, Context);
    _ -> Meta
  end,

  {'{}', [], [quote, meta(NewMeta, Q), [TOpts, TArg]]};

do_quote({unquote, _Meta, [Expr]}, #elixir_quote{unquote=true}, _) ->
  Expr;

%% Aliases

do_quote({'__aliases__', Meta, [H | T]} = Alias, #elixir_quote{aliases_hygiene=true} = Q, E) when is_atom(H) and (H /= 'Elixir') ->
  Annotation =
    case elixir_aliases:expand(Alias, ?key(E, aliases), ?key(E, macro_aliases), ?key(E, lexical_tracker)) of
      Atom when is_atom(Atom) -> Atom;
      Aliases when is_list(Aliases) -> false
    end,
  AliasMeta = keystore(alias, keydelete(counter, Meta), Annotation),
  do_quote_tuple('__aliases__', AliasMeta, [H | T], Q, E);

%% Vars

do_quote({Left, Meta, nil}, #elixir_quote{vars_hygiene=true, imports_hygiene=true} = Q, E) when is_atom(Left) ->
  do_quote_import(Left, Meta, Q#elixir_quote.context, Q, E);

do_quote({Left, Meta, nil}, #elixir_quote{vars_hygiene=true} = Q, E) when is_atom(Left) ->
  do_quote_tuple(Left, Meta, Q#elixir_quote.context, Q, E);

%% Unquote

do_quote({{{'.', Meta, [Left, unquote]}, _, [Expr]}, _, Args}, #elixir_quote{unquote=true} = Q, E) ->
  do_quote_call(Left, Meta, Expr, Args, Q, E);

do_quote({{'.', Meta, [Left, unquote]}, _, [Expr]}, #elixir_quote{unquote=true} = Q, E) ->
  do_quote_call(Left, Meta, Expr, nil, Q, E);

%% Imports

do_quote({'&', Meta, [{'/', _, [{F, _, C}, A]}] = Args},
         #elixir_quote{imports_hygiene=true} = Q, E) when is_atom(F), is_integer(A), is_atom(C) ->
  do_quote_fa('&', Meta, Args, F, A, Q, E);

do_quote({Name, Meta, ArgsOrAtom}, #elixir_quote{imports_hygiene=true} = Q, E) when is_atom(Name) ->
  do_quote_import(Name, Meta, ArgsOrAtom, Q, E);

%% Two-element tuples

do_quote({Left, Right}, #elixir_quote{unquote=true} = Q, E) when
    is_tuple(Left)  andalso (element(1, Left) == unquote_splicing);
    is_tuple(Right) andalso (element(1, Right) == unquote_splicing) ->
  do_quote({'{}', [], [Left, Right]}, Q, E);

do_quote({Left, Right}, Q, E) ->
  TLeft  = do_quote(Left, Q, E),
  TRight = do_quote(Right, Q, E),
  {TLeft, TRight};

%% Everything else

do_quote(Other, Q, E) when is_atom(E) ->
  do_escape(Other, Q, E);

do_quote({_, _, _} = Tuple, Q, E) ->
  Annotated = annotate(Tuple, Q#elixir_quote.context),
  do_quote_tuple(Annotated, Q, E);

do_quote([], _, _) ->
  [];

do_quote([H | T], #elixir_quote{unquote=false} = Q, E) ->
  do_quote_simple_list(T, do_quote(H, Q, E), Q, E);

do_quote([H | T], Q, E) ->
  do_quote_tail(lists:reverse(T, [H]), Q, E);

do_quote(Other, _, _) ->
  Other.

%% do_escape

do_escape({Left, _Meta, Right}, Q, E = prune_metadata) ->
  TL = do_quote(Left, Q, E),
  TR = do_quote(Right, Q, E),
  {'{}', [], [TL, [], TR]};

do_escape(Tuple, Q, E) when is_tuple(Tuple) ->
  TT = do_quote(tuple_to_list(Tuple), Q, E),
  {'{}', [], TT};

do_escape(BitString, _, _) when is_bitstring(BitString) ->
  case bit_size(BitString) rem 8 of
    0 ->
      BitString;
    Size ->
      <<Bits:Size, Bytes/binary>> = BitString,
      {'<<>>', [], [{'::', [], [Bits, {size, [], [Size]}]}, {'::', [], [Bytes, {binary, [], []}]}]}
  end;

do_escape(Map, Q, E) when is_map(Map) ->
  TT = do_quote(maps:to_list(Map), Q, E),
  {'%{}', [], TT};

do_escape([], _, _) -> [];

do_escape([H | T], #elixir_quote{unquote=false} = Q, E) ->
  do_quote_simple_list(T, do_quote(H, Q, E), Q, E);

do_escape([H | T], Q, E) ->
  %% The improper case is inefficient, but improper lists are rare.
  try lists:reverse(T, [H]) of
    L -> do_quote_tail(L, Q, E)
  catch
    _:_ ->
      {L, R} = reverse_improper(T, [H]),
      TL = do_quote_splice(L, Q, E, [], []),
      TR = do_quote(R, Q, E),
      update_last(TL, fun(X) -> {'|', [], [X, TR]} end)
  end;

do_escape(Other, _, _)
    when is_number(Other); is_pid(Other); is_atom(Other) ->
  Other;

do_escape(Fun, _, _) when is_function(Fun) ->
  case (erlang:fun_info(Fun, env) == {env, []}) andalso
       (erlang:fun_info(Fun, type) == {type, external}) of
    true  -> Fun;
    false -> bad_escape(Fun)
  end;

do_escape(Other, _, _) ->
  bad_escape(Other).

bad_escape(Arg) ->
  argument_error(<<"cannot escape ", ('Elixir.Kernel':inspect(Arg, []))/binary, ". ",
                   "The supported values are: lists, tuples, maps, atoms, numbers, bitstrings, ",
                   "PIDs and remote functions in the format &Mod.fun/arity">>).

%% do_quote_*

do_quote_import(Name, Meta, ArgsOrAtom, #elixir_quote{imports_hygiene=true} = Q, E) ->
  Arity = case is_atom(ArgsOrAtom) of
    true  -> 0;
    false -> length(ArgsOrAtom)
  end,

  NewMeta = case (keyfind(import, Meta) == false) andalso
      elixir_dispatch:find_import(Meta, Name, Arity, E) of
    false ->
      case (Arity == 1) andalso keyfind(ambiguous_op, Meta) of
        {ambiguous_op, nil} -> keystore(ambiguous_op, Meta, Q#elixir_quote.context);
        _ -> Meta
      end;
    Receiver ->
      keystore(import, keystore(context, Meta, Q#elixir_quote.context), Receiver)
  end,

  Annotated = annotate({Name, NewMeta, ArgsOrAtom}, Q#elixir_quote.context),
  do_quote_tuple(Annotated, Q, E).

do_quote_call(Left, Meta, Expr, Args, Q, E) ->
  All  = [meta(Meta, Q), Left, {unquote, Meta, [Expr]}, Args, Q#elixir_quote.context],
  TAll = [do_quote(X, Q, E) || X <- All],
  {{'.', Meta, [elixir_quote, dot]}, Meta, TAll}.

do_quote_fa(Target, Meta, Args, F, A, Q, E) ->
  NewMeta =
    case (keyfind(import_fa, Meta) == false) andalso
         elixir_dispatch:find_import(Meta, F, A, E) of
      false    -> Meta;
      Receiver -> keystore(import_fa, Meta, {Receiver, Q#elixir_quote.context})
    end,
  do_quote_tuple(Target, NewMeta, Args, Q, E).

do_quote_tuple({Left, Meta, Right}, Q, E) ->
  do_quote_tuple(Left, Meta, Right, Q, E).

% In a def unquote(name)(args) expression name will be an atom literal,
% thus location: :keep will not have enough information to generate the proper file/line annotation.
% This alters metadata to force Elixir to show the file to which the definition is added
% instead of the file where definition is quoted (i.e. we behave the opposite to location: :keep).
do_quote_tuple(Left, Meta, [{{unquote, _, _}, _, _}, _] = Right, Q, E) when ?defs(Left) ->
  TLeft  = do_quote(Left, Q, E),
  [Head, Body] = do_quote(Right, Q, E),
  {'{}', [], [HLeft, HMeta, HRight]} = Head,
  NewMeta = lists:keydelete(file, 1, HMeta),
  NewHead = {'{}', [], [HLeft, NewMeta, HRight]},
  {'{}', [], [TLeft, meta(Meta, Q), [NewHead, Body]]};

do_quote_tuple(Left, Meta, Right, Q, E) ->
  TLeft = do_quote(Left, Q, E),
  TRight = do_quote(Right, Q, E),
  {'{}', [], [TLeft, meta(Meta, Q), TRight]}.

do_quote_simple_list([], Prev, _, _) -> [Prev];
do_quote_simple_list([H | T], Prev, Q, E) ->
  [Prev | do_quote_simple_list(T, do_quote(H, Q, E), Q, E)];
do_quote_simple_list(Other, Prev, Q, E) ->
  [{'|', [], [Prev, do_quote(Other, Q, E)]}].

do_quote_tail([{'|', Meta, [{unquote_splicing, _, [Left]}, Right]} | T], #elixir_quote{unquote=true} = Q, E) ->
  %% Process the remaining entries on the list.
  %% For [1, 2, 3, unquote_splicing(arg) | tail], this will quote
  %% 1, 2 and 3, which could even be unquotes.
  TT = do_quote_splice(T, Q, E, [], []),
  TR = do_quote(Right, Q, E),
  do_runtime_list(Meta, tail_list, [Left, TR, TT]);

do_quote_tail(List, Q, E) ->
  do_quote_splice(List, Q, E, [], []).

do_quote_splice([{unquote_splicing, Meta, [Expr]} | T], #elixir_quote{unquote=true} = Q, E, Buffer, Acc) ->
  Runtime = do_runtime_list(Meta, list, [Expr, do_list_concat(Buffer, Acc)]),
  do_quote_splice(T, Q, E, [], Runtime);

do_quote_splice([H | T], Q, E, Buffer, Acc) ->
  TH = do_quote(H, Q, E),
  do_quote_splice(T, Q, E, [TH | Buffer], Acc);

do_quote_splice([], _Q, _E, Buffer, Acc) ->
  do_list_concat(Buffer, Acc).

do_list_concat(Left, []) -> Left;
do_list_concat([], Right) -> Right;
do_list_concat(Left, Right) -> {{'.', [], [erlang, '++']}, [], [Left, Right]}.

do_runtime_list(Meta, Fun, Args) ->
  {{'.', Meta, [elixir_quote, Fun]}, Meta, Args}.

%% Helpers

meta(Meta, Q) ->
  generated(keep(Meta, Q), Q).

generated(Meta, #elixir_quote{generated=true}) -> [{generated, true} | Meta];
generated(Meta, #elixir_quote{generated=false}) -> Meta.

keep(Meta, #elixir_quote{file=nil, line=Line}) ->
  line(Meta, Line);
keep(Meta, #elixir_quote{file=File}) ->
  case lists:keytake(line, 1, Meta) of
    {value, {line, Line}, MetaNoLine} ->
      [{keep, {File, Line}} | MetaNoLine];
    false ->
      [{keep, {File, 0}} | Meta]
  end.

line(Meta, true) ->
  Meta;
line(Meta, false) ->
  keydelete(line, Meta);
line(Meta, Line) ->
  keystore(line, Meta, Line).

reverse_improper([H | T], Acc) -> reverse_improper(T, [H | Acc]);
reverse_improper([], Acc) -> Acc;
reverse_improper(T, Acc) -> {Acc, T}.

update_last([], _) -> [];
update_last([H], F) -> [F(H)];
update_last([H | T], F) -> [H | update_last(T, F)].

keyfind(Key, Meta) ->
  lists:keyfind(Key, 1, Meta).
keydelete(Key, Meta) ->
  lists:keydelete(Key, 1, Meta).
keystore(_Key, Meta, nil) ->
  Meta;
keystore(Key, Meta, Value) ->
  lists:keystore(Key, 1, Meta, {Key, Value}).
keynew(Key, Meta, Value) ->
  case lists:keymember(Key, 1, Meta) of
    true -> Meta;
    false -> [{Key, Value} | Meta]
  end.
