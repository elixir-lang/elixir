-module(elixir_quote).
-export([escape/3, linify/3, linify_with_context_counter/3, build/6, quote/6, has_unquotes/1, fun_to_quoted/1]).
-export([dot/5, tail_list/3, list/2, validate_runtime/2]). %% Quote callbacks

-include("elixir.hrl").
-define(defs(Kind), Kind == def; Kind == defp; Kind == defmacro; Kind == defmacrop; Kind == '@').
-define(lexical(Kind), Kind == import; Kind == alias; Kind == require).
-compile({inline, [keyfind/2, keystore/3, keydelete/2, keynew/3, do_tuple_linify/5]}).

-record(elixir_quote, {
  line=false,
  file=nil,
  context=nil,
  vars_hygiene=true,
  aliases_hygiene=true,
  imports_hygiene=true,
  unquote=true,
  generated=false
}).


%% fun_to_quoted

fun_to_quoted(Function) ->
  Meta = [],
  {module, Module} = erlang:fun_info(Function, module),
  {name, Name}     = erlang:fun_info(Function, name),
  {arity, Arity}   = erlang:fun_info(Function, arity),
  {'&', Meta, [{'/', Meta, [{{'.', Meta, [Module, Name]}, [{no_parens, true} | Meta], []}, Arity]}]}.

%% has_unquotes

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

%% Apply the line from site call on quoted contents.
%% Receives a Key to look for the default line as argument.
linify(0, _Key, Exprs) ->
  Exprs;
linify(Line, Key, Exprs) when is_integer(Line) ->
  Fun =
    case Key of
      line ->
        fun(Meta) -> keynew(line, Meta, Line) end;
      keep ->
        fun(Meta) ->
          case lists:keytake(keep, 1, Meta) of
            {value, {keep, {_, Int}}, MetaNoFile} ->
              [{line, Int} | keydelete(line, MetaNoFile)];
            _ ->
              keynew(line, Meta, Line)
          end
        end
    end,

  do_linify(Fun, Exprs, nil).

%% Same as linify but also considers the context counter and generated.
linify_with_context_counter(ContextMeta, Var, Exprs) when is_list(ContextMeta) ->
  Line = ?line(ContextMeta),

  Fun =
    case lists:keyfind(generated, 1, ContextMeta) of
      {generated, true} when Line =:= 0 -> fun(Meta) -> [{generated, true} | Meta] end;
      {generated, true} -> fun(Meta) -> [{generated, true} | keynew(line, Meta, Line)] end;
      _ when Line =:= 0 -> fun(Meta) -> Meta end;
      _ -> fun(Meta) -> keynew(line, Meta, Line) end
    end,

  do_linify(Fun, Exprs, Var).

do_linify(Fun, {quote, Meta, [_ | _] = Args}, {Receiver, Counter} = Var)
    when is_list(Meta) ->
  NewMeta =
    case keyfind(context, Meta) == {context, Receiver} of
      true -> keynew(counter, Meta, Counter);
      false -> Meta
    end,
  do_tuple_linify(Fun, NewMeta, quote, Args, Var);

do_linify(Fun, {Left, Meta, Receiver}, {Receiver, Counter} = Var)
    when is_atom(Left), is_list(Meta), Left /= '_' ->
  do_tuple_linify(Fun, keynew(counter, Meta, Counter), Left, Receiver, Var);

do_linify(Fun, {Lexical, Meta, [_ | _] = Args}, {_, Counter} = Var)
    when ?lexical(Lexical); Lexical == '__aliases__' ->
  do_tuple_linify(Fun, keynew(counter, Meta, Counter), Lexical, Args, Var);

do_linify(Fun, {Left, Meta, Right}, Var) when is_list(Meta) ->
  do_tuple_linify(Fun, Meta, Left, Right, Var);

do_linify(Fun, {Left, Right}, Var) ->
  {do_linify(Fun, Left, Var), do_linify(Fun, Right, Var)};

do_linify(Fun, List, Var) when is_list(List) ->
  [do_linify(Fun, X, Var) || X <- List];

do_linify(_, Else, _) -> Else.

-compile({inline, do_tuple_linify/5}).
do_tuple_linify(Fun, Meta, Left, Right, Var) ->
  {do_linify(Fun, Left, Var), Fun(Meta), do_linify(Fun, Right, Var)}.

%% Escaping

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

do_escape({Left, Meta, Right}, Q, E = prune_metadata) ->
  TM = [{K, V} || {K, V} <- Meta, (K == no_parens) orelse (K == line)],
  TL = do_quote(Left, Q, E),
  TR = do_quote(Right, Q, E),
  {'{}', [], [TL, TM, TR]};

do_escape(Tuple, Q, E) when is_tuple(Tuple) ->
  TT = do_quote(tuple_to_list(Tuple), Q, E),
  {'{}', [], TT};

do_escape(BitString, _, _) when is_bitstring(BitString) ->
  case bit_size(BitString) rem 8 of
    0 ->
      BitString;
    Size ->
      <<Bits:Size, Bytes/binary>> = BitString,
      {'<<>>', [], [{'::', [], [Bits, {size, [], [Size]}]}, {'::', [], [Bytes, {binary, [], nil}]}]}
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
    true  -> fun_to_quoted(Fun);
    false -> bad_escape(Fun)
  end;

do_escape(Other, _, _) ->
  bad_escape(Other).

bad_escape(Arg) ->
  argument_error(<<"cannot escape ", ('Elixir.Kernel':inspect(Arg, []))/binary, ". ",
                   "The supported values are: lists, tuples, maps, atoms, numbers, bitstrings, ",
                   "PIDs and remote functions in the format &Mod.fun/arity">>).

%% Quote entry points

build(Meta, Line, File, Context, Unquote, Generated) ->
  Acc0 = [],
  {ELine, Acc1} = validate_compile(Meta, line, Line, Acc0),
  {EFile, Acc2} = validate_compile(Meta, file, File, Acc1),
  {EContext, Acc3} = validate_compile(Meta, context, Context, Acc2),
  validate_runtime(unquote, Unquote),
  validate_runtime(generated, Generated),

  Q = #elixir_quote{
    line=ELine,
    file=EFile,
    unquote=Unquote,
    context=EContext,
    generated=Generated
  },

  {Q, Acc3}.

validate_compile(_Meta, line, Value, Acc) when is_boolean(Value) ->
  {Value, Acc};
validate_compile(_Meta, file, nil, Acc) ->
  {nil, Acc};
validate_compile(Meta, Key, Value, Acc) ->
  case is_valid(Key, Value) of
    true ->
      {Value, Acc};
    false ->
      Var = {Key, Meta, ?MODULE},
      Call = {{'.', Meta, [?MODULE, validate_runtime]}, Meta, [Key, Value]},
      {Var, [{'=', Meta, [Var, Call]} | Acc]}
  end.

validate_runtime(Key, Value) ->
  case is_valid(Key, Value) of
    true ->
      Value;

    false ->
      erlang:error(
        'Elixir.ArgumentError':exception(
          <<"invalid runtime value for option :", (erlang:atom_to_binary(Key))/binary,
            " in quote, got: ", ('Elixir.Kernel':inspect(Value))/binary>>
        )
      )
  end.

is_valid(line, Line) -> is_integer(Line);
is_valid(file, File) -> is_binary(File);
is_valid(context, Context) -> is_atom(Context) andalso (Context /= nil);
is_valid(generated, Generated) -> is_boolean(Generated);
is_valid(unquote, Unquote) -> is_boolean(Unquote).

quote(_Meta, {unquote_splicing, _, [_]}, _Binding, #elixir_quote{unquote=true}, _, _) ->
  argument_error(<<"unquote_splicing only works inside arguments and block contexts, "
    "wrap it in parens if you want it to work with one-liners">>);

quote(Meta, Expr, Binding, Q, Prelude, E) ->
  Context = Q#elixir_quote.context,

  Vars = [{'{}', [],
    ['=', [], [
      {'{}', [], [K, Meta, Context]},
      V
    ]]
  } || {K, V} <- Binding],

  Quoted = do_quote(Expr, Q, E),

  WithVars = case Vars of
    [] -> Quoted;
    _ -> {'{}', [], ['__block__', [], Vars ++ [Quoted]]}
  end,

  case Prelude of
    [] -> WithVars;
    _ -> {'__block__', [], Prelude ++ [WithVars]}
  end.

%% quote/unquote

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

do_quote({'__aliases__', Meta, [H | T]} = Alias, #elixir_quote{aliases_hygiene=true} = Q, E)
    when is_atom(H), H /= 'Elixir' ->
  Annotation =
    case elixir_aliases:expand(Alias, E) of
      Atom when is_atom(Atom) -> Atom;
      Aliases when is_list(Aliases) -> false
    end,
  AliasMeta = keystore(alias, keydelete(counter, Meta), Annotation),
  do_quote_tuple('__aliases__', AliasMeta, [H | T], Q, E);

%% Vars

do_quote({Name, Meta, nil}, #elixir_quote{vars_hygiene=true} = Q, E)
    when is_atom(Name), is_list(Meta) ->
  ImportMeta = if
    Q#elixir_quote.imports_hygiene -> import_meta(Meta, Name, 0, Q, E);
    true -> Meta
  end,

  {'{}', [], [Name, meta(ImportMeta, Q), Q#elixir_quote.context]};

%% Unquote

do_quote({{{'.', Meta, [Left, unquote]}, _, [Expr]}, _, Args}, #elixir_quote{unquote=true} = Q, E) ->
  do_quote_call(Left, Meta, Expr, Args, Q, E);

do_quote({{'.', Meta, [Left, unquote]}, _, [Expr]}, #elixir_quote{unquote=true} = Q, E) ->
  do_quote_call(Left, Meta, Expr, nil, Q, E);

%% Imports

do_quote({'&', Meta, [{'/', _, [{F, _, C}, A]}] = Args},
         #elixir_quote{imports_hygiene=true} = Q, E) when is_atom(F), is_integer(A), is_atom(C) ->
  NewMeta =
    case elixir_dispatch:find_import(Meta, F, A, E) of
      false ->
        Meta;

      Receiver ->
        keystore(context, keystore(imports, Meta, [{A, Receiver}]), Q#elixir_quote.context)
    end,
  do_quote_tuple('&', NewMeta, Args, Q, E);

do_quote({Name, Meta, ArgsOrContext}, #elixir_quote{imports_hygiene=true} = Q, E)
    when is_atom(Name), is_list(Meta), is_list(ArgsOrContext) or is_atom(ArgsOrContext) ->
  Arity = if
    is_atom(ArgsOrContext) -> 0;
    true -> length(ArgsOrContext)
  end,

  ImportMeta = import_meta(Meta, Name, Arity, Q, E),
  Annotated = annotate({Name, ImportMeta, ArgsOrContext}, Q#elixir_quote.context),
  do_quote_tuple(Annotated, Q, E);

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

import_meta(Meta, Name, Arity, Q, E) ->
  case (keyfind(import, Meta) == false) andalso
      elixir_dispatch:find_imports(Meta, Name, E) of
    [] ->
      case (Arity == 1) andalso keyfind(ambiguous_op, Meta) of
        {ambiguous_op, nil} -> keystore(ambiguous_op, Meta, Q#elixir_quote.context);
        _ -> Meta
      end;

    Imports ->
      keystore(imports, keystore(context, Meta, Q#elixir_quote.context), Imports)
  end.

%% do_quote_*

do_quote_call(Left, Meta, Expr, Args, Q, E) ->
  All  = [Left, {unquote, Meta, [Expr]}, Args, Q#elixir_quote.context],
  TAll = [do_quote(X, Q, E) || X <- All],
  {{'.', Meta, [elixir_quote, dot]}, Meta, [meta(Meta, Q) | TAll]}.

do_quote_tuple({Left, Meta, Right}, Q, E) ->
  do_quote_tuple(Left, Meta, Right, Q, E).

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

%% do_quote runtime callbacks


%% Callbacks

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
      {{'.', Meta, [Left, Right]}, [{no_parens, true} | Meta], []}
  end;

dot(Meta, Left, {Right, _, Context}, nil) when is_atom(Right), is_atom(Context) ->
  {{'.', Meta, [Left, Right]}, [{no_parens, true} | Meta], []};

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