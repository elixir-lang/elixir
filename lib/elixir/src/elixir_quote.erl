-module(elixir_quote).
-export([escape/3, linify/3, linify_with_context_counter/3, build/7, quote/2, has_unquotes/1, fun_to_quoted/1]).
-export([dot/5, tail_list/3, list/2, validate_runtime/2]). %% Quote callbacks

-include("elixir.hrl").
-define(defs(Kind), Kind == def; Kind == defp; Kind == defmacro; Kind == defmacrop; Kind == '@').
-define(lexical(Kind), Kind == import; Kind == alias; Kind == require).
-compile({inline, [keyfind/2, keystore/3, keydelete/2, keynew/3, do_tuple_linify/5]}).

-record(elixir_quote, {
  line=false,
  file=nil,
  context=nil,
  op=none, % none | prune_metadata | add_context
  aliases_hygiene=nil,
  imports_hygiene=nil,
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

has_unquotes(Ast) -> has_unquotes(Ast, 0).

has_unquotes({quote, _, [Child]}, QuoteLevel) ->
  has_unquotes(Child, QuoteLevel + 1);
has_unquotes({quote, _, [QuoteOpts, Child]}, QuoteLevel) ->
  case disables_unquote(QuoteOpts) of
    true -> false;
    _ -> has_unquotes(Child, QuoteLevel + 1)
  end;
has_unquotes({Unquote, _, [Child]}, QuoteLevel)
  when Unquote == unquote; Unquote == unquote_splicing ->
  case QuoteLevel of
    0 -> true;
    _ ->  has_unquotes(Child, QuoteLevel - 1)
end;
has_unquotes({{'.', _, [_, unquote]}, _, [_]}, _) -> true;
has_unquotes({Var, _, Ctx}, _) when is_atom(Var), is_atom(Ctx) -> false;
has_unquotes({Name, _, Args}, QuoteLevel) when is_list(Args) ->
  has_unquotes(Name) orelse lists:any(fun(Child) -> has_unquotes(Child, QuoteLevel) end, Args);
has_unquotes({Left, Right}, QuoteLevel) ->
  has_unquotes(Left, QuoteLevel) orelse has_unquotes(Right, QuoteLevel);
has_unquotes(List, QuoteLevel) when is_list(List) ->
  lists:any(fun(Child) -> has_unquotes(Child, QuoteLevel) end, List);
has_unquotes(_Other, _) -> false.

disables_unquote([{unquote, false} | _]) -> true;
disables_unquote([{bind_quoted, _} | _]) -> true;
disables_unquote([_H | T]) -> disables_unquote(T);
disables_unquote(_) -> false.

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
      {generated, true} when Line =:= 0 -> fun elixir_utils:generated/1;
      {generated, true} -> fun(Meta) -> elixir_utils:generated(keynew(line, Meta, Line)) end;
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
escape(Expr, Op, Unquote) ->
  do_quote(Expr, #elixir_quote{
    line=true,
    file=nil,
    op=Op,
    unquote=Unquote
  }).

do_escape({Left, Meta, Right}, #elixir_quote{op=prune_metadata} = Q) when is_list(Meta) ->
  TM = [{K, V} || {K, V} <- Meta, (K == no_parens) orelse (K == line) orelse (K == delimiter)],
  TL = do_quote(Left, Q),
  TR = do_quote(Right, Q),
  {'{}', [], [TL, TM, TR]};

do_escape(Tuple, Q) when is_tuple(Tuple) ->
  TT = do_quote(tuple_to_list(Tuple), Q),
  {'{}', [], TT};

do_escape(BitString, _) when is_bitstring(BitString) ->
  case bit_size(BitString) rem 8 of
    0 ->
      BitString;
    Size ->
      <<Bits:Size, Bytes/binary>> = BitString,
      {'<<>>', [], [{'::', [], [Bits, {size, [], [Size]}]}, {'::', [], [Bytes, {binary, [], nil}]}]}
  end;

do_escape(Map, Q) when is_map(Map) ->
  TT = do_quote(lists:sort(maps:to_list(Map)), Q),
  {'%{}', [], TT};

do_escape([], _) -> [];

do_escape([H | T], #elixir_quote{unquote=false} = Q) ->
  do_quote_simple_list(T, do_quote(H, Q), Q);

do_escape([H | T], Q) ->
  %% The improper case is inefficient, but improper lists are rare.
  try lists:reverse(T, [H]) of
    L -> do_quote_tail(L, Q)
  catch
    _:_ ->
      {L, R} = reverse_improper(T, [H]),
      TL = do_quote_splice(L, Q, [], []),
      TR = do_quote(R, Q),
      update_last(TL, fun(X) -> {'|', [], [X, TR]} end)
  end;

do_escape(Other, _) when is_number(Other); is_atom(Other); is_pid(Other) ->
  Other;

do_escape(Fun, _) when is_function(Fun) ->
  case (erlang:fun_info(Fun, env) == {env, []}) andalso
       (erlang:fun_info(Fun, type) == {type, external}) of
    true  -> fun_to_quoted(Fun);
    false -> bad_escape(Fun)
  end;

do_escape(Other, _) ->
  bad_escape(Other).

bad_escape(Arg) ->
  argument_error(<<"cannot escape ", ('Elixir.Kernel':inspect(Arg, []))/binary, ". ",
                   "The supported values are: lists, tuples, maps, atoms, numbers, bitstrings, ",
                   "PIDs and remote functions in the format &Mod.fun/arity">>).

%% Quote entry points

build(Meta, Line, File, Context, Unquote, Generated, E) ->
  Acc0 = [],
  {VLine, Acc1} = validate_compile(Meta, line, Line, Acc0),
  {VFile, Acc2} = validate_compile(Meta, file, File, Acc1),
  {VContext, Acc3} = validate_compile(Meta, context, Context, Acc2),
  validate_runtime(unquote, Unquote),
  validate_runtime(generated, Generated),

  Q = #elixir_quote{
    op=add_context,
    aliases_hygiene=E,
    imports_hygiene=E,
    line=VLine,
    file=VFile,
    unquote=Unquote,
    context=VContext,
    generated=Generated
  },

  {Q, VContext, Acc3}.

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

quote({unquote_splicing, _, [_]}, #elixir_quote{unquote=true}) ->
  argument_error(<<"unquote_splicing only works inside arguments and block contexts, "
    "wrap it in parens if you want it to work with one-liners">>);
quote(Expr, Q) ->
  do_quote(Expr, Q).

%% quote/unquote

do_quote({quote, Meta, [Arg]}, Q) when is_list(Meta) ->
  TArg = do_quote(Arg, Q#elixir_quote{unquote=false}),

  NewMeta = case Q of
    #elixir_quote{op=add_context, context=Context} -> keystore(context, Meta, Context);
    _ -> Meta
  end,

  {'{}', [], [quote, meta(NewMeta, Q), [TArg]]};

do_quote({quote, Meta, [Opts, Arg]}, Q) when is_list(Meta) ->
  TOpts = do_quote(Opts, Q),
  TArg = do_quote(Arg, Q#elixir_quote{unquote=false}),

  NewMeta = case Q of
    #elixir_quote{op=add_context, context=Context} -> keystore(context, Meta, Context);
    _ -> Meta
  end,

  {'{}', [], [quote, meta(NewMeta, Q), [TOpts, TArg]]};

do_quote({unquote, Meta, [Expr]}, #elixir_quote{unquote=true}) when is_list(Meta) ->
  Expr;

%% Aliases

do_quote({'__aliases__', Meta, [H | T]}, #elixir_quote{aliases_hygiene=(#{}=E)} = Q)
     when is_list(Meta), is_atom(H), H /= 'Elixir' ->
  Annotation =
    case elixir_aliases:expand(Meta, [H | T], E, true) of
      Atom when is_atom(Atom) -> Atom;
      Aliases when is_list(Aliases) -> false
    end,
  AliasMeta = keystore(alias, keydelete(counter, Meta), Annotation),
  do_quote_tuple('__aliases__', AliasMeta, [H | T], Q);

%% Vars

do_quote({Name, Meta, nil}, #elixir_quote{op=add_context} = Q)
    when is_atom(Name), is_list(Meta) ->
  ImportMeta = case Q#elixir_quote.imports_hygiene of
    nil -> Meta;
    E -> import_meta(Meta, Name, 0, Q, E)
  end,

  {'{}', [], [Name, meta(ImportMeta, Q), Q#elixir_quote.context]};

%% Unquote

do_quote({{{'.', Meta, [Left, unquote]}, _, [Expr]}, _, Args}, #elixir_quote{unquote=true} = Q) when is_list(Meta) ->
  do_quote_call(Left, Meta, Expr, Args, Q);

do_quote({{'.', Meta, [Left, unquote]}, _, [Expr]}, #elixir_quote{unquote=true} = Q) when is_list(Meta) ->
  do_quote_call(Left, Meta, Expr, nil, Q);

%% Imports

do_quote({'&', Meta, [{'/', _, [{F, _, C}, A]}] = Args},
  #elixir_quote{imports_hygiene=(#{}=E)} = Q) when is_atom(F), is_integer(A), is_atom(C), is_list(Meta) ->
  NewMeta =
    case elixir_dispatch:find_import(Meta, F, A, E) of
      false ->
        Meta;

      Receiver ->
        keystore(context, keystore(imports, Meta, [{A, Receiver}]), Q#elixir_quote.context)
    end,
  do_quote_tuple('&', NewMeta, Args, Q);

do_quote({Name, Meta, ArgsOrContext}, #elixir_quote{imports_hygiene=(#{}=E)} = Q)
    when is_atom(Name), is_list(Meta), is_list(ArgsOrContext) or is_atom(ArgsOrContext) ->
  Arity = if
    is_atom(ArgsOrContext) -> 0;
    true -> length(ArgsOrContext)
  end,

  ImportMeta = import_meta(Meta, Name, Arity, Q, E),
  Annotated = annotate({Name, ImportMeta, ArgsOrContext}, Q#elixir_quote.context),
  do_quote_tuple(Annotated, Q);

%% Two-element tuples

do_quote({Left, Right}, #elixir_quote{unquote=true} = Q) when
    is_tuple(Left)  andalso (element(1, Left) == unquote_splicing);
    is_tuple(Right) andalso (element(1, Right) == unquote_splicing) ->
  do_quote({'{}', [], [Left, Right]}, Q);

do_quote({Left, Right}, Q) ->
  TLeft  = do_quote(Left, Q),
  TRight = do_quote(Right, Q),
  {TLeft, TRight};

%% Everything else

do_quote(Other, #elixir_quote{op=Op} = Q) when Op =/= add_context ->
  do_escape(Other, Q);

do_quote({_, _, _} = Tuple, Q) ->
  Annotated = annotate(Tuple, Q#elixir_quote.context),
  do_quote_tuple(Annotated, Q);

do_quote([], _) ->
  [];

do_quote([H | T], #elixir_quote{unquote=false} = Q) ->
  do_quote_simple_list(T, do_quote(H, Q), Q);

do_quote([H | T], Q) ->
  do_quote_tail(lists:reverse(T, [H]), Q);

do_quote(Other, _) ->
  Other.

import_meta(Meta, Name, Arity, Q, E) ->
  case (keyfind(imports, Meta) == false) andalso
      elixir_dispatch:find_imports(Meta, Name, E) of
    [_ | _] = Imports ->
      trace_import_quoted(Imports, Meta, Name, E),
      keystore(imports, keystore(context, Meta, Q#elixir_quote.context), Imports);

    _ ->
      case (Arity == 1) andalso keyfind(ambiguous_op, Meta) of
        {ambiguous_op, nil} -> keystore(ambiguous_op, Meta, Q#elixir_quote.context);
        _ -> Meta
      end
  end.

trace_import_quoted([{Arity, Mod} | Imports], Meta, Name, E) ->
  {Rest, Arities} = collect_trace_import_quoted(Imports, Mod, [], [Arity]),
  elixir_env:trace({imported_quoted, Meta, Mod, Name, Arities}, E),
  trace_import_quoted(Rest, Meta, Name, E);
trace_import_quoted([], _Meta, _Name, _E) ->
  ok.

collect_trace_import_quoted([{Arity, Mod} | Imports], Mod, Acc, Arities) ->
  collect_trace_import_quoted(Imports, Mod, Acc, [Arity | Arities]);
collect_trace_import_quoted([Import | Imports], Mod, Acc, Arities) ->
  collect_trace_import_quoted(Imports, Mod, [Import | Acc], Arities);
collect_trace_import_quoted([], _Mod, Acc, Arities) ->
  {lists:reverse(Acc), lists:reverse(Arities)}.

%% do_quote_*

do_quote_call(Left, Meta, Expr, Args, Q) ->
  All  = [Left, {unquote, Meta, [Expr]}, Args, Q#elixir_quote.context],
  TAll = [do_quote(X, Q) || X <- All],
  {{'.', Meta, [elixir_quote, dot]}, Meta, [meta(Meta, Q) | TAll]}.

do_quote_tuple({Left, Meta, Right}, Q) ->
  do_quote_tuple(Left, Meta, Right, Q).

do_quote_tuple(Left, Meta, Right, Q) ->
  TLeft = do_quote(Left, Q),
  TRight = do_quote(Right, Q),
  {'{}', [], [TLeft, meta(Meta, Q), TRight]}.

do_quote_simple_list([], Prev, _) -> [Prev];
do_quote_simple_list([H | T], Prev, Q) ->
  [Prev | do_quote_simple_list(T, do_quote(H, Q), Q)];
do_quote_simple_list(Other, Prev, Q) ->
  [{'|', [], [Prev, do_quote(Other, Q)]}].

do_quote_tail([{'|', Meta, [{unquote_splicing, _, [Left]}, Right]} | T], #elixir_quote{unquote=true} = Q) ->
  %% Process the remaining entries on the list.
  %% For [1, 2, 3, unquote_splicing(arg) | tail], this will quote
  %% 1, 2 and 3, which could even be unquotes.
  TT = do_quote_splice(T, Q, [], []),
  TR = do_quote(Right, Q),
  do_runtime_list(Meta, tail_list, [Left, TR, TT]);

do_quote_tail(List, Q) ->
  do_quote_splice(List, Q, [], []).

do_quote_splice([{unquote_splicing, Meta, [Expr]} | T], #elixir_quote{unquote=true} = Q, Buffer, Acc) ->
  Runtime = do_runtime_list(Meta, list, [Expr, do_list_concat(Buffer, Acc)]),
  do_quote_splice(T, Q, [], Runtime);

do_quote_splice([H | T], Q, Buffer, Acc) ->
  TH = do_quote(H, Q),
  do_quote_splice(T, Q, [TH | Buffer], Acc);

do_quote_splice([], _Q, Buffer, Acc) ->
  do_list_concat(Buffer, Acc).

do_list_concat(Left, []) -> Left;
do_list_concat([], Right) -> Right;
do_list_concat(Left, Right) -> {{'.', [], [erlang, '++']}, [], [Left, Right]}.

do_runtime_list(Meta, Fun, Args) ->
  {{'.', Meta, [elixir_quote, Fun]}, Meta, Args}.

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
  generated(keep(keydelete(column, Meta), Q), Q).

generated(Meta, #elixir_quote{generated=true}) -> [{generated, true} | Meta];
generated(Meta, #elixir_quote{generated=false}) -> Meta.

keep(Meta, #elixir_quote{file=nil, line=Line}) ->
  line(Meta, Line);
keep(Meta, #elixir_quote{file=File, line=true}) ->
  case lists:keytake(line, 1, Meta) of
    {value, {line, Line}, MetaNoLine} ->
      [{keep, {File, Line}} | MetaNoLine];
    false ->
      [{keep, {File, 0}} | Meta]
  end;
keep(Meta, #elixir_quote{file=File, line=false}) ->
  [{keep, {File, 0}} | keydelete(line, Meta)];
keep(Meta, #elixir_quote{file=File, line=Line}) ->
  [{keep, {File, Line}} | keydelete(line, Meta)].

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

annotate({Def, Meta, [H | T]}, Context) when ?defs(Def) ->
  {Def, Meta, [annotate_def(H, Context) | T]};
annotate({{'.', _, [_, Def]} = Target, Meta, [H | T]}, Context) when ?defs(Def) ->
  {Target, Meta, [annotate_def(H, Context) | T]};
annotate({Lexical, Meta, [_ | _] = Args}, Context) when ?lexical(Lexical) ->
  NewMeta = keystore(context, keydelete(counter, Meta), Context),
  {Lexical, NewMeta, Args};
annotate(Tree, _Context) -> Tree.

annotate_def({'when', Meta, [Left, Right]}, Context) ->
  {'when', Meta, [annotate_def(Left, Context), Right]};
annotate_def({Fun, Meta, Args}, Context) ->
  {Fun, keystore(context, Meta, Context), Args};
annotate_def(Other, _Context) ->
  Other.
