-module(elixir_exp).
-export([expand/2]).
-import(elixir_errors, [compile_error/3, compile_error/4]).
-include("elixir.hrl").

%% =

expand({ '=', Meta, [Left, Right] }, E) ->
  % assert_no_guard_scope(Meta, '=', S),
  { ERight, ER } = expand(Right, E),
  { ELeft, EL }  = match(fun expand/2, Left, E, ER),
  { { '=', Meta, [ELeft, ERight] }, EL };

%% Literal operators

expand({ '{}', Meta, Args }, E) ->
  { EArgs, EA } = expand_args(Args, E),
  { { '{}', Meta, EArgs }, EA };

%% Other operators

expand({ '__op__', Meta, [_, _] = Args }, E) ->
  { EArgs, EA } = expand_args(Args, E),
  { { '__op__', Meta, EArgs }, EA };

expand({ '__op__', Meta, [_, _, _] = Args }, E) ->
  { EArgs, EA } = expand_args(Args, E),
  { { '__op__', Meta, EArgs }, EA };

expand({ '->', Meta, _Args }, E) ->
  compile_error(Meta, E#elixir_env.file, "unhandled operator ->");

%% __block__

expand({ '__block__', _Meta, [] }, E) ->
  { nil, E };
expand({ '__block__', _Meta, [Arg] }, E) ->
  expand(Arg, E);
expand({ '__block__', Meta, Args }, E) when is_list(Args) ->
  { EArgs, EA } = expand_many(Args, E),
  { { '__block__', Meta, EArgs }, EA };

%% __aliases__

expand({ '__aliases__', Meta, _ } = Alias, E) ->
  case elixir_aliases:expand(Alias, E#elixir_env.aliases,
                             E#elixir_env.macro_aliases, E#elixir_env.lexical_tracker) of
    Receiver when is_atom(Receiver) ->
      elixir_lexical:record_remote(Receiver, E#elixir_env.lexical_tracker),
      { Receiver, E };
    Aliases ->
      { EAliases, EA } = expand_args(Aliases, E),

      case lists:all(fun is_atom/1, EAliases) of
        true ->
          Receiver = elixir_aliases:concat(EAliases),
          elixir_lexical:record_remote(Receiver, E#elixir_env.lexical_tracker),
          { Receiver, EA };
        false ->
          { { '__aliases__', Meta, EAliases }, EA }
      end
  end;

%% alias

expand({ alias, Meta, [Ref] }, E) ->
  expand({ alias, Meta, [Ref,[]] }, E);
expand({ alias, Meta, [Ref, KV] }, E) ->
  % assert_no_match_or_guard_scope(Meta, alias, S),
  { ERef, ER } = expand(Ref, E),
  { EKV, EO }  = expand_opts(Meta, alias, [as, warn], no_alias_opts(KV), ER),

  if
    is_atom(ERef) ->
      expand_alias(Meta, true, ERef, EKV, EO);
    true ->
      compile_error(Meta, E#elixir_env.file,
        "invalid args for alias, expected a compile time atom or alias as argument")
  end;

%% Pseudo vars

expand({ '__MODULE__', _, Atom }, E) when is_atom(Atom) ->
  { E#elixir_env.module, E };
expand({ '__FILE__', _, Atom }, E) when is_atom(Atom) ->
  { E#elixir_env.file, E };
expand({ '__DIR__', _, Atom }, E) when is_atom(Atom) ->
  { filename:dirname(E#elixir_env.file), E };
expand({ '__CALLER__', _, Atom } = Caller, E) when is_atom(Atom) ->
  { Caller, E };
expand({ '__ENV__', _, Atom }, E) when is_atom(Atom) ->
  { elixir_env:env_to_ex(E), E };
expand({ { '.', _, [{ '__ENV__', _, Atom }, Field] }, _, [] }, E) when is_atom(Atom), is_atom(Field) ->
  { (elixir_env:env_to_ex(E)):Field(), E };

%% Quote

expand({ Unquote, Meta, [_] }, E) when Unquote == unquote; Unquote == unquote_splicing ->
  compile_error(Meta, E#elixir_env.file, "~p called outside quote", [Unquote]);

expand({ quote, Meta, [Opts] }, E) when is_list(Opts) ->
  case lists:keyfind(do, 1, Opts) of
    { do, Do } ->
      expand({ quote, Meta, [lists:keydelete(do, 1, Opts), [{do,Do}]] }, E);
    false ->
      compile_error(Meta, E#elixir_env.file, "missing do keyword in quote")
  end;

expand({ quote, Meta, [_] }, E) ->
  compile_error(Meta, E#elixir_env.file, "invalid args for quote");

expand({ quote, Meta, [KV, Do] }, E) when is_list(Do) ->
  Exprs =
    case lists:keyfind(do, 1, Do) of
      { do, Expr } -> Expr;
      false -> compile_error(Meta, E#elixir_scope.file, "missing do keyword in quote")
    end,

  ValidOpts   = [hygiene, context, var_context, location, line, unquote, bind_quoted],
  { EKV, ET } = expand_opts(Meta, quote, ValidOpts, KV, E),

  Hygiene = case lists:keyfind(hygiene, 1, EKV) of
    { hygiene, List } when is_list(List) ->
      List;
    false ->
      []
  end,

  Context = case lists:keyfind(context, 1, EKV) of
    { context, Atom } when is_atom(Atom) ->
      Atom;
    { context, _ } ->
      compile_error(Meta, E#elixir_env.file, "invalid :context for quote, expected a compile time atom or an alias");
    false ->
      case E#elixir_env.module of
        nil -> 'Elixir';
        Mod -> Mod
      end
  end,

  Vars    = lists:keyfind(vars, 1, Hygiene) /= { vars, false },
  Aliases = lists:keyfind(aliases, 1, Hygiene) /= { aliases, false },
  Imports = lists:keyfind(imports, 1, Hygiene) /= { imports, false },

  Keep = lists:keyfind(location, 1, EKV) == { location, keep },
  Line = proplists:get_value(line, EKV, false),

  { Binding, DefaultUnquote } = case lists:keyfind(bind_quoted, 1, EKV) of
    { bind_quoted, BQ } -> { BQ, false };
    false -> { nil, true }
  end,

  Unquote = case lists:keyfind(unquote, 1, EKV) of
    { unquote, Bool } when is_boolean(Bool) -> Bool;
    false -> DefaultUnquote
  end,

  Q = #elixir_quote{vars_hygiene=Vars, line=Line, keep=Keep, unquote=Unquote,
        aliases_hygiene=Aliases, imports_hygiene=Imports, context=Context},

  { Quoted, _Q } = elixir_quote:quote(Exprs, Binding, Q, ET),
  expand(Quoted, ET);

expand({ quote, Meta, [_, _] }, E) ->
  compile_error(Meta, E#elixir_env.file, "invalid args for quote");

%% Comprehensions

expand({ Kind, Meta, Args }, E) when is_list(Args), (Kind == lc) orelse (Kind == bc) ->
  expand_comprehension(Meta, Kind, Args, E);

%% Super

expand({ super, Meta, Args }, E) when is_list(Args) ->
  { EArgs, EA } = expand_args(Args, E),
  { { super, Meta, EArgs }, EA };

%% Vars

expand({ '^', Meta, [Arg] }, #elixir_env{context=match} = E) ->
  case expand(Arg, E) of
    { { Name, _, Kind } = EArg, EA } when is_atom(Name), is_atom(Kind) ->
      { { '^', Meta, [EArg] }, EA };
    _ ->
    Msg = "invalid args for unary operator ^, expected an existing variable, got ^~ts",
    compile_error(Meta, E#elixir_env.file, Msg, ['Elixir.Macro':to_string(Arg)])
  end;
expand({ '^', Meta, [Arg] }, E) ->
  compile_error(Meta, E#elixir_env.file,
    "cannot use ^~ts outside of match clauses", ['Elixir.Macro':to_string(Arg)]);

expand({ Name, Meta, Kind } = Var, #elixir_env{context=match,vars=Vars} = E) when is_atom(Name), is_atom(Kind) ->
  { Var, E#elixir_env{vars=ordsets:add_element({ Name, var_kind(Meta, Kind) }, Vars)} };
expand({ Name, Meta, Kind } = Var, #elixir_env{vars=Vars} = E) when is_atom(Name), is_atom(Kind) ->
  case lists:member({ Name, var_kind(Meta, Kind) }, Vars) of
    true ->
      { Var, E };
    false ->
      VarMeta = lists:keyfind(var, 1, Meta),
      if
        VarMeta == { var, true } ->
          Extra = case Kind of
            nil -> "";
            _   -> io_lib:format(" (context ~ts)", [elixir_aliases:inspect(Kind)])
          end,

          compile_error(Meta, E#elixir_env.file, "expected var ~ts~ts to expand to an existing "
                        "variable or be a part of a match", [Name, Extra]);
        E#elixir_env.context == guard ->
          compile_error(Meta, E#elixir_env.file, "unknown variable ~ts or cannot invoke "
                        "function ~ts/0 inside guard", [Name, Name]);
        true ->
          expand({ Name, Meta, [] }, E)
      end
  end;

%% Local calls

expand({ Atom, Meta, Args }, E) when is_atom(Atom), is_list(Meta), is_list(Args) ->
  % assert_no_ambiguous_op(Atom, Meta, Args, S),

  elixir_exp_dispatch:dispatch_import(Meta, Atom, Args, E, fun() ->
    expand_local(Meta, Atom, Args, E)
  end);

%% Remote calls

expand({ { '.', DotMeta, [Left, Right] }, Meta, Args }, E)
    when (is_tuple(Left) orelse is_atom(Left)), is_atom(Right), is_list(Meta), is_list(Args) ->
  { ELeft, EL } = expand(Left, E),

  elixir_exp_dispatch:dispatch_require(Meta, ELeft, Right, Args, EL, fun(Receiver) ->
    expand_remote(Receiver, DotMeta, Right, Meta, Args, E, EL)
  end);

%% Anonymous calls

expand({ { '.', DotMeta, [Expr] }, Meta, Args }, E) when is_list(Args) ->
  { EExpr, EE } = expand(Expr, E),
  if
    is_atom(EExpr) ->
      compile_error(Meta, E#elixir_env.file, "invalid function call :~ts.()", [EExpr]);
    true ->
      { EArgs, EA } = expand_args(Args, elixir_env:mergea(E, EE)),
      { { { '.', DotMeta, [EExpr] }, Meta, EArgs }, elixir_env:mergev(EE, EA) }
  end;

%% Invalid calls

expand({ _, Meta, Args } = Invalid, E) when is_list(Meta) and is_list(Args) ->
  compile_error(Meta, E#elixir_env.file, "invalid call ~ts",
    ['Elixir.Macro':to_string(Invalid)]);

expand({ _, _, _ } = Tuple, E) ->
  compile_error([{line,0}], E#elixir_env.file, "invalid quoted expression: ~ts",
    ['Elixir.Kernel':inspect(Tuple, [{raw,true}])]);

%% Literals

expand({ Left, Right }, E) ->
  { [ELeft, ERight], EE } = expand_args([Left, Right], E),
  { { ELeft, ERight }, EE };

expand(List, E) when is_list(List) ->
  expand_args(List, E);

expand(Other, E) ->
  { Other, E }.

%% Helpers

expand_many(Args, E) ->
  lists:mapfoldl(fun expand/2, E, Args).

%% Variables in arguments are not propagated from one
%% argument to the other. For instance:
%%
%%   x = 1
%%   foo(x = x + 2, x)
%%   x
%%
%% Should be the same as:
%%
%%   foo(3, 1)
%%   3
%%
%% However, notice that if we are doing an assignment,
%% it behaves as regular expansion.
expand_arg(Arg, { Acc, E }) ->
  { TArg, TAcc } = expand(Arg, Acc),
  { TArg, { elixir_env:mergea(Acc, TAcc), elixir_env:mergev(E, TAcc) } }.

expand_args(Args, #elixir_env{context=match} = E) ->
  lists:mapfoldl(fun expand/2, E, Args);
expand_args(Args, E) ->
  { TArgs, { EC, EV } } = lists:mapfoldl(fun expand_arg/2, {E, E}, Args),
  { TArgs, elixir_env:mergea(EV, EC) }.

%% Match/var helpers

%% TODO: Merge this var mangling into #elixir_scope
match(Fun, Expr, #elixir_env{context=Context} = E, NE) ->
  { EExpr, EE } = Fun(Expr, (elixir_env:mergec(E, NE))#elixir_env{context=match}),
  { EExpr, (elixir_env:mergev(EE, NE))#elixir_env{context=Context} }.

var_kind(Meta, Kind) ->
  case lists:keyfind(counter, 1, Meta) of
    { counter, Counter } -> Counter;
    false -> Kind
  end.

%% Locals

expand_local(Meta, Name, Args, #elixir_env{local=nil, function=nil} = E) ->
  { { Name, Meta, Args }, E };
expand_local(Meta, Name, Args, #elixir_env{local=nil, module=Module, function=Function} = E) ->
  elixir_locals:record_local({ Name, length(Args) }, Module, Function),
  { { Name, Meta, Args }, E };
expand_local(Meta, Name, Args, E) ->
  expand({ { '.', Meta, [E#elixir_env.local, Name] }, Meta, Args }, E).

%% Remote

expand_remote(Receiver, DotMeta, Right, Meta, Args, E, EL) ->
  if
    is_atom(Receiver) -> elixir_lexical:record_remote(Receiver, E#elixir_env.lexical_tracker);
    true -> ok
  end,

  { EArgs, EA } = expand_args(Args, elixir_env:mergea(E, EL)),
  { { { '.', DotMeta, [Receiver, Right] }, Meta, EArgs }, elixir_env:mergev(EL, EA) }.

%% Lexical helpers

expand_opts(Meta, Kind, Allowed, Opts, E) ->
  { EOpts, EE } = expand(Opts, E),
  validate_opts(Meta, Kind, Allowed, EOpts, EE),
  { EOpts, EE }.

validate_opts(Meta, Kind, Allowed, Opts, E) when is_list(Opts) ->
  [begin
    compile_error(Meta, E#elixir_env.file,
                  "unsupported option ~ts given to ~s", ['Elixir.Kernel':inspect(Key), Kind])
  end || { Key, _ } <- Opts, not lists:member(Key, Allowed)];

validate_opts(Meta, Kind, _Allowed, _Opts, S) ->
  compile_error(Meta, S#elixir_scope.file, "invalid options for ~s, expected a keyword list", [Kind]).

no_alias_opts(KV) when is_list(KV) ->
  case lists:keyfind(as, 1, KV) of
    { as, As } -> lists:keystore(as, 1, KV, { as, no_alias_expansion(As) });
    false -> KV
  end;
no_alias_opts(KV) -> KV.

no_alias_expansion({ '__aliases__', Meta, [H|T] }) when (H /= 'Elixir') and is_atom(H) ->
  { '__aliases__', Meta, ['Elixir',H|T] };
no_alias_expansion(Other) ->
  Other.

expand_alias(Meta, IncludeByDefault, Ref, KV, #elixir_env{context_modules=Context} = E) ->
  New = expand_as(lists:keyfind(as, 1, KV), Meta, IncludeByDefault, Ref, E),

  %% Add the alias to context_modules if defined is true.
  %% This is used by defmodule in order to store the defined
  %% module in context modules.
  NewContext =
    case lists:keyfind(defined, 1, Meta) of
      { defined, Mod } when is_atom(Mod) -> [Mod|Context];
      false -> Context
    end,

  { Aliases, MacroAliases } = elixir_aliases:store(Meta, New, Ref, KV, E#elixir_env.aliases,
                                E#elixir_env.macro_aliases, E#elixir_env.lexical_tracker),

  { { 'alias', Meta, [Ref, KV] },
    E#elixir_env{aliases=Aliases, macro_aliases=MacroAliases, context_modules=NewContext} }.

expand_as({ as, true }, _Meta, _IncludeByDefault, Ref, _E) ->
  elixir_aliases:last(Ref);
expand_as({ as, false }, _Meta, _IncludeByDefault, Ref, _E) ->
  Ref;
expand_as({ as, Atom }, Meta, _IncludeByDefault, _Ref, E) when is_atom(Atom) ->
  case length(string:tokens(atom_to_list(Atom), ".")) of
    1 -> compile_error(Meta, E#elixir_env.file,
           "invalid :as, expected an alias, got atom: ~ts", [elixir_aliases:inspect(Atom)]);
    2 -> Atom;
    _ -> compile_error(Meta, E#elixir_env.file,
           "invalid :as, expected an alias, got nested alias: ~ts", [elixir_aliases:inspect(Atom)])
  end;
expand_as(false, _Meta, IncludeByDefault, Ref, _E) ->
  if IncludeByDefault -> elixir_aliases:last(Ref);
     true -> Ref
  end;
expand_as({ as, Other }, Meta, _IncludeByDefault, _Ref, E) ->
  compile_error(Meta, E#elixir_env.file,
    "invalid :as, expected an alias, got: ~ts", ['Elixir.Macro':to_string(Other)]).

%% Comprehensions

expand_comprehension(Meta, Kind, Args, E) ->
  case elixir_utils:split_last(Args) of
    { Cases, [{do,Expr}] } ->
      { ECases, EC } = lists:mapfoldl(fun expand_comprehension_clause/2, E, Cases),
      { EExpr, EE }  = expand(Expr, EC),
      { { Kind, Meta, ECases ++ [[{do,EExpr}]] }, elixir_env:mergec(E, EE) };
    _ ->
      compile_error(Meta, E#elixir_env.file, "missing do keyword in comprehension ~ts", [Kind])
  end.

expand_comprehension_clause({Gen, Meta, [Left, Right]}, E) when Gen == inbits; Gen == inlist ->
  { ERight, ER } = expand(Right, E),
  { ELeft, EL }  = match(fun expand/2, Left, E, ER),
  { { Gen, Meta, [ELeft, ERight] }, EL };
expand_comprehension_clause(X, E) ->
  expand(X, E).