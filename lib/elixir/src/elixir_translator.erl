%% Main entry point for translations. All macros that cannot be
%% overriden are defined in this file.
-module(elixir_translator).
-export([forms/4, 'forms!'/4]).
-export([translate/2, translate_each/2, translate_arg/2, translate_args/2]).
-import(elixir_scope, [umergev/2, umergec/2, umergea/2]).
-import(elixir_errors, [syntax_error/3, syntax_error/4,
  compile_error/3, compile_error/4,
  assert_function_scope/3, assert_module_scope/3,
  assert_no_guard_scope/3, assert_no_match_or_guard_scope/3]).
-include("elixir.hrl").

forms(String, StartLine, File, Opts) ->
  case elixir_tokenizer:tokenize(String, StartLine, [{ file, File }|Opts]) of
    { ok, _Line, Tokens } ->
      try elixir_parser:parse(Tokens) of
        { ok, Forms } -> { ok, Forms };
        { error, { Line, _, [Error, Token] } } -> { error, { Line, Error, Token } }
      catch
        { error, { Line, _, [Error, Token] } } -> { error, { Line, Error, Token } }
      end;
    { error, Reason, _Rest, _SoFar  } -> { error, Reason }
  end.

'forms!'(String, StartLine, File, Opts) ->
  case forms(String, StartLine, File, Opts) of
    { ok, Forms } ->
      Forms;
    { error, { Line, Error, Token } } ->
      elixir_errors:parse_error(Line, File, Error, Token)
  end.

translate(Forms, S) ->
  lists:mapfoldl(fun translate_each/2, S, Forms).

%% Assignment operator

translate_each({ '=', Meta, [Left, Right] }, S) ->
  assert_no_guard_scope(Meta, '=', S),
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL } = elixir_clauses:assigns(fun translate_each/2, Left, SR),
  { { match, ?line(Meta), TLeft, TRight }, SL };

%% Containers

translate_each({ C, _, _ } = Original, S) when C == '{}'; C == '<<>>' ->
  elixir_literal:translate(Original, S);

%% Blocks and scope rewriters

translate_each({ '__block__', Meta, [] }, S) ->
  { { atom, ?line(Meta), nil }, S };

translate_each({ '__block__', _Line, [Arg] }, S) ->
  translate_each(Arg, S);

translate_each({ '__block__', Meta, Args }, S) when is_list(Args) ->
  { TArgs, NS } = translate(Args, S),
  { { block, ?line(Meta), TArgs }, NS };

%% Erlang op

translate_each({ '__op__', Meta, [Op, Expr] }, S) when is_atom(Op) ->
  { TExpr, NS } = translate_each(Expr, S),
  { { op, ?line(Meta), Op, TExpr }, NS };

translate_each({ '__op__', Meta, [Op, Left, Right] }, S) when is_atom(Op) ->
  { [TLeft, TRight], NS }  = translate_args([Left, Right], S),
  { { op, ?line(Meta), Op, TLeft, TRight }, NS };

%% Lexical

translate_each({ alias, Meta, [Ref] }, S) ->
  translate_each({ alias, Meta, [Ref,[]] }, S);

translate_each({ alias, Meta, [Ref, KV] }, S) ->
  assert_no_match_or_guard_scope(Meta, alias, S),
  { TRef, SR } = translate_each(Ref, S),
  { TKV, ST }  = translate_opts(Meta, alias, [as, warn], no_alias_opts(KV), SR),

  case TRef of
    { atom, _, Old } ->
      translate_alias(Meta, true, Old, TKV, ST);
    _ ->
      compile_error(Meta, S#elixir_scope.file, "invalid args for alias, expected a compile time atom or alias as argument")
  end;

translate_each({ require, Meta, [Ref] }, S) ->
  translate_each({ require, Meta, [Ref, []] }, S);

translate_each({ require, Meta, [Ref, KV] }, S) ->
  assert_no_match_or_guard_scope(Meta, require, S),

  { TRef, SR } = translate_each(Ref, S),
  { TKV, ST }  = translate_opts(Meta, require, [as, warn], no_alias_opts(KV), SR),

  case TRef of
    { atom, _, Old } ->
      elixir_aliases:ensure_loaded(Meta, Old, ST),
      translate_require(Meta, Old, TKV, ST);
    _ ->
      compile_error(Meta, S#elixir_scope.file, "invalid args for require, expected a compile time atom or alias as argument")
  end;

translate_each({ import, Meta, [Left] }, S) ->
  translate_each({ import, Meta, [Left, []]}, S);

translate_each({ import, Meta, [Ref, KV] }, S) ->
  assert_no_match_or_guard_scope(Meta, import, S),
  { TRef, SR } = translate_each(Ref, S),
  { TKV, ST }  = translate_opts(Meta, import, [only, except, warn], KV, SR),

  case TRef of
    { atom, _, Atom } ->
      elixir_aliases:ensure_loaded(Meta, Atom, ST),
      SF = elixir_import:import(Meta, Atom, TKV, ST),
      translate_require(Meta, Atom, TKV, SF);
    _ ->
      compile_error(Meta, S#elixir_scope.file, "invalid name for import, expected a compile time atom or alias")
  end;

%% Pseudo variables

translate_each({ '__MODULE__', Meta, Atom }, S) when is_atom(Atom) ->
  { { atom, ?line(Meta), S#elixir_scope.module }, S };

translate_each({ '__FILE__', _Meta, Atom }, S) when is_atom(Atom) ->
  translate_each(S#elixir_scope.file, S);

translate_each({ '__DIR__', _Meta, Atom }, S) when is_atom(Atom) ->
  translate_each(filename:dirname(S#elixir_scope.file), S);

translate_each({ '__ENV__', Meta, Atom }, S) when is_atom(Atom) ->
  Env = elixir_env:scope_to_ex({ ?line(Meta), S }),
  { ex_env_to_erl(Env), S };

translate_each({ '__CALLER__', Meta, Atom }, S) when is_atom(Atom) ->
  { { var, ?line(Meta), '__CALLER__' }, S#elixir_scope{caller=true} };

%% Aliases

translate_each({ '__aliases__', Meta, _ } = Alias, S) ->
  case elixir_aliases:expand(Alias, S#elixir_scope.aliases, S#elixir_scope.macro_aliases, S#elixir_scope.lexical_tracker) of
    Receiver when is_atom(Receiver) ->
      elixir_lexical:record_remote(Receiver, S#elixir_scope.lexical_tracker),
      { { atom, ?line(Meta), Receiver }, S };
    Aliases ->
      { TAliases, SA } = translate_args(Aliases, S),

      case lists:all(fun is_atom_tuple/1, TAliases) of
        true ->
          Atoms = [Atom || { atom, _, Atom } <- TAliases],
          Receiver = elixir_aliases:concat(Atoms),
          elixir_lexical:record_remote(Receiver, S#elixir_scope.lexical_tracker),
          { { atom, ?line(Meta), Receiver }, SA };
        false ->
          Args = [elixir_utils:list_to_cons(?line(Meta), TAliases)],
          { ?wrap_call(?line(Meta), elixir_aliases, concat, Args), SA }
      end
  end;

%% Quoting

translate_each({ Unquote, Meta, [_|_] }, S) when Unquote == unquote; Unquote == unquote_splicing ->
  compile_error(Meta, S#elixir_scope.file, "~p called outside quote", [Unquote]);

translate_each({ quote, Meta, [Opts] }, S) when is_list(Opts) ->
  case lists:keyfind(do, 1, Opts) of
    { do, Do } ->
      translate_each({ quote, Meta, [lists:keydelete(do, 1, Opts), [{do,Do}]] }, S);
    false ->
      syntax_error(Meta, S#elixir_scope.file, "missing do keyword in quote")
  end;

translate_each({ quote, Meta, [_] }, S) ->
  syntax_error(Meta, S#elixir_scope.file, "invalid args for quote");

translate_each({ quote, Meta, [KV, Do] }, S) when is_list(Do) ->
  Exprs =
    case lists:keyfind(do, 1, Do) of
      { do, E } -> E;
      false -> syntax_error(Meta, S#elixir_scope.file, "missing do keyword in quote")
    end,

  ValidOpts   = [hygiene, context, var_context, location, line, unquote, bind_quoted],
  { TKV, ST } = translate_opts(Meta, quote, ValidOpts, KV, S),

  Hygiene = case lists:keyfind(hygiene, 1, TKV) of
    { hygiene, List } when is_list(List) ->
      List;
    false ->
      []
  end,

  Context = case lists:keyfind(context, 1, TKV) of
    { context, Atom } when is_atom(Atom) ->
      Atom;
    { context, _ } ->
      compile_error(Meta, S#elixir_scope.file, "invalid :context for quote, expected a compile time atom or an alias");
    false ->
      case S#elixir_scope.module of
        nil -> 'Elixir';
        Mod -> Mod
      end
  end,

  Vars     = lists:keyfind(vars, 1, Hygiene) /= { vars, false },
  Aliases  = lists:keyfind(aliases, 1, Hygiene) /= { aliases, false },
  Imports  = lists:keyfind(imports, 1, Hygiene) /= { imports, false },

  Keep = lists:keyfind(location, 1, TKV) == { location, keep },
  Line = proplists:get_value(line, TKV, false),

  { Binding, DefaultUnquote } = case lists:keyfind(bind_quoted, 1, TKV) of
    { bind_quoted, BQ } -> { BQ, false };
    false -> { nil, true }
  end,

  Unquote = case lists:keyfind(unquote, 1, TKV) of
    { unquote, Bool } when is_boolean(Bool) -> Bool;
    false -> DefaultUnquote
  end,

  Q = #elixir_quote{vars_hygiene=Vars, line=Line, keep=Keep, unquote=Unquote,
        aliases_hygiene=Aliases, imports_hygiene=Imports, context=Context},

  { Quoted, _Q } = elixir_quote:quote(Exprs, Binding, Q, ST),
  translate_each(Quoted, ST);

translate_each({ quote, Meta, [_, _] }, S) ->
  syntax_error(Meta, S#elixir_scope.file, "invalid args for quote");

%% Functions

translate_each({ '&', Meta, [Arg] }, S) ->
  assert_no_match_or_guard_scope(Meta, '&', S),
  elixir_fn:capture(Meta, Arg, S);

translate_each({ fn, Meta, [{ '->', _, Pairs }] }, S) ->
  assert_no_match_or_guard_scope(Meta, 'fn', S),
  elixir_fn:fn(Meta, Pairs, S);

%% Case

translate_each({'case', Meta, [Expr, KV]}, S) when is_list(KV) ->
  assert_no_match_or_guard_scope(Meta, 'case', S),
  Clauses = elixir_clauses:get_pairs(Meta, do, KV, S),
  { TExpr, NS } = translate_each(Expr, S),

  RClauses = case elixir_utils:returns_boolean(TExpr) of
    true  -> rewrite_case_clauses(Clauses);
    false -> Clauses
  end,

  { TClauses, TS } = elixir_clauses:match(Meta, RClauses, NS),
  { { 'case', ?line(Meta), TExpr, TClauses }, TS };

%% Try

translate_each({'try', Meta, [Clauses]}, S) when is_list(Clauses) ->
  assert_no_match_or_guard_scope(Meta, 'try', S),
  Do = proplists:get_value('do', Clauses, nil),
  { TDo, SB } = elixir_translator:translate_each(Do, S#elixir_scope{noname=true}),

  Catch = [Tuple || { X, _ } = Tuple <- Clauses, X == 'rescue' orelse X == 'catch'],
  { TCatch, SC } = elixir_try:clauses(Meta, Catch, umergea(S, SB)),

  After = proplists:get_value('after', Clauses, nil),
  { TAfter, SA } = translate_each(After, umergea(S, SC)),

  Else = elixir_clauses:get_pairs(Meta, else, Clauses, S),
  { TElse, SE } = elixir_clauses:match(Meta, Else, umergea(S, SA)),

  SF = (umergec(S, SE))#elixir_scope{noname=S#elixir_scope.noname},
  { { 'try', ?line(Meta), pack(TDo), TElse, TCatch, pack(TAfter) }, SF };

%% Receive

translate_each({'receive', Meta, [KV] }, S) when is_list(KV) ->
  assert_no_match_or_guard_scope(Meta, 'receive', S),
  Do = elixir_clauses:get_pairs(Meta, do, KV, S, true),

  case lists:keyfind('after', 1, KV) of
    false ->
      { TClauses, SC } = elixir_clauses:match(Meta, Do, S),
      { { 'receive', ?line(Meta), TClauses }, SC };
    _ ->
      After = elixir_clauses:get_pairs(Meta, 'after', KV, S),
      { TClauses, SC } = elixir_clauses:match(Meta, Do ++ After, S),
      { FClauses, TAfter } = elixir_utils:split_last(TClauses),
      { _, _, [FExpr], _, FAfter } = TAfter,
      { { 'receive', ?line(Meta), FClauses, FExpr, FAfter }, SC }
  end;

%% Comprehensions

translate_each({ Kind, Meta, Args }, S) when is_list(Args), (Kind == lc) orelse (Kind == bc) ->
  translate_comprehension(Meta, Kind, Args, S);

%% Super

translate_each({ super, Meta, Args }, S) when is_list(Args) ->
  assert_no_match_or_guard_scope(Meta, super, S),
  Module = assert_module_scope(Meta, super, S),
  Function = assert_function_scope(Meta, super, S),
  elixir_def_overridable:ensure_defined(Meta, Module, Function, S),

  { _, Arity } = Function,

  { TArgs, TS } = if
    length(Args) == Arity ->
      translate_args(Args, S);
    true ->
      syntax_error(Meta, S#elixir_scope.file, "super must be called with the same number of "
                   "arguments as the current function")
  end,

  Super = elixir_def_overridable:name(Module, Function),
  { { call, ?line(Meta), { atom, ?line(Meta), Super }, TArgs }, TS#elixir_scope{super=true} };

%% Variables

translate_each({ '^', Meta, [ { Name, VarMeta, Kind } = Var ] },
               #elixir_scope{extra=fn_match, extra_guards=Extra} = S) when is_atom(Name), is_atom(Kind) ->
  case orddict:find({ Name, var_kind(VarMeta, Kind) }, S#elixir_scope.backup_vars) of
    { ok, Value } ->
      Line = ?line(Meta),
      { TVar, TS } = translate_each(Var, S),
      Guard = { op, Line, '=:=', { var, ?line(Meta), Value }, TVar },
      { TVar, TS#elixir_scope{extra_guards=[Guard|Extra]} };
    error ->
      compile_error(Meta, S#elixir_scope.file, "unbound variable ^~ts", [Name])
  end;

translate_each({ '^', Meta, [ { Name, VarMeta, Kind } ] }, #elixir_scope{context=match} = S) when is_atom(Name), is_atom(Kind) ->
  case orddict:find({ Name, var_kind(VarMeta, Kind) }, S#elixir_scope.backup_vars) of
    { ok, Value } ->
      { { var, ?line(Meta), Value }, S };
    error ->
      compile_error(Meta, S#elixir_scope.file, "unbound variable ^~ts", [Name])
  end;

translate_each({ '^', Meta, [ { Name, _, Kind } ] }, S) when is_atom(Name), is_atom(Kind) ->
  compile_error(Meta, S#elixir_scope.file,
    "cannot use ^~ts outside of match clauses", [Name]);

translate_each({ '^', Meta, [ Expr ] }, S) ->
  syntax_error(Meta, S#elixir_scope.file,
    "the unary operator ^ can only be used with variables, invalid expression ^~ts", ['Elixir.Macro':to_string(Expr)]);

translate_each({ Name, Meta, Kind }, S) when is_atom(Name), is_atom(Kind) ->
  elixir_scope:translate_var(Meta, Name, var_kind(Meta, Kind), S, fun() ->
    case lists:keyfind(var, 1, Meta) of
      { var, true } ->
        compile_error(Meta, S#elixir_scope.file, "expected var ~ts to expand to an existing "
                      "variable or be a part of a match", [Name]);
      _ ->
        translate_each({ Name, Meta, [] }, S)
    end
  end);

%% Local calls

translate_each({ '->', Meta, _Args }, S) ->
  syntax_error(Meta, S#elixir_scope.file, "unhandled operator ->");

translate_each({ Atom, Meta, Args }, S) when is_atom(Atom), is_list(Meta), is_list(Args) ->
  assert_no_ambiguous_op(Atom, Meta, Args, S),

  Callback = fun() ->
    case S#elixir_scope.context of
      match ->
        compile_error(Meta, S#elixir_scope.file,
                      "cannot invoke function ~ts/~B inside match", [Atom, length(Args)]);
      guard ->
        Arity = length(Args),
        File  = S#elixir_scope.file,
        case Arity of
          0 -> compile_error(Meta, File, "unknown variable ~ts or cannot invoke "
                             "function ~ts/~B inside guard", [Atom, Atom, Arity]);
          _ -> compile_error(Meta, File, "cannot invoke local ~ts/~B inside guard",
                             [Atom, Arity])
        end;
      _ ->
        translate_local(Meta, Atom, Args, S)
    end
  end,

  elixir_dispatch:dispatch_import(Meta, Atom, Args, S, Callback);

%% Remote calls

translate_each({ { '.', _, [Left, Right] }, Meta, Args }, S)
    when (is_tuple(Left) orelse is_atom(Left)), is_atom(Right), is_list(Meta), is_list(Args) ->
  { TLeft,  SL } = translate_each(Left, S),
  { TRight, SR } = translate_each(Right, umergec(S, SL)),

  Callback = fun() ->
    case TLeft of
      { atom, _, Receiver } ->
        Tuple = { element(3, TRight), length(Args) },
        elixir_lexical:record_remote(Receiver, S#elixir_scope.lexical_tracker),
        elixir_tracker:record_remote(Tuple, Receiver, S#elixir_scope.module, S#elixir_scope.function);
      _ ->
        ok
    end,

    Line = ?line(Meta),
    { TArgs, SA } = translate_args(Args, umergec(S, SR)),
    { { call, Line, { remote, Line, TLeft, TRight }, TArgs }, umergev(SL, umergev(SR, SA)) }
  end,

  case TLeft of
    { atom, _, Receiver } ->
      elixir_dispatch:dispatch_require(Meta, Receiver, Right, Args, umergev(SL, SR), fun() ->
        case S#elixir_scope.context of
          Context when Receiver /= erlang, (Context == match) orelse (Context == guard) ->
            compile_error(Meta, S#elixir_scope.file, "cannot invoke remote function ~ts.~ts/~B inside ~ts",
              [elixir_errors:inspect(Receiver), Right, length(Args), Context]);
          _ ->
            Callback()
        end
      end);
    _ ->
      case S#elixir_scope.context of
        Context when Context == match; Context == guard ->
          compile_error(Meta, S#elixir_scope.file, "cannot invoke remote function ~ts/~B inside ~ts",
            [Right, length(Args), Context]);
        _ ->
          Callback()
      end
  end;

%% Anonymous function calls

translate_each({ { '.', _, [Expr] }, Meta, Args }, S) when is_list(Args) ->
  { TExpr, SE } = translate_each(Expr, S),
  case TExpr of
    { atom, _, Atom } ->
      syntax_error(Meta, S#elixir_scope.file, "invalid function call :~ts.()", [Atom]);
    _ ->
      { TArgs, SA } = translate_args(Args, umergec(S, SE)),
      { {call, ?line(Meta), TExpr, TArgs}, umergev(SE, SA) }
  end;

%% Invalid calls

translate_each({ { '.', _, [Invalid, _] }, Meta, Args }, S) when is_list(Meta) and is_list(Args) ->
  syntax_error(Meta, S#elixir_scope.file, "invalid remote call on ~ts",
    ['Elixir.Macro':to_string(Invalid)]);

translate_each({ _, Meta, Args } = Invalid, S) when is_list(Meta) and is_list(Args) ->
  syntax_error(Meta, S#elixir_scope.file, "invalid call ~ts",
    ['Elixir.Macro':to_string(Invalid)]);

translate_each({ _, _, _ } = Tuple, S) ->
  syntax_error([{line,0}], S#elixir_scope.file, "expected a valid quoted expression, got: ~ts",
    ['Elixir.Kernel':inspect(Tuple, [{raw,true}])]);

%% Literals

translate_each(Literal, S) ->
  elixir_literal:translate(Literal, S).

%% Helpers

var_kind(Meta, Kind) ->
  case lists:keyfind(counter, 1, Meta) of
    { counter, Counter } -> Counter;
    false -> Kind
  end.

ex_env_to_erl(Structure) ->
  elixir_utils:elixir_to_erl(Structure, fun
    (X) when is_pid(X) ->
      ?wrap_call(0, erlang, binary_to_term, [elixir_utils:elixir_to_erl(term_to_binary(X))]);
    (Other) ->
      error({ badarg, Other })
  end).

%% Case

%% TODO: Once we have elixir_exp, we can move this
%% clause to Elixir code and out of case.
rewrite_case_clauses([
    {do,Meta1,[{'when',_,[{V,M,C},{in,_,[{V,M,C},[false,nil]]}]}],False},
    {do,Meta2,[{'_',_,UC}],True}] = Clauses)
    when is_atom(V), is_list(M), is_atom(C), is_atom(UC) ->
  case lists:keyfind('cond', 1, M) of
    { 'cond', true } ->
      [{do,Meta1,[false],False},{do,Meta2,[true],True}];
    _ ->
      Clauses
  end;
rewrite_case_clauses(Clauses) ->
  Clauses.

% Pack a list of expressions from a block.
pack({ 'block', _, Exprs }) -> Exprs;
pack(Expr)                  -> [Expr].

%% Opts

translate_opts(Meta, Kind, Allowed, Opts, S) ->
  { Expanded, TS } = case literal_opts(Opts) orelse skip_expansion(S#elixir_scope.module) of
    true  -> { Opts, S };
    false -> 'Elixir.Macro':expand_all(Opts, elixir_env:scope_to_ex({ ?line(Meta), S }), S)
  end,
  validate_opts(Meta, Kind, Allowed, Expanded, TS),
  { Expanded, TS }.

skip_expansion('Elixir.Kernel') -> true;
skip_expansion('Elixir.Kernel.Typespec') -> true;
skip_expansion(_) -> false.

literal_opts({ X, Y }) -> literal_opts(X) andalso literal_opts(Y);
literal_opts(Opts) when is_list(Opts) -> lists:all(fun literal_opts/1, Opts);
literal_opts(Opts) when is_atom(Opts); is_number(Opts); is_bitstring(Opts) -> true;
literal_opts(_) -> false.

validate_opts(Meta, Kind, Allowed, Opts, S) when is_list(Opts) ->
  [begin
    compile_error(Meta, S#elixir_scope.file,
                  "unsupported option ~ts given to ~s", ['Elixir.Kernel':inspect(Key), Kind])
  end || { Key, _ } <- Opts, not lists:member(Key, Allowed)];

validate_opts(Meta, Kind, _Allowed, _Opts, S) ->
  compile_error(Meta, S#elixir_scope.file, "invalid options for ~s, expected a keyword list", [Kind]).

%% Require

translate_require(Meta, Old, TKV, S) ->
  SF = S#elixir_scope{
    requires=ordsets:add_element(Old, S#elixir_scope.requires)
  },
  translate_alias(Meta, false, Old, TKV, SF).

%% Aliases

translate_alias(Meta, IncludeByDefault, Old, TKV, #elixir_scope{context_modules=Context} = S) ->
  New = case lists:keyfind(as, 1, TKV) of
    { as, true } ->
      elixir_aliases:last(Old);
    { as, false } ->
      Old;
    { as, Atom } when is_atom(Atom) ->
      Atom;
    false ->
      if
        IncludeByDefault -> elixir_aliases:last(Old);
        true -> Old
      end;
    _ ->
      compile_error(Meta, S#elixir_scope.file,
        "invalid :as for alias, expected a compile time atom or alias")
  end,

  case (New == Old) orelse (length(string:tokens(atom_to_list(New), ".")) == 2) of
    true  -> ok;
    false -> compile_error(Meta, S#elixir_scope.file,
               "invalid :as for alias, nested alias ~s not allowed", [elixir_errors:inspect(New)])
  end,

  %% Add the alias to context_modules if defined is true.
  %% This is used by defmodule.
  NewContext =
    case lists:keyfind(defined, 1, Meta) of
      { defined, Mod } when is_atom(Mod) -> [Mod|Context];
      false -> Context
    end,

  { Aliases, MacroAliases } = elixir_aliases:store(Meta, New, Old, TKV, S#elixir_scope.aliases,
                                S#elixir_scope.macro_aliases, S#elixir_scope.lexical_tracker),

  { { atom, ?line(Meta), nil },
    S#elixir_scope{aliases=Aliases, macro_aliases=MacroAliases, context_modules=NewContext} }.

no_alias_opts(KV) when is_list(KV) ->
  case lists:keyfind(as, 1, KV) of
    { as, As } -> lists:keystore(as, 1, KV, { as, no_alias_expansion(As) });
    false -> KV
  end;
no_alias_opts(KV) -> KV.

%% Do not allow expansion of the alias itself.
no_alias_expansion({ '__aliases__', Meta, [H|T] }) when (H /= 'Elixir') and is_atom(H) ->
  { '__aliases__', Meta, ['Elixir',H|T] };
no_alias_expansion(Other) ->
  Other.

is_atom_tuple({ atom, _, _ }) -> true;
is_atom_tuple(_) -> false.

%% Locals

translate_local(Meta, Name, Args, #elixir_scope{local=nil,function=nil} = S) ->
  compile_error(Meta, S#elixir_scope.file, "function ~ts/~B undefined", [Name, length(Args)]);

translate_local(Meta, Name, Args, #elixir_scope{local=nil,module=Module,function=Function} = S) ->
  elixir_tracker:record_local({ Name, length(Args) }, Module, Function),
  Line = ?line(Meta),
  { TArgs, NS } = translate_args(Args, S),
  { { call, Line, { atom, Line, Name }, TArgs }, NS };

translate_local(Meta, Name, Args, S) ->
  translate_each({ { '.', Meta, [S#elixir_scope.local, Name] }, Meta, Args }, S).

%% Translate args

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
%% it behaves the same as translate.

translate_arg(Arg, { Acc, S }) ->
  { TArg, TAcc } = translate_each(Arg, Acc),
  { TArg, { umergea(Acc, TAcc), umergev(S, TAcc) } }.

translate_args(Args, #elixir_scope{context=match} = S) ->
  translate(Args, S);

translate_args(Args, S) ->
  { TArgs, { SC, SV } } = lists:mapfoldl(fun translate_arg/2, {S, S}, Args),
  { TArgs, umergea(SV, SC) }.

%% __op__ helpers

assert_no_ambiguous_op(Name, Meta, [Arg], S) ->
  case lists:keyfind(ambiguous_op, 1, Meta) of
    { ambiguous_op, Kind } ->
      case orddict:find({ Name, Kind }, S#elixir_scope.vars) of
        error -> ok;
        _ ->
          syntax_error(Meta, S#elixir_scope.file, "\"~ts ~ts\" looks like a function call but "
                       "there is a variable named \"~ts\", please use explicit parenthesis or even spaces",
                       [Name, 'Elixir.Macro':to_string(Arg), Name])
      end;
    _ -> ok
  end;

assert_no_ambiguous_op(_Atom, _Meta, _Args, _S) -> ok.

%% Comprehensions

translate_comprehension(Meta, Kind, Args, S) ->
  case elixir_utils:split_last(Args) of
    { Cases, [{do,Expr}] } ->
      { TCases, SC } = lists:mapfoldl(fun(C, Acc) -> translate_comprehension_clause(Meta, C, Acc) end, S, Cases),
      { TExpr, SE }  = translate_comprehension_do(Meta, Kind, Expr, SC),
      { { Kind, ?line(Meta), TExpr, TCases }, umergec(S, SE) };
    _ ->
      syntax_error(Meta, S#elixir_scope.file, "missing do keyword in comprehension ~s", [Kind])
  end.

translate_comprehension_do(_Meta, bc, { '<<>>', _, _ } = Expr, S) ->
  translate_each(Expr, S);

translate_comprehension_do(Meta, bc, _Expr, S) ->
  syntax_error(Meta, S#elixir_scope.file, "a bit comprehension expects a bit string << >> to be returned");

translate_comprehension_do(_Meta, _Kind, Expr, S) ->
  translate_each(Expr, S).

translate_comprehension_clause(_Meta, {inbits, Meta, [{ '<<>>', _, _} = Left, Right]}, S) ->
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL  } = elixir_clauses:assigns(fun elixir_translator:translate_each/2, Left, SR),
  { { b_generate, ?line(Meta), TLeft, TRight }, SL };

translate_comprehension_clause(_Meta, {inbits, Meta, [_Left, _Right]}, S) ->
  syntax_error(Meta, S#elixir_scope.file, "a bit comprehension expects a bit string << >> to be used in inbits generators");

translate_comprehension_clause(_Meta, {inlist, Meta, [Left, Right]}, S) ->
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL  } = elixir_clauses:assigns(fun elixir_translator:translate_each/2, Left, SR),
  { { generate, ?line(Meta), TLeft, TRight }, SL };

translate_comprehension_clause(Meta, X, S) ->
  Line = ?line(Meta),
  { TX, TS } = translate_each(X, S),
  { BX, BS } = elixir_utils:convert_to_boolean(Line, TX, true, TS),
  { { match, Line, { var, Line, '_' }, BX }, BS }.
