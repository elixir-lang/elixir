%% Those macros behave like they belong to Kernel,
%% but do not since they need to be implemented in Erlang.
-module(elixir_macros).
-export([translate/2]).
-import(elixir_translator, [translate_each/2, translate_args/2, translate_apply/7]).
-import(elixir_scope, [umergec/2, umergea/2]).
-import(elixir_errors, [compile_error/3, compile_error/4,
  syntax_error/3, syntax_error/4, assert_no_function_scope/3,
  assert_module_scope/3, assert_no_match_or_guard_scope/3]).

-include("elixir.hrl").
-define(opt_in_types(Kind), Kind == atom orelse Kind == integer orelse Kind == float).

%% Operators

translate({ '+', _Meta, [Expr] }, S) when is_number(Expr) ->
  translate_each(Expr, S);

translate({ '-', _Meta, [Expr] }, S) when is_number(Expr) ->
  translate_each(-1 * Expr, S);

translate({ Op, Meta, Exprs }, S) when is_list(Exprs),
    Op == '<-' orelse Op == '--' ->
  assert_no_match_or_guard_scope(Meta, Op, S),
  translate_each({ '__op__', Meta, [Op|Exprs] }, S);

translate({ Op, Meta, Exprs }, S) when is_list(Exprs),
    Op == '+'   orelse Op == '-'   orelse Op == '*'   orelse Op == '/' orelse
    Op == '++'  orelse Op == 'not' orelse Op == 'and' orelse Op == 'or' orelse
    Op == 'xor' orelse Op == '<'   orelse Op == '>'   orelse Op == '<=' orelse
    Op == '>='  orelse Op == '=='  orelse Op == '!='  orelse Op == '===' orelse
    Op == '!==' ->
  translate_each({ '__op__', Meta, [Op|Exprs] }, S);

translate({ '!', Meta, [{ '!', _, [Expr] }] }, S) ->
  { TExpr, SE } = translate_each(Expr, S),
  elixir_tree_helpers:convert_to_boolean(?line(Meta), TExpr, true, S#elixir_scope.context == guard, SE);

translate({ '!', Meta, [Expr] }, S) ->
  { TExpr, SE } = translate_each(Expr, S),
  elixir_tree_helpers:convert_to_boolean(?line(Meta), TExpr, false, S#elixir_scope.context == guard, SE);

translate({ in, Meta, [Left, Right] }, #elixir_scope{extra_guards=nil} = S) ->
  { _, TExpr, TS } = translate_in(Meta, Left, Right, S),
  { TExpr, TS };

translate({ in, Meta, [Left, Right] }, #elixir_scope{extra_guards=Extra} = S) ->
  { TVar, TExpr, TS } = translate_in(Meta, Left, Right, S),
  { TVar, TS#elixir_scope{extra_guards=[TExpr|Extra]} };

%% Functions
%% Once this function is removed, the related checks from quote needs to be removed too.
%% We also need to remove it from the Kernel in erlang list.

translate({ function, Meta, [[{do,{ '->',_,Pairs}}]] }, S) ->
  elixir_errors:deprecation(Meta, S#elixir_scope.file, "function do ... end is deprecated, please use fn ... end instead"),
  assert_no_match_or_guard_scope(Meta, 'function', S),
  elixir_fn:fn(Meta, Pairs, S);

translate({ function, _, [{ '/', _, [{{ '.', Meta, [M, F] }, _ , []}, A]}] }, S) when is_atom(F), is_integer(A) ->
  elixir_errors:deprecation(Meta, S#elixir_scope.file, "function(Mod.fun/a) is deprecated, please use &Mod.fun/a instead"),
  assert_no_match_or_guard_scope(Meta, 'function', S),
  { [A0,B0,C0], SA } = translate_args([M, F, A], S),
  { { 'fun', ?line(Meta), { function, A0, B0, C0 } }, SA };

translate({ function, MetaFA, [{ '/', _, [{F, Meta, C}, A]}] }, S) when is_atom(F), is_integer(A), is_atom(C) ->
  elixir_errors:deprecation(Meta, S#elixir_scope.file, "function(fun/a) is deprecated, please use &fun/a instead"),
  assert_no_match_or_guard_scope(Meta, 'function', S),

  WrappedMeta =
    case lists:keyfind(import_fa, 1, MetaFA) of
      { import_fa, { Receiver, Context } } ->
        lists:keystore(context, 1,
          lists:keystore(import, 1, Meta, { import, Receiver }),
          { context, Context }
        );
      false -> Meta
    end,

  case elixir_dispatch:import_function(WrappedMeta, F, A, S) of
    false -> compile_error(WrappedMeta, S#elixir_scope.file,
                           "expected ~ts/~B to be a function, but it is a macro", [F, A]);
    Else  -> Else
  end;

translate({ function, Meta, [Arg] }, S) ->
  assert_no_match_or_guard_scope(Meta, 'function', S),
  syntax_error(Meta, S#elixir_scope.file, "invalid args for function/1: ~ts",
               ['Elixir.Macro':to_string(Arg)]);

translate({ function, Meta, [_,_,_] = Args }, S) when is_list(Args) ->
  elixir_errors:deprecation(Meta, S#elixir_scope.file, "function/3 is deprecated, please use Module.function/3 instead"),
  assert_no_match_or_guard_scope(Meta, 'function', S),
  { [A,B,C], SA } = translate_args(Args, S),
  { { 'fun', ?line(Meta), { function, A, B, C } }, SA };

%% @

translate({'@', Meta, [{ Name, _, [Arg] }]}, S) when Name == typep; Name == type; Name == spec; Name == callback; Name == opaque ->
  case elixir_compiler:get_opt(internal) of
    true  -> { { nil, ?line(Meta) }, S };
    false ->
      Call = { { '.', Meta, ['Elixir.Kernel.Typespec', spec_to_macro(Name)] }, Meta, [Arg] },
      translate_each(Call, S)
  end;

translate({'@', Meta, [{ Name, _, Args }]}, S) ->
  assert_module_scope(Meta, '@', S),

  case is_reserved_data(Name) andalso elixir_compiler:get_opt(internal) of
    true ->
      { { nil, ?line(Meta) }, S };
    _ ->
      case Args of
        [Arg] ->
          case S#elixir_scope.function of
            nil ->
              translate_each({
                { '.', Meta, ['Elixir.Module', put_attribute] },
                  Meta,
                  [ { '__MODULE__', Meta, false }, Name, Arg ]
              }, S);
            _  ->
              syntax_error(Meta, S#elixir_scope.file,
                "cannot dynamically set attribute @~ts inside a function", [Name])
          end;
        _ when is_atom(Args) or (Args == []) ->
          case S#elixir_scope.function of
            nil ->
              translate_each({
                { '.', Meta, ['Elixir.Module', get_attribute] },
                Meta,
                [ { '__MODULE__', Meta, false }, Name, true ]
              }, S);
            _ ->
              Contents = 'Elixir.Module':get_attribute(S#elixir_scope.module, Name),
              { elixir_tree_helpers:elixir_to_erl(Contents), S }
          end;
        _ ->
          syntax_error(Meta, S#elixir_scope.file, "expected 0 or 1 argument for @~ts, got: ~p", [Name, length(Args)])
      end
  end;

%% Case

translate({'case', Meta, [Expr, KV]}, S) when is_list(KV) ->
  assert_no_match_or_guard_scope(Meta, 'case', S),
  Clauses = elixir_clauses:get_pairs(Meta, do, KV, S),
  { TExpr, NS } = translate_each(Expr, S),

  RClauses = case elixir_tree_helpers:returns_boolean(TExpr) of
    true  -> rewrite_case_clauses(Clauses);
    false -> Clauses
  end,

  { TClauses, TS } = elixir_clauses:match(Meta, RClauses, NS),
  { { 'case', ?line(Meta), TExpr, TClauses }, TS };

%% Try

translate({'try', Meta, [Clauses]}, S) when is_list(Clauses) ->
  assert_no_match_or_guard_scope(Meta, 'try', S),

  Do = proplists:get_value('do', Clauses, nil),
  { TDo, SB } = elixir_translator:translate_each(Do, S#elixir_scope{noname=true}),

  Catch = [Tuple || { X, _ } = Tuple <- Clauses, X == 'rescue' orelse X == 'catch'],
  { TCatch, SC } = elixir_try:clauses(Meta, Catch, umergea(S, SB)),

  After = proplists:get_value('after', Clauses, nil),
  { TAfter, SA } = elixir_translator:translate_each(After, umergea(S, SC)),

  Else = elixir_clauses:get_pairs(Meta, else, Clauses, S),
  { TElse, SE } = elixir_clauses:match(Meta, Else, umergea(S, SA)),

  SF = (umergec(S, SE))#elixir_scope{noname=S#elixir_scope.noname},
  { { 'try', ?line(Meta), pack(TDo), TElse, TCatch, pack(TAfter) }, SF };

%% Receive

translate({'receive', Meta, [KV] }, S) when is_list(KV) ->
  assert_no_match_or_guard_scope(Meta, 'receive', S),
  Do = elixir_clauses:get_pairs(Meta, do, KV, S, true),

  case lists:keyfind('after', 1, KV) of
    false ->
      { TClauses, SC } = elixir_clauses:match(Meta, Do, S),
      { { 'receive', ?line(Meta), TClauses }, SC };
    _ ->
      After = elixir_clauses:get_pairs(Meta, 'after', KV, S),
      { TClauses, SC } = elixir_clauses:match(Meta, Do ++ After, S),
      { FClauses, TAfter } = elixir_tree_helpers:split_last(TClauses),
      { _, _, [FExpr], _, FAfter } = TAfter,
      { { 'receive', ?line(Meta), FClauses, FExpr, FAfter }, SC }
  end;

%% Definitions

translate({defmodule, Meta, [Ref, KV]}, S) when is_list(KV) ->
  { TRef, _ } = translate_each(Ref, S),

  Block = case lists:keyfind(do, 1, KV) of
    { do, DoValue } -> DoValue;
    false -> syntax_error(Meta, S#elixir_scope.file, "missing do keyword in defmodule")
  end,

  { FRef, FS } = case TRef of
    { atom, _, Module } ->
      FullModule = expand_module(Ref, Module, S),

      RS = case elixir_aliases:nesting_alias(S#elixir_scope.module, FullModule) of
        { New, Old } -> elixir_aliases:store(Meta, New, Old, S);
        false -> S
      end,

      {
        { atom, Meta, FullModule },
        RS#elixir_scope{context_modules=[FullModule|S#elixir_scope.context_modules]}
      };
    _ ->
      { TRef, S }
  end,

  MS = FS#elixir_scope{local=nil},
  { elixir_module:translate(Meta, FRef, Block, MS), FS };

translate({Kind, Meta, [Call]}, S) when ?defs(Kind) ->
  translate({Kind, Meta, [Call, nil]}, S);

translate({Kind, Meta, [Call, Expr]}, S) when ?defs(Kind) ->
  assert_module_scope(Meta, Kind, S),
  assert_no_function_scope(Meta, Kind, S),
  { TCall, QC, SC } = elixir_quote:erl_escape(Call, true, S),
  { TExpr, QE, SE } = elixir_quote:erl_escape(Expr, true, SC),
  CheckClauses = (not lists:keymember(context, 1, Meta)) andalso
                   (not QC#elixir_quote.unquoted) andalso (not QE#elixir_quote.unquoted),
  { elixir_def:wrap_definition(Kind, Meta, TCall, TExpr, CheckClauses, SE), SE };

translate({Kind, Meta, [Name, Args, Guards, Expr]}, S) when ?defs(Kind) ->
  assert_module_scope(Meta, Kind, S),
  assert_no_function_scope(Meta, Kind, S),
  { TName, SN }   = translate_each(Name, S),
  { TArgs, SA }   = translate_each(Args, SN),
  { TGuards, SG } = translate_each(Guards, SA),
  { TExpr, SE }   = translate_each(Expr, SG),
  { elixir_def:wrap_definition(Kind, Meta, TName, TArgs, TGuards, TExpr, false, SE), SE };

%% Apply - Optimize apply by checking what doesn't need to be dispatched dynamically

translate({ apply, Meta, [Left, Right, Args] }, S) when is_list(Args) ->
  { TLeft,  SL } = translate_each(Left, S),
  { TRight, SR } = translate_each(Right, umergec(S, SL)),
  translate_apply(Meta, TLeft, TRight, Args, S, SL, SR);

translate({ apply, Meta, Args }, S) ->
  { TArgs, NS } = translate_args(Args, S),
  { ?wrap_call(?line(Meta), erlang, apply, TArgs), NS };

translate({ Name, Meta, Args }, S) ->
  syntax_error(Meta, S#elixir_scope.file,
               "invalid arguments for macro ~ts/~B", [Name, length(Args)]).

%% Helpers

translate_in(Meta, Left, Right, S) ->
  Line = ?line(Meta),

  { TLeft, SL } = case Left of
    { '_', _, Atom } when is_atom(Atom) ->
      elixir_scope:build_erl_var(Line, S);
    _ ->
      translate_each(Left, S)
  end,

  { TRight, SR } = translate_each(Right, SL),

  Cache = (S#elixir_scope.context == nil),

  { Var, SV } = case Cache of
    true  -> elixir_scope:build_erl_var(Line, SR);
    false -> { TLeft, SR }
  end,

  { TCache, TExpr } = case TRight of
    { nil, _ } ->
      Expr = { atom, Line, false },
      { Cache, Expr };
    { cons, _, _, _ } ->
      [H|T] = elixir_tree_helpers:cons_to_list(TRight),
      Expr = lists:foldr(fun(X, Acc) ->
        { op, Line, 'orelse', { op, Line, '==', Var, X }, Acc }
      end, { op, Line, '==', Var, H }, T),
      { Cache, Expr };
    { string, _, [H|T] } ->
      Expr = lists:foldl(fun(X, Acc) ->
        { op, Line, 'orelse', { op, Line, '==', Var, { integer, Line, X } }, Acc }
      end, { op, Line, '==', Var, { integer, Line, H } }, T),
      { Cache, Expr };
    { tuple, _, [{ atom, _, 'Elixir.Range' }, Start, End] } ->
      Expr = case { Start, End } of
        { { K1, _, StartInt }, { K2, _, EndInt } } when ?opt_in_types(K1), ?opt_in_types(K2), StartInt =< EndInt ->
          increasing_compare(Line, Var, Start, End);
        { { K1, _, _ }, { K2, _, _ } } when ?opt_in_types(K1), ?opt_in_types(K2) ->
          decreasing_compare(Line, Var, Start, End);
        _ ->
          { op, Line, 'orelse',
            { op, Line, 'andalso',
              { op, Line, '=<', Start, End},
              increasing_compare(Line, Var, Start, End) },
            { op, Line, 'andalso',
              { op, Line, '<', End, Start},
              decreasing_compare(Line, Var, Start, End) } }
      end,
      { Cache, Expr };
    _ ->
      case Cache of
        true ->
          { false, ?wrap_call(Line, 'Elixir.Enum', 'member?', [TRight, TLeft]) };
        false ->
          compile_error(Meta, S#elixir_scope.file, "invalid args for operator in, it expects a "
                        "compile time list or range on the right side when used in guard expressions")
      end
  end,

  case TCache of
    true  -> { Var, { block, Line, [ { match, Line, Var, TLeft }, TExpr ] }, SV };
    false -> { Var, TExpr, SV }
  end.

increasing_compare(Line, Var, Start, End) ->
  { op, Line, 'andalso',
    { op, Line, '>=', Var, Start },
    { op, Line, '=<', Var, End } }.

decreasing_compare(Line, Var, Start, End) ->
  { op, Line, 'andalso',
    { op, Line, '=<', Var, Start },
    { op, Line, '>=', Var, End } }.

rewrite_case_clauses([{do,Meta1,[{in,_,[{'_',_,_},[false,nil]]}],False},{do,Meta2,[{'_',_,_}],True}]) ->
  [{do,Meta1,[false],False},{do,Meta2,[true],True}];

rewrite_case_clauses(Clauses) ->
  Clauses.

%% defmodule :foo
expand_module(Raw, _Module, _S) when is_atom(Raw) ->
  Raw;

%% defmodule Hello
expand_module({ '__aliases__', _, [H] }, _Module, S) ->
  elixir_aliases:concat([S#elixir_scope.module, H]);

%% defmodule Hello.World
expand_module({ '__aliases__', _, _ } = Alias, Module, S) ->
  case elixir_aliases:expand(Alias, S#elixir_scope.aliases, S#elixir_scope.macro_aliases) of
    Atom when is_atom(Atom) ->
      Module;
    Aliases when is_list(Aliases) ->
      elixir_aliases:concat([S#elixir_scope.module, Module])
  end;

%% defmodule Elixir.Hello.World
expand_module(_Raw, Module, S) ->
  elixir_aliases:concat([S#elixir_scope.module, Module]).

is_reserved_data(moduledoc) -> true;
is_reserved_data(doc)       -> true;
is_reserved_data(_)         -> false.

spec_to_macro(type)     -> deftype;
spec_to_macro(typep)    -> deftypep;
spec_to_macro(opaque)   -> defopaque;
spec_to_macro(spec)     -> defspec;
spec_to_macro(callback) -> defcallback.

% Pack a list of expressions from a block.
pack({ 'block', _, Exprs }) -> Exprs;
pack(Expr)                  -> [Expr].
