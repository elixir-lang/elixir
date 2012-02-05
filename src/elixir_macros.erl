%% Those macros behave like they belong to Elixir::Builtin,
%% but do not since they need to be implemented in Erlang.
-module(elixir_macros).
-export([translate_macro/2]).
-import(elixir_translator, [translate_each/2, translate/2, translate_args/2, translate_apply/7]).
-import(elixir_variables, [umergec/2, umergev/2]).
-import(elixir_errors, [syntax_error/3, syntax_error/4]).
-include("elixir.hrl").

%% Operators

translate_macro({ '+', _Line, [Expr] }, S) when is_number(Expr) ->
  translate_each(Expr, S);

translate_macro({ '-', _Line, [Expr] }, S) when is_number(Expr) ->
  translate_each(-1 * Expr, S);

translate_macro({ Op, Line, Exprs }, S) when is_list(Exprs),
  Op == '+'; Op == '-'; Op == '*'; Op == '/'; Op == '<-';
  Op == '++'; Op == '--'; Op == 'andalso'; Op == 'orelse';
  Op == 'not'; Op == 'and'; Op == 'or'; Op == 'xor';
  Op == '<'; Op == '>'; Op == '<='; Op == '>=';
  Op == '=='; Op == '!='; Op == '==='; Op == '!==' ->
  translate_each({ '__OP__', Line, [Op|Exprs] }, S);

%% ::

translate_macro({'::', Line, [Left]}, S) ->
  translate_macro({'::', Line, [nil,Left]}, S);

translate_macro({'::', Line, [Left|Right]}, S) ->
  { TLeft, LS } = translate_each(Left, S),
  { TRight, RS } = translate_args(Right, (umergec(S, LS))#elixir_scope{noref=true}),
  TArgs = [TLeft|TRight],
  Atoms = [Atom || { atom, _, Atom } <- TArgs],
  Final = case length(Atoms) == length(TArgs) of
    true  -> { atom, Line, elixir_ref:concat(Atoms) };
    false ->
      FArgs = [elixir_tree_helpers:build_simple_list(Line, TArgs)],
      ?ELIXIR_WRAP_CALL(Line, elixir_ref, concat, FArgs)
  end,
  { Final, (umergev(LS, RS))#elixir_scope{noref=S#elixir_scope.noref} };

%% @

translate_macro({'@', Line, [{ Name, _, Args }]}, S) ->
  assert_module_scope(Line, '@', S),
  assert_no_function_scope(Line, '@', S),
  case Args of
    [Arg] ->
      translate_each({
        { '.', Line, ['::Module', merge_data] },
          Line,
          [ { '__MODULE__', Line, false }, [{ Name, Arg }] ]
      }, S);
    _ when is_atom(Args) or (Args == []) ->
        translate_each({
          { '.', Line, ['::Module', read_data] },
          Line,
          [ { '__MODULE__', Line, false }, Name ]
        }, S);
    _ ->
      syntax_error(Line, S#elixir_scope.filename, "expected 0 or 1 argument for @~s, got: ~p", [Name, length(Args)])
  end;


%% Case

translate_macro({'case', Line, [Expr, RawClauses]}, S) ->
  Clauses = orddict:erase(do, RawClauses),
  { TExpr, NS } = translate_each(Expr, S),
  { TClauses, TS } = elixir_clauses:match(Line, Clauses, NS),
  { { 'case', Line, TExpr, TClauses }, TS };

%% Try

translate_macro({'try', Line, [Clauses]}, RawS) ->
  Do    = proplists:get_value('do',    Clauses, []),
  After = proplists:get_value('after', Clauses, []),
  Catch = orddict:erase('after', orddict:erase('do', Clauses)),

  S = RawS#elixir_scope{noname=true},

  { TDo, SB }    = translate([Do], S),
  { TCatch, SC } = elixir_try:clauses(Line, Catch, umergec(S, SB)),
  { TAfter, SA } = translate([After], umergec(S, SC)),
  { { 'try', Line, unpack_try(do, TDo), [], TCatch, unpack_try('after', TAfter) }, umergec(RawS, SA) };

%% Receive

translate_macro({'receive', Line, [RawClauses] }, S) ->
  Clauses = orddict:erase(do, RawClauses),
  case orddict:find('after', Clauses) of
    { ok, After } ->
      AClauses = orddict:erase('after', Clauses),
      { TClauses, SC } = elixir_clauses:match(Line, AClauses ++ [{'after',After}], S),
      { FClauses, [TAfter] } = lists:split(length(TClauses) - 1, TClauses),
      { _, _, [FExpr], _, FAfter } = TAfter,
      { { 'receive', Line, FClauses, FExpr, FAfter }, SC };
    error ->
      { TClauses, SC } = elixir_clauses:match(Line, Clauses, S),
      { { 'receive', Line, TClauses }, SC }
  end;

%% Definitions

translate_macro({defmodule, Line, [Ref, [{do,Block}]]}, S) ->
  { TRef, _ } = translate_each(Ref, S),

  NS = case TRef of
    { atom, _, Module } ->
      S#elixir_scope{scheduled=[Module|S#elixir_scope.scheduled]};
    _ -> S
  end,

  { elixir_module:translate(Line, TRef, Block, S), NS };

translate_macro({Kind, Line, [_Call]}, S) when Kind == def; Kind == defmacro; Kind == defp ->
  assert_module_scope(Line, Kind, S),
  { { nil, Line }, S };

translate_macro({Kind, Line, [Call, KV]}, S) when Kind == def; Kind == defp; Kind == defmacro ->
  { TCall, Guards } = elixir_clauses:extract_guards(Call),
  { Name, Args } = elixir_clauses:extract_args(TCall),
  translate_macro({ Kind, Line, [ Name, Args, Guards, KV ] }, S);

translate_macro({Kind, Line, [Name, Args, KV]}, S) when Kind == def; Kind == defp; Kind == defmacro ->
  translate_macro({ Kind, Line, [ Name, Args, true, KV ] }, S);

translate_macro({Kind, Line, [Name, Args, Guards, KV]}, S) when Kind == def; Kind == defp; Kind == defmacro ->
  assert_module_scope(Line, Kind, S),
  case KV of
    [{do, Expr}] -> [];
    _ -> Expr = { 'try', Line, [KV]}
  end,
  { TName, TS } = translate_each(Name, S),
  { elixir_def:wrap_definition(Kind, Line, TName, Args, elixir_clauses:extract_guard_clauses(Guards), Expr, TS), TS };

%% Modules directives

translate_macro({use, Line, [Ref|Args]}, S) when length(Args) =< 1 ->
  assert_module_scope(Line, use, S),
  Module = S#elixir_scope.module,
  Call = { '__BLOCK__', Line, [
    { require, Line, [Ref,[{as,false}]] },
    { { '.', Line, [Ref, '__using__'] }, Line, [Module|Args] }
  ] },
  translate_each(Call, S);

%% Apply - Optimize apply by checking what doesn't need to be dispatched dynamically

translate_macro({apply, Line, [Left, Right, Args]}, S) when is_list(Args) ->
  { TLeft,  SL } = translate_each(Left, S),
  { TRight, SR } = translate_each(Right, umergec(S, SL)),
  translate_apply(Line, TLeft, TRight, Args, S, SL, SR);

%% Handle forced variables

translate_macro({ 'var!', _, [{Name, Line, Atom}] }, S) when is_atom(Name), is_atom(Atom) ->
  elixir_variables:translate_each(Line, Name, S);

translate_macro({ 'var!', Line, [_] }, S) ->
  syntax_error(Line, S#elixir_scope.filename, "invalid args for var!");

translate_macro({ Atom, Line, Args }, S) ->
  { TArgs, NS } = translate_args(Args, S),
  { { call, Line, { atom, Line, Atom }, TArgs }, NS }.

%% HELPERS

% Unpack a list of expressions from a block.
% Return an empty list in case it is an empty expression on after.
unpack_try(_, [{ '__BLOCK__', _, Exprs }]) -> Exprs;
unpack_try('after', [{ nil, _ }])          -> [];
unpack_try(_, Exprs)                       -> Exprs.

%% Assertions

assert_no_function_scope(_Line, _Kind, #elixir_scope{function=[]}) -> [];
assert_no_function_scope(Line, Kind, S) ->
  syntax_error(Line, S#elixir_scope.filename, "cannot invoke ~s inside a function", [Kind]).

assert_module_scope(Line, Kind, #elixir_scope{module=[],filename=Filename}) ->
  syntax_error(Line, Filename, "cannot invoke ~s outside module", [Kind]);
assert_module_scope(_Line, _Kind, _S) -> [].