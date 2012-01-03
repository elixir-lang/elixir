%% Those macros are local and can be partially applied.
%% In the future, they may even be overriden by imports.
-module(elixir_macros).
-export([translate_macro/2]).
-import(elixir_translator, [translate_each/2, translate/2, translate_args/2, translate_apply/7]).
-import(elixir_tree_helpers, [umergev/2, umergec/2]).
-import(elixir_errors, [syntax_error/4]).
-include("elixir.hrl").

%% Operators

translate_macro({ '+', _Line, [Expr] }, S) when is_number(Expr) ->
  record('+', S),
  translate_each(Expr, S);

translate_macro({ '-', _Line, [Expr] }, S) when is_number(Expr) ->
  record('-', S),
  translate_each(-1 * Expr, S);

translate_macro({ Op, Line, Exprs }, S) when is_list(Exprs),
  Op == '+'; Op == '-'; Op == '*'; Op == '/'; Op == '<-';
  Op == '++'; Op == '--'; Op == 'andalso'; Op == 'orelse';
  Op == 'not'; Op == 'and'; Op == 'or'; Op == 'xor';
  Op == '<'; Op == '>'; Op == '<='; Op == '>=';
  Op == '=='; Op == '!='; Op == '==='; Op == '!==' ->
  record(Op, S),
  translate_macro({ erlang_op, Line, [Op|Exprs] }, S);

%% Erlang Operators

translate_macro({ erlang_op, Line, [Op, Expr] }, S) when is_atom(Op) ->
  record(erlang_op, S),
  { TExpr, NS } = translate_each(Expr, S),
  { { op, Line, convert_op(Op), TExpr }, NS };

translate_macro({ erlang_op, Line, [Op|Args] }, S) when is_atom(Op) ->
  record(erlang_op, S),
  { [TLeft, TRight], NS }  = translate_args(Args, S),
  { { op, Line, convert_op(Op), TLeft, TRight }, NS };

%% Case

translate_macro({'case', Line, [Expr, RawClauses]}, S) ->
  record('case', S),
  Clauses = orddict:erase(do, RawClauses),
  { TExpr, NS } = translate_each(Expr, S),
  { TClauses, TS } = elixir_clauses:match(Line, Clauses, NS),
  { { 'case', Line, TExpr, TClauses }, TS };

%% Try

translate_macro({'try', Line, [Clauses]}, RawS) ->
  record('try', RawS),
  Do    = proplists:get_value('do',    Clauses, []),
  Catch = proplists:get_value('catch', Clauses, []),
  After = proplists:get_value('after', Clauses, []),

  S = RawS#elixir_scope{noname=true},

  { TDo, SB }    = translate([Do], S),
  { TCatch, SC } = elixir_clauses:try_catch(Line, [{'catch',Catch}], umergec(S, SB)),
  { TAfter, SA } = translate([After], umergec(S, SC)),
  { { 'try', Line, unpack_try(do, TDo), [], TCatch, unpack_try('after', TAfter) }, umergec(RawS, SA) };

%% Receive

translate_macro({'receive', Line, [RawClauses] }, S) ->
  record('receive', S),
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

%% Definitions

translate_macro({defmodule, Line, [Ref, [{do,Block}]]}, S) ->
  record(defmodule, S),
  { TRef, _ } = translate_each(Ref, S#elixir_scope{noref=true}),

  NS = case TRef of
    { atom, _, Module } ->
      S#elixir_scope{scheduled=[Module|S#elixir_scope.scheduled]};
    _ -> S
  end,

  { elixir_module:transform(Line, S#elixir_scope.filename, TRef, Block), NS };

translate_macro({Kind, Line, [Call,[{do, Expr}]]}, S) when Kind == def; Kind == defp; Kind == defmacro ->
  record(Kind, S),
  case S#elixir_scope.function /= [] of
    true ->
      syntax_error(Line, S#elixir_scope.filename, "invalid function scope for: ", atom_to_list(Kind));
    _ ->
      { elixir_def:wrap_definition(Kind, Line, Call, Expr, S), S }
  end;

translate_macro({Kind, Line, [Call]}, S) when Kind == def; Kind == defmacro; Kind == defp ->
  record(Kind, S),
  { Name, Args } = elixir_clauses:extract_args(Call),
  { { tuple, Line, [{ atom, Line, Name }, { integer, Line, length(Args) }] }, S };

translate_macro({Kind, Line, Args}, S) when is_list(Args), Kind == def; Kind == defmacro; Kind == defp ->
  syntax_error(Line, S#elixir_scope.filename, "invalid args for: ", atom_to_list(Kind));

%% Functions

translate_macro({fn, Line, RawArgs}, S) when is_list(RawArgs) ->
  record(fn, S),
  Clauses = case lists:split(length(RawArgs) - 1, RawArgs) of
    { Args, [[{do,Expr}]] } ->
      [{match,Args,Expr}];
    { [], [KV] } when is_list(KV) ->
      elixir_kv_block:decouple(orddict:erase(do, KV));
    _ ->
      syntax_error(Line, S#elixir_scope.filename, "no block given for: ", "fn")
  end,

  Transformer = fun({ match, ArgsWithGuards, Expr }, Acc) ->
    { FinalArgs, Guards } = elixir_clauses:extract_last_guards(ArgsWithGuards),
    elixir_clauses:assigns_block(Line, fun elixir_translator:translate/2, FinalArgs, [Expr], Guards, umergec(S, Acc))
  end,

  { TClauses, NS } = lists:mapfoldl(Transformer, S, Clauses),
  { { 'fun', Line, {clauses, TClauses} }, umergec(S, NS) };

%% Loop and recur

translate_macro({loop, Line, RawArgs}, S) when is_list(RawArgs) ->
  record(loop, S),
  case lists:split(length(RawArgs) - 1, RawArgs) of
    { Args, [KV] } when is_list(KV) ->
      %% Generate a variable that will store the function
      { FunVar, VS }  = elixir_tree_helpers:build_ex_var(Line, S),

      %% Add this new variable to all match clauses
      [{match, KVBlock}] = elixir_kv_block:normalize(orddict:erase(do, KV)),
      Values = [{ [FunVar|Conds], Expr } || { Conds, Expr } <- element(3, KVBlock)],
      NewKVBlock = setelement(3, KVBlock, Values),

      %% Generate a function with the match blocks
      Function = { fn, Line, [[{match,NewKVBlock}]] },

      %% Finally, assign the function to a variable and
      %% invoke it passing the function itself as first arg
      Block = { block, Line, [
        { '=', Line, [FunVar, Function] },
        { { '.', Line, [FunVar] }, Line, [FunVar|Args] }
      ] },

      { TBlock, TS } = translate_each(Block, VS#elixir_scope{recur=element(1,FunVar)}),
      { TBlock, TS#elixir_scope{recur=[]} };
    _ ->
      syntax_error(Line, S#elixir_scope.filename, "invalid args for: ", "loop")
  end;

translate_macro({recur, Line, Args}, S) when is_list(Args) ->
  record(recur, S),
  case S#elixir_scope.recur of
    [] ->
      syntax_error(Line, S#elixir_scope.filename, "cannot invoke recur outside of a loop. invalid scope for: ", "recur");
    Recur ->
      ExVar = { Recur, Line, false },
      Call = { { '.', Line, [ExVar] }, Line, [ExVar|Args] },
      translate_each(Call, S)
  end;

%% Apply - Optimize apply by checking what doesn't need to be dispatched dynamically

translate_macro({apply, Line, [Left, Right, Args]}, S) when is_list(Args) ->
  record(apply, S),
  { TLeft,  SL } = translate_each(Left, S),
  { TRight, SR } = translate_each(Right, umergec(S, SL)),
  translate_apply(Line, TLeft, TRight, Args, S, SL, SR);

%% Else

translate_macro({ Atom, Line, Args }, S) ->
  Callback = fun() ->
    { TArgs, NS } = translate_args(Args, S),
    { { call, Line, { atom, Line, Atom }, TArgs }, NS }
  end,
  elixir_dispatch:dispatch_imports(Line, Atom, Args, S, Callback).

%% Helpers

% Unpack a list of expressions from a block.
% Return an empty list in case it is an empty expression on after.
unpack_try(_, [{ block, _, Exprs }]) -> Exprs;
unpack_try('after', [{ nil, _ }])    -> [];
unpack_try(_, Exprs)                 -> Exprs.

% We need to record macros invoked so we raise users
% a nice error in case they define a local that overrides
% an invoked macro instead of silently failing.
%
% Some macros are not recorded because they will always
% raise an error to users if they define something similar
% regardless if they invoked it or not.
record(Atom, S) ->
  elixir_import:record(internal, { Atom, nil }, in_erlang_macros, S).

convert_op('!==') -> '=/=';
convert_op('===') -> '=:=';
convert_op('!=')  ->  '/=';
convert_op('<=')  ->  '=<';
convert_op('<-')  ->  '!';
convert_op(Else)  ->  Else.