%% Those macros are local and can be partially applied.
%% In the future, they may even be overriden by imports.
-module(elixir_macros).
-export([translate_macro/2]).
-import(elixir_translator, [translate_each/2, translate/2, translate_args/2, translate_apply/7]).
-import(elixir_variables, [umergec/2, umergev/2]).
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
  record('@', S),
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
      syntax_error(Line, S#elixir_scope.filename, "expected 0 or 1 argument for: ", [$@|atom_to_list(Name)])
  end;

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
  After = proplists:get_value('after', Clauses, []),
  Catch = orddict:erase('after', orddict:erase('do', Clauses)),

  S = RawS#elixir_scope{noname=true},

  { TDo, SB }    = translate([Do], S),
  { TCatch, SC } = elixir_clauses:try_catch(Line, Catch, umergec(S, SB)),
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

%% Definitions

translate_macro({defmodule, Line, [Ref, [{do,Block}]]}, S) ->
  record(defmodule, S),
  { TRef, _ } = translate_each(Ref, S),

  NS = case TRef of
    { atom, _, Module } ->
      S#elixir_scope{scheduled=[Module|S#elixir_scope.scheduled]};
    _ -> S
  end,

  { elixir_module:translate(Line, TRef, Block, S), NS };

translate_macro({Kind, Line, [_Call]}, S) when Kind == def; Kind == defmacro; Kind == defp ->
  record(Kind, S),
  assert_module_scope(Line, Kind, S),
  { { nil, Line }, S };

translate_macro({Kind, Line, [Call, KV]}, S) when Kind == def; Kind == defp; Kind == defmacro ->
  { TCall, Guards } = elixir_clauses:extract_guards(Call),
  { Name, Args } = elixir_clauses:extract_args(TCall),
  translate_macro({ Kind, Line, [ Name, Args, Guards, KV ] }, S);

translate_macro({Kind, Line, [Name, Args, Guards, KV]}, S) when Kind == def; Kind == defp; Kind == defmacro ->
  record(Kind, S),
  assert_module_scope(Line, Kind, S),
  case KV of
    [{do, Expr}] -> [];
    _ -> Expr = { 'try', Line, [KV]}
  end,
  { TName, TS } = translate_each(Name, S),
  { elixir_def:wrap_definition(Kind, Line, TName, Args, Guards, Expr, TS), TS };

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

%% Modules directives

translate_macro({use, Line, [Ref|Args]}, S) ->
  record(use, S),
  assert_module_scope(Line, use, S),
  Module = S#elixir_scope.module,
  Call = { block, Line, [
    { require, Line, [Ref] },
    { { '.', Line, [Ref, '__using__'] }, Line, [Module|Args] }
  ] },
  translate_each(Call, S);

translate_macro({import, Line, [Arg]}, S) ->
  translate_macro({import, Line, [Arg, []]}, S);

translate_macro({import, Line, [_,_] = Args}, S) ->
  record(import, S),
  assert_module_scope(Line, import, S),
  assert_no_function_scope(Line, import, S),
  Module = S#elixir_scope.module,
  NewArgs = [Line, S#elixir_scope.filename, Module|Args],
  translate_each({{'.', Line, [elixir_import, handle_import]}, Line, NewArgs}, S);

%% Loop and recur

translate_macro({loop, Line, RawArgs}, S) when is_list(RawArgs) ->
  record(loop, S),
  case lists:split(length(RawArgs) - 1, RawArgs) of
    { Args, [KV] } when is_list(KV) ->
      %% Generate a variable that will store the function
      { FunVar, VS }  = elixir_variables:build_ex(Line, S),

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
      ExVar = { Recur, Line, nil },
      Call = { { '.', Line, [ExVar] }, Line, [ExVar|Args] },
      translate_each(Call, S)
  end;

%% Comprehensions

translate_macro({ Kind, Line, Args }, S) when is_list(Args), (Kind == lc) orelse (Kind == bc) ->
  translate_comprehension(Line, Kind, Args, S);

translate_macro({ for, Line, RawArgs }, S) when is_list(RawArgs) ->
  case lists:split(length(RawArgs) - 1, RawArgs) of
    { Cases, [[{do,Expr}]] } ->
      { Generators, Filters } = lists:splitwith(fun
        ({ 'in', _, _ }) -> true;
        (_) -> false
      end, Cases),

      case Generators of
        [] -> syntax_error(Line, S#elixir_scope.filename, "expected at least one generator for: ", "for");
        _  -> []
      end,

      Args  = [X || { _, _, [X, _] } <- Generators],
      Enums = [Y || { _, _, [_, Y] } <- Generators],

      { AccVar, VS } = elixir_variables:build_ex(Line, S),
      Tail = [{ '|', Line, [Expr, AccVar] }],

      Fun = case lists:all(fun is_var/1, Args) andalso Filters == [] of
        true  ->
          { fn, Line, [AccVar|lists:reverse(Args)] ++ [[{do,Tail}]] };
        false ->
          Condition  = lists:reverse(Args),
          Underscore = [{ '_', Line, nil } || _ <- Args],

          Body = case Filters of
            [] -> Tail;
            _  ->
              Guard = lists:foldl(fun(X, Acc) ->
                { '&&', Line, [X, Acc] }
              end, hd(Filters), tl(Filters)),

              { 'if', Line, [Guard, [{do,Tail},{else,AccVar}]] }
          end,

          { fn, Line, [[
            { match, { kv_block, Line, [
              { [AccVar|Condition], Body },
              { [AccVar|Underscore], AccVar }
            ] } }
          ]] }
      end,

      translate_each({ { '.', Line, ['::Enum', for] }, Line, [Enums, Fun] }, VS);
    _ ->
      syntax_error(Line, S#elixir_scope.filename, "no block given for: ", "for")
  end;

%% Apply - Optimize apply by checking what doesn't need to be dispatched dynamically

translate_macro({apply, Line, [Left, Right, Args]}, S) when is_list(Args) ->
  record(apply, S),
  { TLeft,  SL } = translate_each(Left, S),
  { TRight, SR } = translate_each(Right, umergec(S, SL)),
  translate_apply(Line, TLeft, TRight, Args, S, SL, SR);

%% Handle forced variables

translate_macro({ 'var!', _, [{Name, Line, Atom}] }, S) when is_atom(Name), is_atom(Atom) ->
  elixir_variables:translate_each(Line, Name, S);

translate_macro({ 'var!', Line, [_] }, S) ->
  syntax_error(Line, S#elixir_scope.filename, "invalid args for: ", "var!");

%% Else

translate_macro({ Atom, Line, Args }, S) ->
  Callback = fun() ->
    { TArgs, NS } = translate_args(Args, S),
    { { call, Line, { atom, Line, Atom }, TArgs }, NS }
  end,
  elixir_dispatch:dispatch_imports(Line, Atom, Args, S, Callback).

%% Helpers

translate_comprehension(Line, Kind, Args, S) ->
  case lists:split(length(Args) - 1, Args) of
    { Cases, [[{do,Expr}]] } ->
      { TCases, SC } = lists:mapfoldl(fun translate_each_comprehension/2, S, Cases),
      { TExpr, SE } = translate_each(Expr, SC),
      { { Kind, Line, TExpr, TCases }, umergec(S, SE) };
    _ ->
      syntax_error(Line, S#elixir_scope.filename, "no block given for comprehension: ", atom_to_list(Kind))
  end.

translate_each_comprehension({ in, Line, [{'<<>>', _, _} = Left, Right] }, S) ->
  translate_each_comprehension({ inbin, Line, [Left, Right]}, S);

translate_each_comprehension({inbin, Line, [Left, Right]}, S) ->
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL  } = elixir_clauses:assigns(fun elixir_translator:translate_each/2, Left, SR),
  { { b_generate, Line, TLeft, TRight }, SL };

translate_each_comprehension({Kind, Line, [Left, Right]}, S) when Kind == in; Kind == inlist ->
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL  } = elixir_clauses:assigns(fun elixir_translator:translate_each/2, Left, SR),
  { { generate, Line, TLeft, TRight }, SL };

translate_each_comprehension(X, S) ->
  { TX, TS } = translate_each(X, S),
  Line = case X of
    { _, L, _ } -> L;
    _ -> 0
  end,
  { elixir_tree_helpers:convert_to_boolean(Line, TX, true), TS }.

% Unpack a list of expressions from a block.
% Return an empty list in case it is an empty expression on after.
unpack_try(_, [{ block, _, Exprs }]) -> Exprs;
unpack_try('after', [{ nil, _ }])    -> [];
unpack_try(_, Exprs)                 -> Exprs.

is_var({ Name, _Line, Atom }) when is_atom(Name), is_atom(Atom) -> true;
is_var(_) -> false.

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

%% Assertions

assert_no_function_scope(_Line, _Kind, #elixir_scope{function=[]}) -> [];
assert_no_function_scope(Line, Kind, S) ->
  syntax_error(Line, S#elixir_scope.filename, "cannot invoke inside function: ", atom_to_list(Kind)).

assert_module_scope(Line, Kind, #elixir_scope{module=[],filename=Filename}) ->
  syntax_error(Line, Filename, "cannot invoke outside module: ", atom_to_list(Kind));
assert_module_scope(_Line, _Kind, _S) -> [].