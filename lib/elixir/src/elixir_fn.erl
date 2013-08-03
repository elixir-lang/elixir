-module(elixir_fn).
-export([fn/3, capture/3]).
-import(elixir_scope, [umergec/2]).
-import(elixir_errors, [syntax_error/3, syntax_error/4, compile_error/4]).
-include("elixir.hrl").

fn(Meta, Clauses, S) ->
  Transformer = fun({ ArgsWithGuards, CMeta, Expr }, Acc) ->
    { Args, Guards } = elixir_clauses:extract_splat_guards(ArgsWithGuards),
    elixir_clauses:assigns_block(?line(CMeta), fun translate_fn_match/2, Args, [Expr], Guards, umergec(S, Acc))
  end,

  { TClauses, NS } = lists:mapfoldl(Transformer, S, Clauses),
  Arities = [length(Args) || { clause, _Line, Args, _Guards, _Exprs } <- TClauses],

  case length(lists:usort(Arities)) of
    1 ->
      { { 'fun', ?line(Meta), { clauses, TClauses } }, umergec(S, NS) };
    _ ->
      syntax_error(Meta, S#elixir_scope.file,
                   "cannot mix clauses with different arities in function definition")
  end.

translate_fn_match(Arg, S) ->
  { TArg, TS } = elixir_translator:translate(Arg, S#elixir_scope{extra=fn_match}),
  { TArg, TS#elixir_scope{extra=S#elixir_scope.extra} }.

capture(Meta, { '/', _, [{ { '.', _, [_, F] } = Dot, RequireMeta , [] }, A] }, S) when is_atom(F), is_integer(A) ->
  Args = [{ '&', [], [X] } || X <- lists:seq(1, A)],
  capture_require(Meta, { Dot, RequireMeta, Args }, S, true);

capture(Meta, { '/', _, [{ F, _, C }, A] }, S) when is_atom(F), is_integer(A), is_atom(C) ->
  ImportMeta =
    case lists:keyfind(import_fa, 1, Meta) of
      { import_fa, { Receiver, Context } } ->
        lists:keystore(context, 1,
          lists:keystore(import, 1, Meta, { import, Receiver }),
          { context, Context }
        );
      false -> Meta
    end,
  Args = [{ '&', [], [X] } || X <- lists:seq(1, A)],
  capture_import(Meta, { F, ImportMeta, Args }, S, true);

capture(Meta, { { '.', _, [_, Fun] }, _, Args } = Expr, S) when is_atom(Fun), is_list(Args) ->
  capture_require(Meta, Expr, S, is_sequential_and_not_empty(Args));

capture(Meta, { '__block__', _, _ } = Expr, S) ->
  Message = "invalid args for &, block expressions are not allowed, got: ~ts",
  syntax_error(Meta, S#elixir_scope.file, Message, ['Elixir.Macro':to_string(Expr)]);

capture(Meta, { Atom, _, Args } = Expr, S) when is_atom(Atom), is_list(Args) ->
  capture_import(Meta, Expr, S, is_sequential_and_not_empty(Args));

capture(Meta, { Left, Right }, S) ->
  capture(Meta, { '{}', Meta, [Left, Right] }, S);

capture(Meta, List, S) when is_list(List) ->
  capture(Meta, { '[]', Meta, List }, S);

capture(Meta, Arg, S) when is_integer(Arg) ->
  compile_error(Meta, S#elixir_scope.file, "unhandled &~B outside of a capture", [Arg]);

capture(Meta, Arg, S) ->
  invalid_capture(Meta, Arg, S).

%% Helpers

capture_import(Meta, { Atom, ImportMeta, Args } = Expr, S, Sequential) ->
  case Sequential andalso
       elixir_dispatch:import_function(ImportMeta, Atom, length(Args), S) of
    false -> do_capture(Meta, Expr, S, Sequential);
    Else  -> Else
  end.

capture_require(Meta, { { '.', _, [Left, Right] }, RequireMeta, Args } = Expr, S, Sequential) ->
  { Mod, SE } = 'Elixir.Macro':expand_all(Left, elixir_scope:to_ex_env({ ?line(Meta), S }), S),

  case Sequential andalso is_atom(Mod) andalso
       elixir_dispatch:require_function(RequireMeta, Mod, Right, length(Args), SE) of
    false -> do_capture(Meta, Expr, S, Sequential);
    Else  -> Else
  end.

do_capture(Meta, Expr, S, Sequential) ->
  case do_escape(Expr, S, []) of
    { _, _, [] } when not Sequential ->
      invalid_capture(Meta, Expr, S);
    { TExpr, TS, TDict } ->
      TVars = validate(Meta, TDict, 1, S),
      fn(Meta, [{ TVars, Meta, TExpr }], TS)
  end.

invalid_capture(Meta, Arg, S) ->
  Message = "invalid args for &, expected an expression in the format of &Mod.fun/arity, "
            "&local/arity or a capture containing at least one argument as &1, got: ~ts",
  syntax_error(Meta, S#elixir_scope.file, Message, ['Elixir.Macro':to_string(Arg)]).

validate(Meta, [{ Pos, Var }|T], Pos, S) ->
  [Var|validate(Meta, T, Pos + 1, S)];

validate(Meta, [{ Pos, _ }|_], Expected, S) ->
  compile_error(Meta, S#elixir_scope.file, "capture &~B cannot be defined without &~B", [Pos, Expected]);

validate(_Meta, [], _Pos, _S) ->
  [].

do_escape({ '&', Meta, [Pos] }, S, Dict) when is_integer(Pos) ->
  case orddict:find(Pos, Dict) of
    { ok, Var } ->
      { Var, S, Dict };
    error ->
      { Var, SC } = elixir_scope:build_ex_var(?line(Meta), S),
      { Var, SC, orddict:store(Pos, Var, Dict) }
  end;

do_escape({ '&', Meta, _ } = Arg, S, _Dict) ->
  Message = "nested captures via & are not allowed: ~ts",
  compile_error(Meta, S#elixir_scope.file, Message, ['Elixir.Macro':to_string(Arg)]);

do_escape({ Left, Meta, Right }, S0, Dict0) ->
  { TLeft, S1, Dict1 }  = do_escape(Left, S0, Dict0),
  { TRight, S2, Dict2 } = do_escape(Right, S1, Dict1),
  { { TLeft, Meta, TRight }, S2, Dict2 };

do_escape({ Left, Right }, S0, Dict0) ->
  { TLeft, S1, Dict1 }  = do_escape(Left, S0, Dict0),
  { TRight, S2, Dict2 } = do_escape(Right, S1, Dict1),
  { { TLeft, TRight }, S2, Dict2 };

do_escape(List, S, Dict) when is_list(List) ->
  do_escape_list(List, S, Dict, []);

do_escape(Other, S, Dict) ->
  { Other, S, Dict }.

do_escape_list([H|T], S, Dict, Acc) ->
  { TH, TS, TDict } = do_escape(H, S, Dict),
  do_escape_list(T, TS, TDict, [TH|Acc]);

do_escape_list([], S, Dict, Acc) ->
  { lists:reverse(Acc), S, Dict }.

is_sequential_and_not_empty([])   -> false;
is_sequential_and_not_empty(List) -> is_sequential(List, 1).

is_sequential([{ '&', _, [Int] }|T], Int) ->
  is_sequential(T, Int + 1);
is_sequential([], _Int) -> true;
is_sequential(_, _Int) -> false.
