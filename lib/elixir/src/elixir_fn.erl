-module(elixir_fn).
-export([capture/4, expand/4, format_error/1]).
-import(elixir_errors, [form_error/4]).
-include("elixir.hrl").

%% Anonymous functions

expand(Meta, Clauses, S, E) when is_list(Clauses) ->
  Transformer = fun({_, _, [Left, _Right]} = Clause, SA) ->
    case lists:any(fun is_invalid_arg/1, Left) of
      true ->
        form_error(Meta, E, ?MODULE, defaults_in_args);
      false ->
        SReset = elixir_env:reset_unused_vars(SA),

        {EClause, SAcc, EAcc} =
          elixir_clauses:clause(Meta, fn, fun elixir_clauses:head/3, Clause, SReset, E),

        {EClause, elixir_env:merge_and_check_unused_vars(SAcc, SA, EAcc)}
    end
  end,

  {EClauses, SE} = lists:mapfoldl(Transformer, S, Clauses),
  EArities = [fn_arity(Args) || {'->', _, [Args, _]} <- EClauses],

  case lists:usort(EArities) of
    [_] ->
      {{fn, Meta, EClauses}, SE, E};
    _ ->
      form_error(Meta, E, ?MODULE, clauses_with_different_arities)
  end.

is_invalid_arg({'\\\\', _, _}) -> true;
is_invalid_arg(_) -> false.

fn_arity([{'when', _, Args}]) -> length(Args) - 1;
fn_arity(Args) -> length(Args).

%% Capture

capture(Meta, {'/', _, [{{'.', _, [M, F]} = Dot, DotMeta, []}, A]}, S, E) when is_atom(F), is_integer(A) ->
  Args = args_from_arity(Meta, A, E),
  handle_capture_possible_warning(Meta, DotMeta, M, F, A, E),
  capture_require(Meta, {Dot, Meta, Args}, S, E, true);

capture(Meta, {'/', _, [{F, _, C}, A]}, S, E) when is_atom(F), is_integer(A), is_atom(C) ->
  Args = args_from_arity(Meta, A, E),
  capture_import(Meta, {F, Meta, Args}, S, E, true);

capture(Meta, {{'.', _, [_, Fun]}, _, Args} = Expr, S, E) when is_atom(Fun), is_list(Args) ->
  capture_require(Meta, Expr, S, E, is_sequential_and_not_empty(Args));

capture(Meta, {{'.', _, [_]}, _, Args} = Expr, S, E) when is_list(Args) ->
  capture_expr(Meta, Expr, S, E, false);

capture(Meta, {'__block__', _, [Expr]}, S, E) ->
  capture(Meta, Expr, S, E);

capture(Meta, {'__block__', _, _} = Expr, _S, E) ->
  form_error(Meta, E, ?MODULE, {block_expr_in_capture, Expr});

capture(Meta, {Atom, _, Args} = Expr, S, E) when is_atom(Atom), is_list(Args) ->
  capture_import(Meta, Expr, S, E, is_sequential_and_not_empty(Args));

capture(Meta, {Left, Right}, S, E) ->
  capture(Meta, {'{}', Meta, [Left, Right]}, S, E);

capture(Meta, List, S, E) when is_list(List) ->
  capture_expr(Meta, List, S, E, is_sequential_and_not_empty(List));

capture(Meta, Integer, _S, E) when is_integer(Integer) ->
  form_error(Meta, E, ?MODULE, {capture_arg_outside_of_capture, Integer});

capture(Meta, Arg, _S, E) ->
  invalid_capture(Meta, Arg, E).

capture_import(Meta, {Atom, ImportMeta, Args} = Expr, S, E, Sequential) ->
  Res = Sequential andalso
        elixir_dispatch:import_function(ImportMeta, Atom, length(Args), E),
  handle_capture(Res, Meta, Expr, S, E, Sequential).

capture_require(Meta, {{'.', DotMeta, [Left, Right]}, RequireMeta, Args}, S, E, Sequential) ->
  case escape(Left, E, []) of
    {EscLeft, []} ->
      {ELeft, SE, EE} = elixir_expand:expand(EscLeft, S, E),

      Res = Sequential andalso case ELeft of
        {Name, _, Context} when is_atom(Name), is_atom(Context) ->
          {remote, ELeft, Right, length(Args)};
        _ when is_atom(ELeft) ->
          elixir_dispatch:require_function(RequireMeta, ELeft, Right, length(Args), EE);
        _ ->
          false
      end,

      Dot = {{'.', DotMeta, [ELeft, Right]}, RequireMeta, Args},
      handle_capture(Res, Meta, Dot, SE, EE, Sequential);

    {EscLeft, Escaped} ->
      Dot = {{'.', DotMeta, [EscLeft, Right]}, RequireMeta, Args},
      capture_expr(Meta, Dot, S, E, Escaped, Sequential)
  end.

handle_capture(false, Meta, Expr, S, E, Sequential) ->
  capture_expr(Meta, Expr, S, E, Sequential);
handle_capture(LocalOrRemote, _Meta, _Expr, S, E, _Sequential) ->
  {LocalOrRemote, S, E}.

capture_expr(Meta, Expr, S, E, Sequential) ->
  capture_expr(Meta, Expr, S, E, [], Sequential).
capture_expr(Meta, Expr, S, E, Escaped, Sequential) ->
  case escape(Expr, E, Escaped) of
    {_, []} when not Sequential ->
      invalid_capture(Meta, Expr, E);
    {EExpr, EDict} ->
      EVars = validate(Meta, EDict, 1, E),
      Fn = {fn, Meta, [{'->', Meta, [EVars, EExpr]}]},
      {expand, Fn, S, E}
  end.

invalid_capture(Meta, Arg, E) ->
  form_error(Meta, E, ?MODULE, {invalid_args_for_capture, Arg}).

validate(Meta, [{Pos, Var} | T], Pos, E) ->
  [Var | validate(Meta, T, Pos + 1, E)];
validate(Meta, [{Pos, _} | _], Expected, E) ->
  form_error(Meta, E, ?MODULE, {capture_arg_without_predecessor, Pos, Expected});
validate(_Meta, [], _Pos, _E) ->
  [].

escape({'&', _, [Pos]}, _E, Dict) when is_integer(Pos), Pos > 0 ->
  Var = {list_to_atom([$x | integer_to_list(Pos)]), [], ?var_context},
  {Var, orddict:store(Pos, Var, Dict)};
escape({'&', Meta, [Pos]}, E, _Dict) when is_integer(Pos) ->
  form_error(Meta, E, ?MODULE, {unallowed_capture_arg, Pos});
escape({'&', Meta, _} = Arg, E, _Dict) ->
  form_error(Meta, E, ?MODULE, {nested_capture, Arg});
escape({Left, Meta, Right}, E, Dict0) ->
  {TLeft, Dict1}  = escape(Left, E, Dict0),
  {TRight, Dict2} = escape(Right, E, Dict1),
  {{TLeft, Meta, TRight}, Dict2};
escape({Left, Right}, E, Dict0) ->
  {TLeft, Dict1}  = escape(Left, E, Dict0),
  {TRight, Dict2} = escape(Right, E, Dict1),
  {{TLeft, TRight}, Dict2};
escape(List, E, Dict) when is_list(List) ->
  lists:mapfoldl(fun(X, Acc) -> escape(X, E, Acc) end, Dict, List);
escape(Other, _E, Dict) ->
  {Other, Dict}.

args_from_arity(_Meta, A, _E) when is_integer(A), A >= 0, A =< 255 ->
  [{'&', [], [X]} || X <- lists:seq(1, A)];
args_from_arity(Meta, A, E) ->
  form_error(Meta, E, ?MODULE, {invalid_arity_for_capture, A}).

is_sequential_and_not_empty([])   -> false;
is_sequential_and_not_empty(List) -> is_sequential(List, 1).

is_sequential([{'&', _, [Int]} | T], Int) -> is_sequential(T, Int + 1);
is_sequential([], _Int) -> true;
is_sequential(_, _Int) -> false.

handle_capture_possible_warning(Meta, DotMeta, Mod, Fun, Arity, E) ->
  case (Arity =:= 0) andalso (lists:keyfind(no_parens, 1, DotMeta) /= {no_parens, true}) of
    true ->
      elixir_errors:form_warn(Meta, E, ?MODULE, {parens_remote_capture, Mod, Fun});

    false -> ok
  end.

%% TODO: Raise on Elixir v2.0
format_error({parens_remote_capture, Mod, Fun}) ->
  io_lib:format("extra parentheses on a remote function capture &~ts.~ts()/0 have been "
                 "deprecated. Please remove the parentheses: &~ts.~ts/0",
                 ['Elixir.Macro':to_string(Mod), Fun, 'Elixir.Macro':to_string(Mod), Fun]);
format_error(clauses_with_different_arities) ->
  "cannot mix clauses with different arities in anonymous functions";
format_error(defaults_in_args) ->
  "anonymous functions cannot have optional arguments";
format_error({block_expr_in_capture, Expr}) ->
  io_lib:format("invalid args for &, block expressions are not allowed, got: ~ts",
                ['Elixir.Macro':to_string(Expr)]);
format_error({nested_capture, Arg}) ->
  io_lib:format("nested captures via & are not allowed: ~ts", ['Elixir.Macro':to_string(Arg)]);
format_error({invalid_arity_for_capture, Arity}) ->
  io_lib:format("invalid arity for &, expected a number between 0 and 255, got: ~b", [Arity]);
format_error({capture_arg_outside_of_capture, Integer}) ->
  io_lib:format("unhandled &~B outside of a capture", [Integer]);
format_error({capture_arg_without_predecessor, Pos, Expected}) ->
  io_lib:format("capture &~B cannot be defined without &~B", [Pos, Expected]);
format_error({unallowed_capture_arg, Integer}) ->
  io_lib:format("capture &~B is not allowed", [Integer]);
format_error({invalid_args_for_capture, Arg}) ->
  Message =
    "invalid args for &, expected one of:\n\n"
    "  * &Mod.fun/arity to capture a remote function, such as &Enum.map/2\n"
    "  * &fun/arity to capture a local or imported function, such as &is_atom/1\n"
    "  * &some_code(&1, ...) containing at least one argument as &1, such as &List.flatten(&1)\n\n"
    "Got: ~ts",
  io_lib:format(Message, ['Elixir.Macro':to_string(Arg)]).
