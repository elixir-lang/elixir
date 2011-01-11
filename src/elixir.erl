-module(elixir).
-export([parse/1, eval/1, eval/2, throw_elixir/1, throw_erlang/1]).

eval(String) -> eval(String, []).

eval(String, Binding) ->
  {value, Value, NewBinding} = erl_eval:exprs(parse(String), Binding),
  {Value, NewBinding}.

% Temporary to aid debugging
throw_elixir(String) ->
  erlang:error(io:format("~p~n", [parse(String)])).

% Temporary to aid debugging
throw_erlang(String) ->
  {ok, Tokens, _} = erl_scan:string(String),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  erlang:error(io:format("~p~n", [Form])).

% Parse file and transform tree to erlang bytecode
parse(String) ->
	{ok, Tokens, _} = elixir_lexer:string(String),
	{ok, ParseTree} = elixir_parser:parse(Tokens),
  Transform = fun(X, Acc) -> [transform(X)|Acc] end,
  lists:foldr(Transform, [], ParseTree).

transform({ binary_op, Line, Op, Left, Right }) ->
  {op, Line, Op, transform(Left), transform(Right)};

transform({ unary_op, Line, Op, Right }) ->
  {op, Line, Op, transform(Right)};

transform({ match, Line, Left, Right }) ->
  {match, Line, transform(Left), transform(Right)};

transform({'fun', Line, Clauses}) ->
  {'fun', Line, transform(Clauses)};

transform({clauses, Clauses}) ->
  {clauses, lists:map(fun transform/1, Clauses)};

transform({clause, Line, Arg1, Arg2, Expr}) ->
  {clause, Line, Arg1, Arg2, lists:map(fun transform/1, Expr) };

transform({call, Line, Vars, Args }) ->
  {call, Line, Vars, lists:map(fun transform/1, Args) };

% Match all other expressions
transform(Expr) -> Expr.