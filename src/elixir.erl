-module(elixir).
-export([to_form/1, eval/1]).

eval(String) ->
  {value, Value, _} = erl_eval:expr(to_form(String), []),
  Value.

to_form(String) ->
  transform(parse(String)).

parse(String) ->
	{ok, Tokens, _} = elixir_lexer:string(String),
	{ok, ParseTree} = elixir_parser:parse(Tokens),
	ParseTree.

transform({ binary_op, Line, Op, Left, Right }) ->
  {op, Line, Op, transform(Left), transform(Right)};

transform({ integer, _, _ } = Expr) -> Expr.