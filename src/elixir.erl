-module(elixir).
-export([eval/1, from_elixir/1, from_erlang/1]).

eval(String) ->
  {value, Value, _} = erl_eval:expr(from_elixir(String), []),
  Value.

% Temporary to aid debugging
from_elixir(String) ->
  transform(parse(String)).

% Temporary to aid debugging
from_erlang(String) ->
  {ok, Tokens, _} = erl_scan:string(String),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  Form.

parse(String) ->
	{ok, Tokens, _} = elixir_lexer:string(String),
	{ok, ParseTree} = elixir_parser:parse(Tokens),
	ParseTree.

transform({ binary_op, Line, Op, Left, Right }) ->
  {op, Line, Op, transform(Left), transform(Right)};

transform({ unary_op, Line, Op, Right }) ->
  {op, Line, Op, transform(Right)};


transform({ integer, _, _ } = Expr) -> Expr.