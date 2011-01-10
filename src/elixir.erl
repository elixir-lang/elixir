-module(elixir).
-export([parse/1, eval/1, eval/2, from_elixir/1, from_erlang/1]).

eval(String) -> eval(String, []).

eval(String, Binding) ->
  {value, Value, NewBinding} = erl_eval:exprs(from_elixir(String), Binding),
  {Value, NewBinding}.

% Temporary to aid debugging
from_elixir(String) ->
  Transform = fun(X, Acc) -> [transform(X)|Acc] end,
  lists:foldr(Transform, [], parse(String)).

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

transform({ match, Line, Left, Right }) ->
  {match, Line, transform(Left), transform(Right)};

% Match all other expressions. Types:
%   integer
%   var
transform(Expr) -> Expr.