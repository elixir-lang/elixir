-module(elixir).
-export([eval/1, eval/2, throw_elixir/1, throw_erlang/1]).

% Evaluates a string
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

% Parse string and transform tree to Erlang Abstract Form format
parse(String) ->
	{ok, Tokens, _} = elixir_lexer:string(String),
	{ok, Forms} = elixir_parser:parse(Tokens),
  Transform = fun(X, Acc) -> [transform(X)|Acc] end,
  lists:foldr(Transform, [], Forms).

% TODO transformations should contain the filename

transform({match, Line, Left, Right}) ->
  {match, Line, transform(Left), transform(Right)};

transform({binary_op, Line, Op, Left, Right}) ->
  {op, Line, Op, transform(Left), transform(Right)};

transform({unary_op, Line, Op, Right}) ->
  {op, Line, Op, transform(Right)};

transform({'fun', Line, Clauses}) ->
  {'fun', Line, transform(Clauses)};

transform({clauses, Clauses}) ->
  {clauses, [transform(Clause) || Clause <- Clauses]};

transform({clause, Line, Arg1, Arg2, Exprs}) ->
  {clause, Line, Arg1, Arg2, [transform(Expr) || Expr <- Exprs]};

transform({call, Line, Vars, Args }) ->
  {call, Line, Vars, [transform(Arg) || Arg <- Args]};

transform({module, Line, Name, Exprs}) ->
  Body = [transform(Expr) || Expr <- Exprs],
  {value, Value, _} = erl_eval:exprs(Body, []),
  load_module(build_module(Line, Name)),
  {nil, Line};

% Match all other expressions
transform(Expr) -> Expr.

build_module(Line, Name) ->
  [{attribute, Line, module, Name}, {attribute, Line, export, []}].

load_module(Forms) ->
  case compile:forms(Forms) of
     {ok,ModuleName,Binary}           -> code:load_binary(ModuleName, "nofile", Binary);
     {ok,ModuleName,Binary,_Warnings} -> code:load_binary(ModuleName, "nofile", Binary)
  end.
