-module(elixir).
-export([boot/0, eval/1, eval/2, parse/1]).
-include("elixir.hrl").

% Boot up Elixir setting up tables and loading main files.
boot() ->
  code:ensure_loaded(elixir_object_methods),
  load_core_classes().

% Load core elixir classes
load_core_classes() ->
  Dirname = filename:dirname(?FILE),
  Basepath = filename:join([Dirname, "..", "lib"]),
  Loader = fun(Class) -> load_file(filename:join(Basepath, Class)) end,
  lists:foreach(Loader, [
    'object.ex',
    'module.ex',
    'numeric.ex',
    'integer.ex',
    'float.ex'
  ]).

% Loads a given file
% TODO Binding here should pass self = Object
load_file(Filepath) ->
  {ok, Bin} = file:read_file(Filepath),
  eval(binary_to_list(Bin)).

% Evaluates a string
eval(String) -> eval(String, []).

eval(String, Binding) ->
  {value, Value, NewBinding} = erl_eval:exprs(parse(String), Binding),
  {Value, NewBinding}.

% Parse string and transform tree to Erlang Abstract Form format
parse(String) ->
	{ok, Tokens, _} = elixir_lexer:string(String),
	{ok, Forms} = elixir_parser:parse(Tokens),
	{NewForms, _ } = transform_tree(Forms, [self], []),
	NewForms.

% Transform a tree given the function.
transform_tree(Forms, V, S) ->
  mapfoldl_tree(Forms, fun transform/3, V, S).

mapfoldl_tree(Forms, Fun, V, S) ->
  Transform = fun(X, Acc) -> Fun(X, Acc, S) end,
  lists:mapfoldl(Transform, V, Forms).

% TODO transformations should contain the filename

% A transformation receives a node with a Filename and a Scope
% and transforms it to Erlang Abstract Form.

% Represents a method call. The arguments need to be packed into
% an array before sending it to dispatch (which has fixed arity).
transform({method_call, Line, Name, Args, Expr}, V, S) ->
  { TArgs, _ } = transform({list, Line, Args}, V, S),
  { TExpr, _ } = transform(Expr, V, S),
  { ?ELIXIR_WRAP_CALL(Line, elixir_dispatch, dispatch, [TExpr, {atom, Line, Name}, TArgs]), V };

transform({constant, Line, Name}, V, S) ->
  { ?ELIXIR_WRAP_CALL(Line, elixir_constants, lookup, [{atom, Line, Name}]), V };

transform({fun_call, Line, Var, Args }, V, S) ->
  { TArgs, _ } = transform_tree(Args, V, S),
  { TVar, _ }  = transform(Var, V, S),
  { {call, Line, TVar, TArgs}, V };

transform({match, Line, Left, Right}, V, S) ->
  { TLeft, _ } = transform(Left, V, S),
  { TRight, _ } = transform(Right, V, S),
  { {match, Line, TLeft, TRight }, V };

transform({tuple, Line, Exprs }, V, S) ->
  { TExprs, _ } = transform_tree(Exprs, V, S),
  { {tuple, Line, TExprs}, V };

transform({list, Line, Exprs }, V, S) ->
  { TExprs, _ } = transform_tree(Exprs, V, S),
  Transform = fun(Expr, Acc) -> {cons, Line, Expr, Acc} end,
  { lists:foldr(Transform, {nil, Line}, TExprs), V };

transform({binary_op, Line, Op, Left, Right}, V, S) ->
  { TLeft, _ } = transform(Left, V, S),
  { TRight, _ } = transform(Right, V, S),
  Args = { cons, Line, TRight, {nil, Line} },
  { ?ELIXIR_WRAP_CALL(Line, elixir_dispatch, dispatch, [TLeft, {atom, Line, Op}, Args]), V };

transform({unary_op, Line, Op, Right}, V, S) ->
  { TRight, _ } = transform(Right, V, S),
  { { op, Line, Op, TRight }, V };

transform({'fun', Line, Clauses}, V, S) ->
  { TClauses, _ } = transform(Clauses, V, S),
  { { 'fun', Line, TClauses }, V };

% Clauses do not use transform_subtree because variables in one
% clause do not affect the other.
transform({clauses, Clauses}, V, S) ->
  TClauses = [element(1, transform(Clause, V, S)) || Clause <- Clauses],
  { { clauses, TClauses }, V };

transform({clause, Line, Args, Guards, Exprs}, V, S) ->
  { TArgs, _  } = transform_tree(Args, V, S),
  { TExprs, _ } = transform_tree(Exprs, V, S),
  { { clause, Line, TArgs, Guards, TExprs }, V };

transform({object, Line, Name, Exprs}, V, S) ->
  Scope = elixir_module:scope_for(S, Name),
  { TExprs, _ } = transform_tree(Exprs, V, Scope),
  { elixir_module:transform(object, Line, Scope, TExprs), V };

transform({module, Line, Name, Exprs}, V, S) ->
  Scope = elixir_module:scope_for(S, Name),
  { TExprs, _ } = transform_tree(Exprs, V, Scope),
  { elixir_module:transform(module, Line, Scope, TExprs), V };

transform({erlang_call, Line, Prefix, Suffix, Args}, V, S) ->
  { TPrefix, _ } = transform(Prefix, V, S),
  { TSuffix, _ } = transform(Suffix, V, S),
  { TArgs, _ }   = transform_tree(Args, V, S),
  { ?ELIXIR_WRAP_CALL(Line, TPrefix, TSuffix, TArgs), V };

% TODO This cannot be tested yet, because in theory the parser will
% never allow us to have this behavior. In any case, we will need
% to wrap it in the future by Elixir exception handling.
transform({method, Line, Name, Arity, Clauses}, F, []) ->
  erlang:error("Method definition outside the scope.");

% Method definitions are never executed by Elixir runtime. Their
% abstract form is stored into an ETS table and is just added to
% an Erlang module when they are compiled.
transform({method, Line, Name, Arity, Clauses}, V, S) ->
  { TClauses, _ } = mapfoldl_tree(Clauses, fun pack_method_clause/3, V, S),
  Method = {function, Line, Name, Arity + 1, TClauses},
  { elixir_module:wrap_method_definition(S, Line, Method), V };

% Match all other expressions.
transform(Expr, V, S) -> { Expr, V }.

% Pack method clause in a format that receives Elixir metadata
% as first argument (like self) and annotates __current__ with
% the current module name (for super)
pack_method_clause({clause, Line, Args, Guards, Exprs}, V, S) -> 
  transform({clause, Line, [{var, Line, self}|Args], Guards, Exprs}, V, S).