-module(elixir).
-export([boot/0, eval/1, eval/2, parse/1]).
-include("elixir.hrl").
-import(lists, [umerge/1, umerge/2]).

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
  Transform = fun(X, Acc) -> transform(X, Acc, S) end,
  lists:mapfoldl(Transform, V, Forms).

% A transformation receives a node with a variables list (V),
% a scope (S) and transforms it to Erlang Abstract Form.

% Represents a method call. The arguments need to be packed into
% an array before sending it to dispatch (which has fixed arity).
%
% = Variables
%
% Both the prefix of the function as the arguments can declare new variables:
%
%   (a = 1).+(b = 2)
%
transform({method_call, Line, Name, Args, Expr}, V, S) ->
  { TArgs, VA } = transform({list, Line, Args}, V, S),
  { TExpr, VE } = transform(Expr, V, S),
  { ?ELIXIR_WRAP_CALL(Line, elixir_dispatch, dispatch, [TExpr, {atom, Line, Name}, TArgs]), umerge([V,VA,VE]) };

% Reference to a constant (that should then be loaded).
%
% = Variables
%
% It has no affect on variables scope.
transform({constant, Line, Name}, V, S) ->
  { ?ELIXIR_WRAP_CALL(Line, elixir_constants, lookup, [{atom, Line, Name}]), V };

% Handle function calls.
%
% = Variables
%
% Both the left and right side can contain variable declarations, as below:
%
%   (a = -> (x) x + 2)(b = 1)
%
% So we need to take both into account.
transform({fun_call, Line, Var, Args }, V, S) ->
  { TArgs, VA } = transform_tree(Args, V, S),
  { TVar, VV }  = transform(Var, V, S),
  { {call, Line, TVar, TArgs}, umerge([V, VA, VV]) };

% Handle match declarations.
%
% = Variables
%
% Both the left and right side can contain variable declarations, as below:
%
%   a = (b = 1)
%
% So we need to take both into account.
transform({match, Line, Left, Right}, V, S) ->
  { TLeft, VL } = transform(Left, V, S),
  { TRight, VR } = transform(Right, V, S),
  { {match, Line, TLeft, TRight }, umerge([V, VL, VR]) };

% Handle tuple declarations.
%
% = Variables
%
% Each expression in the tuple can contain a match expression.
% Variables defined inside these expressions needs to be added to the var list.
transform({tuple, Line, Exprs }, V, S) ->
  { TExprs, VE } = transform_tree(Exprs, V, S),
  { {tuple, Line, TExprs}, umerge(V, VE) };

% Handle list declarations.
%
% = Variables
%
% Each expression in the list can contain a match expression.
% Variables defined inside these expressions needs to be added to the var list.
transform({list, Line, Exprs }, V, S) ->
  { TExprs, VE } = transform_tree(Exprs, V, S),
  Transform = fun(Expr, Acc) -> {cons, Line, Expr, Acc} end,
  { lists:foldr(Transform, {nil, Line}, TExprs), umerge(V, VE) };

% Handle binary operations.
%
% = Variables
%
% The Left and Right values of the binary operation can be a match expression.
% Variables defined inside these expressions needs to be added to the list.
transform({binary_op, Line, Op, Left, Right}, V, S) ->
  { TLeft, VL } = transform(Left, V, S),
  { TRight, VR } = transform(Right, V, S),
  Args = { cons, Line, TRight, {nil, Line} },
  { ?ELIXIR_WRAP_CALL(Line, elixir_dispatch, dispatch, [TLeft, {atom, Line, Op}, Args]), umerge([V, VL, VR]) };

% Handle unary operations.
%
% = Variables
%
% The target (Right) of the unary operation can be a match expression.
% Variables defined inside these expressions needs to be added to the list.
transform({unary_op, Line, Op, Right}, V, S) ->
  { TRight, V1} = transform(Right, V, S),
  { { op, Line, Op, TRight }, umerge(V, V1) };

% Handle functions declarations. They preserve the current binding.
%
% = Variables
%
% Variables defined inside functions do not leak to the outer scope
% but variables previously defined affect the current function.
transform({'fun', Line, {clauses, Clauses}}, V, S) ->
  TClauses = [transform(Clause, V, S) || Clause <- Clauses],
  { { 'fun', Line, {clauses, TClauses} }, V };

% Handle function clauses.
%
% = Variables
%
% Variables declared in args do affect the exprs and should be taken
% into account. Clauses do not return variables list as second argument
% because variables in one clause should not affect the other.
transform({clause, Line, Args, Guards, Exprs}, V, S) ->
  { TArgs, V1 } = transform_tree(Args, V, S),
  { TExprs, _ } = transform_tree(Exprs, umerge(V, V1), S),
  { clause, Line, TArgs, Guards, TExprs };

% Handle objects declarations.
%
% = Variables
%
% Objects do not share binding with the previous context, so
% previous variable declarations do not affect a module and
% variables declared in a module do not leak outside its
% context. The only variable available in the module by default
% is self.
transform({object, Line, Name, Exprs}, V, S) ->
  Scope = elixir_module:scope_for(S, Name),
  { TExprs, _ } = transform_tree(Exprs, [self], Scope),
  { elixir_module:transform(object, Line, Scope, TExprs), V };

% Handle module declarations.
%
% = Variables
%
% Modules do not share binding with the previous context, so
% previous variable declarations do not affect a module and
% variables declared in a module do not leak outside its
% context. The only variable available in the module by default
% is self.
transform({module, Line, Name, Exprs}, V, S) ->
  Scope = elixir_module:scope_for(S, Name),
  { TExprs, _ } = transform_tree(Exprs, [self], Scope),
  { elixir_module:transform(module, Line, Scope, TExprs), V };

% Handles erlang function calls in the following format:
%
%   erl.lists.mapfoldr()
%
% = Variables
%
% Variables can be set inside the args hash, so they need
% to be taken into account on the variables list.
transform({erlang_call, Line, Prefix, Suffix, Args}, V, S) ->
  { TArgs, V1 } = transform_tree(Args, V, S),
  { ?ELIXIR_WRAP_CALL(Line, Prefix, Suffix, TArgs), umerge(V, V1) };

% TODO This cannot be tested yet, because in theory the parser will
% never allow us to have this behavior. In any case, we will need
% to wrap it in the future by Elixir exception handling.
transform({method, Line, Name, Arity, Clauses}, F, []) ->
  erlang:error("Method definition outside the scope.");

% Method definitions are never executed by Elixir runtime. Their
% abstract form is stored into an ETS table and is just added to
% an Erlang module when they are compiled.
%
% = Variables
%
% Variables are handled in each function clause.
transform({method, Line, Name, Arity, Clauses}, V, S) ->
  TClauses = [pack_method_clause(Clause, V, S) || Clause <- Clauses],
  Method = {function, Line, Name, Arity + 1, TClauses},
  { elixir_module:wrap_method_definition(S, Line, Method), V };

% Match all other expressions.
transform(Expr, V, S) -> { Expr, V }.

% Pack method clause in a format that receives Elixir metadata
% as first argument (like self) and annotates __current__ with
% the current module name (for super).
%
% = Variables
%
% It does not accummulate variables because variables in one
% clause do not affect the other. Each clause starts with an
% empty variable set as there is no binding.
pack_method_clause({clause, Line, Args, Guards, Exprs}, V, S) -> 
  Clause = {clause, Line, [{var, Line, self}|Args], Guards, Exprs},
  transform(Clause, [], S).