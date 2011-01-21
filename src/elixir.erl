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
  Transform = fun(X, Acc) -> [transform(X, [], [])|Acc] end,
  lists:foldr(Transform, [], Forms).

% TODO transformations should contain the filename

% A transformation receives a node with a Filename and a Scope
% and transforms it to Erlang Abstract Form.

% Represents a method call. The arguments need to be packed into
% an array before sending it to dispatch (which has fixed arity).
transform({method_call, Line, Name, Args, Expr}, F, S) ->
  TransformedArgs = transform({list, Line, Args}, F, S),
  ?ELIXIR_WRAP_CALL(Line, elixir_dispatch, dispatch, [transform(Expr, F, S), {atom, Line, Name}, TransformedArgs]);

transform({constant, Line, Name}, F, S) ->
  ?ELIXIR_WRAP_CALL(Line, elixir_constants, lookup, [{atom, Line, Name}]);

transform({fun_call, Line, Vars, Args }, F, S) ->
  {call, Line, Vars, [transform(Arg, F, S) || Arg <- Args]};

transform({match, Line, Left, Right}, F, S) ->
  {match, Line, transform(Left, F, S), transform(Right, F, S)};

transform({tuple, Line, Exprs }, F, S) ->
  {tuple, Line, [transform(Expr, F, S) || Expr <- Exprs]};

transform({list, Line, Exprs }, F, S) ->
  Transform = fun(X, Acc) -> {cons, Line, transform(X, F, S), Acc} end,
  lists:foldr(Transform, {nil, Line}, Exprs);

transform({binary_op, Line, Op, Left, Right}, F, S) ->
  Args = { cons, Line, transform(Right, F, S), {nil, Line} },
  ?ELIXIR_WRAP_CALL(Line, elixir_dispatch, dispatch, [transform(Left, F, S), {atom, Line, Op}, Args]);

transform({unary_op, Line, Op, Right}, F, S) ->
  {op, Line, Op, transform(Right, F, S)};

transform({'fun', Line, Clauses}, F, S) ->
  {'fun', Line, transform(Clauses, F, S)};

transform({clauses, Clauses}, F, S) ->
  {clauses, [transform(Clause, F, S) || Clause <- Clauses]};

transform({clause, Line, Args, Guards, Exprs}, F, S) ->
  {clause, Line, [transform(Arg, F, S) || Arg <- Args], Guards, [transform(Expr, F, S) || Expr <- Exprs]};

transform({object, Line, Name, Exprs}, F, S) ->
  Scope = elixir_module:scope_for(S, Name),
  Body = [transform(Expr, F, Scope) || Expr <- Exprs],
  elixir_module:transform(object, Line, Scope, Body);

transform({module, Line, Name, Exprs}, F, S) ->
  Scope = elixir_module:scope_for(S, Name),
  Body = [transform(Expr, F, Scope) || Expr <- Exprs],
  elixir_module:transform(module, Line, Scope, Body);

transform({erlang_call, Line, Prefix, Suffix, Args}, F, S) ->
  ?ELIXIR_WRAP_CALL(Line, transform(Prefix, F, S), transform(Suffix, F, S), [transform(Arg, F, S) || Arg <- Args]);

transform({const_assign, Line, Left, Right}, F, S) ->
  ?ELIXIR_WRAP_CALL(Line, elixir_constants, store, [{atom, Line, Left}, transform(Right, F, S)]);

% TODO This cannot be tested yet, because in theory the parser will
% never allow us to have this behavior. In any case, we will need
% to wrap it in the future by Elixir exception handling.
transform({method, Line, Name, Arity, Clauses}, F, []) ->
  erlang:error("Method definition outside the scope.");

% Method definitions are never executed by Elixir runtime. Their
% abstract form is stored into an ETS table and is just added to
% an Erlang module when they are compiled.
transform({method, Line, Name, Arity, Clauses}, F, S) ->
  TClauses = [transform(pack_method_clause(Clause), F, S) || Clause <- Clauses],
  Method = {function, Line, Name, Arity + 1, TClauses},
  elixir_module:wrap_method_definition(S, Line, Method);

% Match all other expressions.
transform(Expr, F, S) -> Expr.

% Pack method clause in a format that receives Elixir metadata
% as first argument (like self) and annotates __current__ with
% the current module name (for super)
pack_method_clause({clause, Line, Args, Guards, Exprs}) -> 
  {clause, Line, [{var, Line, self}|Args], Guards, Exprs}.