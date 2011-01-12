-module(elixir).
-export([eval/1, eval/2, throw_elixir/1, throw_erlang/1, store_method_for/3]).

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
  Transform = fun(X, Acc) -> [transform(X, [], [])|Acc] end,
  lists:foldr(Transform, [], Forms).

% TODO transformations should contain the filename

% A transformation receives a node with a Filename and a Scope
% and transforms it to Erlang Abstract Form.
transform({match, Line, Left, Right}, F, S) ->
  {match, Line, transform(Left, F, S), transform(Right, F, S)};

transform({binary_op, Line, Op, Left, Right}, F, S) ->
  {op, Line, Op, transform(Left, F, S), transform(Right, F, S)};

transform({unary_op, Line, Op, Right}, F, S) ->
  {op, Line, Op, transform(Right, F, S)};

transform({'fun', Line, Clauses}, F, S) ->
  {'fun', Line, transform(Clauses, F, S)};

transform({clauses, Clauses}, F, S) ->
  {clauses, [transform(Clause, F, S) || Clause <- Clauses]};

transform({clause, Line, Args, Guards, Exprs}, F, S) ->
  {clause, Line, Args, Guards, [transform(Expr, F, S) || Expr <- Exprs]};

transform({call, Line, Vars, Args }, F, S) ->
  {call, Line, Vars, [transform(Arg, F, S) || Arg <- Args]};

transform({module, Line, Name, Exprs}, F, S) ->
  Options = [ordered_set, private],
  CompiledTable = ets:new(prepend_to_atom(c, Name), Options),
  AddedTable = ets:new(prepend_to_atom(a, Name), Options),
  Body = [transform(Expr, F, {CompiledTable, AddedTable}) || Expr <- Exprs],
  {value, Value, _} = erl_eval:exprs(Body, []),
  load_module(build_module(Line, Name, AddedTable)),
  ets:delete(CompiledTable),
  ets:delete(AddedTable),
  {nil, Line};

% TODO This cannot be tested, because in theory the parser will never
% allow us to have this behavior. In any case, we will need to wrap
% it in the future by Elixir exception handling.
transform({method, Line, Name, Arity, Clauses}, F, []) ->
  erlang:error("Method definition outside the scope.");
  
transform({method, Line, Name, Arity, Clauses}, F, S) ->
  TClauses = [transform(Clause, F, S) || Clause <- Clauses],
  Method = {function, Line, Name, Arity, TClauses},
  { CompiledTable, AddedTable } = S,
  Index = append_to_table(CompiledTable, Method),
  wrap_into_call(elixir, store_method_for,
    [{integer, 0, Index}, {integer, 0, CompiledTable}, {integer, 0, AddedTable}]
  );

% Match all other expressions
transform(Expr, F, S) -> Expr.

prepend_to_atom(Prefix, Atom) ->
  list_to_atom(lists:concat([Prefix, Atom])).

% Related to module compilation
append_to_table(Table, Set) ->
  Last = ets:last(Table),
  Index = next_table_index(Table),
  true = ets:insert(Table, {Index, Set}),
  Index.

next_table_index('$end_of_table') -> 1;
next_table_index(I) -> I + 1.

store_method_for(Index, CompiledTable, AddedTable) ->
  [{Index, Function}] = ets:lookup(CompiledTable, Index),
  append_to_table(AddedTable, Function).

build_module(Line, Name, Table) ->
  Pairs = ets:tab2list(Table),
  Functions = [element(2, Pair) || Pair <- Pairs],
  Export = [{element(3, Function), element(4, Function)} || Function <- Functions],
  [{attribute, Line, module, Name}, {attribute, Line, export, Export} | Functions].

load_module(Forms) ->
  case compile:forms(Forms) of
     {ok,ModuleName,Binary}           -> code:load_binary(ModuleName, "nofile", Binary);
     {ok,ModuleName,Binary,_Warnings} -> code:load_binary(ModuleName, "nofile", Binary)
  end.

wrap_into_call(Parent, Method, Args) ->
  { call, 0,
    { remote, 0, { atom, 0, Parent }, { atom, 0, Method} },
    Args
  }.