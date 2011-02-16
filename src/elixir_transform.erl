-module(elixir_transform).
-export([parse/3]).
-include("elixir.hrl").
-import(lists, [umerge/2]).

parse(String, Binding, Filename) ->
  Vars = lists:usort(proplists:get_keys(Binding)),
  parse(String, 1, Vars, #elixir_scope{filename=Filename}).

parse(String, Line, V, #elixir_scope{filename=Filename} = S) ->
  Forms = forms(String, Line, Filename),
  transform_tree(Forms, V, S).

forms(String, StartLine, Filename) ->
  case elixir_lexer:string(String, StartLine) of
    {ok, Tokens, _} -> 
      case elixir_parser:parse(Tokens) of
        {ok, Forms} -> Forms;
        {error, {Line, _, [Error, Token]}} -> elixir_errors:syntax_error(Line, Filename, Error, Token)
      end;
    {error, {Line, _, {Error, Token}}, _} -> elixir_errors:syntax_error(Line, Filename, Error, Token)
  end.  

% Transform the given tree Forms.
%
% V is a list of variables that were bound (used for arranging
% implicit self) and S is the scope. The scope is a tuple with
% two elements. The first is a boolean that says if we are in a
% scope where new variables can be defined (i.e. the left side
% of a match or function clauses) and the second the nested
% module name.
transform_tree(Forms, V, S) ->
  Transform = fun(X, Acc) -> transform(X, Acc, S) end,
  lists:mapfoldl(Transform, V, Forms).

% A transformation receives a node with a variables list (V),
% a scope (S) and transforms it to Erlang Abstract Form.

% Handles identifiers, i.e. method calls or variable calls, allowing
% implicit self.
%
% = Variables
%
% If the scope has true for variables, it means new variables can be
% defined. In such cases, variables are added to the list if they don't
% exist yet. If we cannot define a variable and it does not belong to
% the list, make it a method call.
transform({identifier, Line, Name}, V, S) ->
  Var = S#elixir_scope.vars,
  case { Var, lists:member(Name, V) } of
    { _, true }      -> { {var, Line, Name}, V };
    { true, false }  -> { {var, Line, Name}, lists:sort([Name|V]) };
    { false, false } -> transform({local_call, Line, Name, []}, V, S)
  end;

% Represents a method call. The arguments need to be packed into
% an array before sending it to dispatch (which has fixed arity).
%
% = Variables
%
% Both the prefix of the function as the arguments can declare new variables:
%
%   (a = 1).+(b = 2)
%
% = new
%
% This method special cases new by wrapping all arguments into an array.
% This is required so Object.new() implementation can handle all arguments.
% This case could also be implemented in the dispatcher, but would affect
% performance.
transform({method_call, Line, Name, Args, Expr}, V, S) ->
  { TExpr, VE } = transform(Expr, V, S),
  { TArgs, VA } = transform({list, Line, Args, {nil, Line}}, V, S),
  { build_method_call(Name, Line, TArgs, TExpr), umerge(VA,VE) };

% Makes a local call.
%
% If a local call is done inside a method, they don't go through the method
% dispatching path and always call functions in the same module. Othewise,
% they are always treated as a remote call.
%
% = Variables
%
% New variables can be defined inside the arguments list.
transform({local_call, Line, Name, Args}, V, S) ->
  case S#elixir_scope.method of
    true ->
      { TArgs, VA } = transform_tree(Args, V, S),
      FArgs = handle_new_call(Name, Line, [{var, Line, self}|TArgs]),
      { { call, Line, {atom, Line, Name}, FArgs }, VA };
    false -> transform({method_call, Line, Name, Args, {var, Line, self}}, V, S)
  end;

% Reference to a constant (that should then be loaded).
%
% = Variables
%
% It has no affect on variables scope.
transform({constant, Line, Name}, V, S) ->
  { ?ELIXIR_WRAP_CALL(Line, elixir_constants, lookup, [{atom, Line, Name}]), V };

% Reference to an instance variable (that should then be loaded).
%
% = Variables
%
% It has no affect on variables scope.
transform({ivar, Line, Name}, V, S) ->
  Args = [{var, Line, self}, {atom, Line, Name}],
  { ?ELIXIR_WRAP_CALL(Line, elixir_object_methods, get_ivar, Args), V };

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
  { TLeft, VL } = transform(Left, V, S#elixir_scope{vars=true}),
  { TRight, VR } = transform(Right, V, S),
  { {match, Line, TLeft, TRight }, umerge(VL, VR) };

% Handle tuple declarations.
%
% = Variables
%
% Each expression in the tuple can contain a match expression.
% Variables defined inside these expressions needs to be added to the var list.
transform({tuple, Line, Exprs }, V, S) ->
  { TExprs, VE } = transform_tree(Exprs, V, S),
  { {tuple, Line, TExprs}, VE };

% Handle list declarations.
%
% = Variables
%
% Each expression in the list can contain a match expression.
% Variables defined inside these expressions needs to be added to the var list.
transform({list, Line, Exprs, Tail }, V, S) ->
  Transformer = fun (X, Acc) -> transform(X, Acc, S) end,
  { TTail, VT }  = transform(Tail, V, S),
  { TExprs, VE } = build_list(Transformer, Exprs, Line, V, TTail),
  { TExprs, umerge(VE, VT) };

% Handle orddict declarations. It simply delegates to list to build a list
% of args that is dispatched to orddict:from_list/1. The final Dict
% object is created explicitly and not through Dict.new.
%
% = Variables
%
% See list.
transform({orddict, Line, Exprs }, V, S) ->
  { List, NV } = transform({list, Line, Exprs, {nil, Line} }, V, S),
  Dict = ?ELIXIR_WRAP_CALL(Line, orddict, from_list, [List]),
  { {tuple, Line, [{atom, Line, elixir_orddict__}, Dict] }, NV };

% Handle binaries declarations.
%
% = Variables
%
% Variables can not be defined inside each bin element.
transform({bin, Line, Exprs }, V, S) ->
  { TExprs, NV } = transform_tree(Exprs, V, S),
  { { bin, Line, TExprs }, NV };

transform({bin_element, Line, Expr, Type, Specifiers }, V, S) ->
  { TExpr, NV } = transform(Expr, V, S),
  { { bin_element, Line, TExpr, Type, Specifiers }, NV };

% Handle strings by wrapping them in the String object. A string is created
% by explicitly creating an #elixir_object__ and not through String.new.
%
% = Variables
%
% No variables can be defined in a string without interpolation.
transform({string, Line, String } = Expr, V, S) ->
  { { tuple, Line, [{atom, Line, elixir_string__}, build_bin(Line, [Expr])] }, V };

% Handle interpolated strings declarations. A string is created
% by explicitly creating an #elixir_object__ and not through String.new.
%
% = Variables
%
% Variables can be defined inside the interpolation.
transform({interpolated_string, Line, String }, V, S) ->
  { List, VE } = handle_interpolations(String, Line, V, S),
  Binary = ?ELIXIR_WRAP_CALL(Line, erlang, iolist_to_binary, [List]),
  { { tuple, Line, [{atom, Line, elixir_string__}, Binary] }, VE };

% Handle interpolated atoms by converting them to lists and calling atom_to_list.
%
% = Variables
%
% Variables can be defined inside the interpolation.
transform({interpolated_atom, Line, String}, V, S) ->
  { List, VE } = handle_interpolations(String, Line, V, S),
  Binary = ?ELIXIR_WRAP_CALL(Line, erlang, iolist_to_binary, [List]),
  { ?ELIXIR_WRAP_CALL(Line, erlang, binary_to_atom, [Binary, {atom, Line, utf8}]), VE };

% Handle regexps by dispatching a list and its options to Regexp.new.
%
% = Variables
%
% No variables can be defined in a string without interpolation.
transform({regexp, Line, String, Operators }, V, S) ->
  build_regexp(Line, {string, Line, String}, Operators, V, S);

% Handle interpolated regexps by dispatching a list and its options to Regexp.new.
%
% = Variables
%
% Variables can be defined inside the interpolation.
transform({interpolated_regexp, Line, String, Operators }, V, S) ->
  { List, VE } = handle_interpolations(String, Line, V, S),
  Binary = ?ELIXIR_WRAP_CALL(Line, erlang, iolist_to_binary, [List]),
  build_regexp(Line, Binary, Operators, VE, S);

% Handle char lists by simply converting them to Erlang strings.
%
% = Variables
%
% No variables can be defined in a char list without interpolation.
transform({char_list, Line, String } = Expr, V, S) ->
  { {string, Line, String}, V };

% Handle interpolated strings declarations. A string is created
% by explicitly creating an #elixir_object__ and not through String.new.
%
% = Variables
%
% Variables can be defined inside the interpolation.
transform({interpolated_char_list, Line, String }, V, S) ->
  { List, VE } = handle_interpolations(String, Line, V, S),
  Binary = ?ELIXIR_WRAP_CALL(Line, erlang, iolist_to_binary, [List]),
  Flattened = ?ELIXIR_WRAP_CALL(Line, erlang, binary_to_list, [Binary]),
  { Flattened, VE };

% Handle comparison operations.
%
% = Variables
%
% The Left and Right values of the comparison operation can be a match expression.
% Variables defined inside these expressions needs to be added to the list.
transform({comp_op, Line, Op, Left, Right}, V, S) ->
  { TLeft, VL } = transform(Left, V, S),
  { TRight, VR } = transform(Right, V, S),
  build_comp_op(Line, Op, TLeft, TRight, umerge(VL, VR));

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
  { build_method_call(Op, Line, Args, TLeft), umerge(VL, VR) };

% Handle unary operations.
%
% = Variables
%
% The target (Right) of the unary operation can be a match expression.
% Variables defined inside these expressions needs to be added to the list.
transform({unary_op, Line, Op, Right}, V, S) ->
  { TRight, V1} = transform(Right, V, S),
  { build_unary_op(Line, Op, TRight), V1 };

% Handle if/elsif/else expressions.
%
% = Variables
%
% The expression to be evaluated by the if expression can create
% new variables. The expression list after evaluation can also
% generate new variables and these new variables may be available
% outside the if/else expression. Consider this example:
%
%     module Foo
%       def foo; 1; end
%
%       def bar(x)
%         if x
%           foo = 2
%         else
%           foo = foo
%         end
%         foo
%       end
%     end
%
%     Foo.bar(true)  % => 2
%     Foo.bar(false) % => 1
%
% The example above shows two important things. A variable defined
% inside if/else can be available outside the clauses, but, if that
% happens, all clauses need to define this variable.
%
% Second, a variable defined in a clause does not affect other clauses,
% so the second clause above could sucessfully invoke the method foo.
transform({'if', Line, [If|Elsifs], Else}, V, S) ->
  { TIf, IfV } = transform(If, {V,[]}, S),
  { TElsifs, {ExprV, ListV} } = transform_tree(Elsifs, IfV, S),
  { TElse, ElseV } = transform_tree(Else, ExprV, S),
  { hd(lists:foldr(fun build_if_clauses/2, TElse, [TIf|TElsifs])), umerge(ElseV, ListV) };

transform({if_clause, Line, Bool, Expr, List}, {ExprV, ListV}, S) ->
  { TExpr, TExprV } = transform(Expr, ExprV, S),
  { TList, TListV } = transform_tree(List, TExprV, S),
  { {if_clause, Line, Bool, TExpr, TList }, { TExprV, umerge(ListV, TListV) } };

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
  { TArgs, NV } = transform_tree(Args, V, S#elixir_scope{vars=true}),
  { TExprs, _ } = transform_tree(Exprs, NV, S),
  { clause, Line, TArgs, Guards, TExprs };

% Handles erlang function calls in the following format:
%
%   Erlang.lists.mapfoldr()
%
% = Variables
%
% Variables can be set inside the args hash, so they need
% to be taken into account on the variables list.
transform({erlang_call, Line, Prefix, Suffix, Args}, V, S) ->
  { TArgs, VA } = transform_tree(Args, V, S),
  { ?ELIXIR_WRAP_CALL(Line, Prefix, Suffix, TArgs), VA };

% Method definitions are never executed by Elixir runtime. Their
% abstract form is stored into an ETS table and is just added to
% an Erlang module when they are compiled.
%
% = Variables
%
% Variables are handled in each function clause.
%
% TODO Test that a method declaration outside a module raises an error.
transform({def_method, Line, Name, Arity, Clauses}, V, S) ->
  Module = S#elixir_scope.module,
  NewScope = S#elixir_scope{method=true},
  TClauses = [pack_method_clause(Clause, V, NewScope) || Clause <- Clauses],
  Method = {function, Line, Name, Arity + 1, TClauses},
  { elixir_methods:wrap_method_definition(Module, Line, S#elixir_scope.filename, Method), V };

% Handle function calls.
%
% = Variables
%
% Both the left and right side can contain variable declarations, as below:
%
%   (a = -> (x) x + 2)(b = 1)
%
% So we need to take both into account.
%
% Also, there are a few cases where a function may be ambigous with a method call:
%
%    module Foo
%      def bar; 1; end
%      def baz; bar(); end
%    end
%
% This is parsed as a function call but is properly disambiguated to a method
% call in this method.
transform({fun_call, Line, Var, Args }, V, S) ->
  case Var of
    { identifier, _, Name } -> Method = not lists:member(Name, V);
    Name -> Method = false
  end,

  case Method of
    true -> transform({local_call, Line, Name, Args}, V, S);
    false ->
      { TArgs, VA } = transform_tree(Args, V, S),
      { TVar, VV }  = transform(Var, V, S),
      { {call, Line, TVar, TArgs}, umerge(VA, VV) }
  end;

% Handle module/object declarations. The difference between
% them is specified in Parent.
%
% = Variables
%
% Objects do not share binding with the previous context, so
% previous variable declarations do not affect a module and
% variables declared in a module do not leak outside its
% context. The only variable available in the module by default
% is self.
transform({Kind, Line, Name, Parent, Exprs}, V, S) when Kind == object; Kind == module->
  Current = S#elixir_scope.module,
  Filename = S#elixir_scope.filename,
  NewName = elixir_object:scope_for(Current, Name),
  { TExprs, _ } = transform_tree(Exprs, [self], S#elixir_scope{method=false,module=NewName}),
  { elixir_object:transform(Kind, Line, Filename, NewName, Parent, TExprs), V };

% Handles __FILE__
%
% = Variables
%
% No variables can be defined.
transform({filename, Line}, V, S) ->
  transform({string, Line, S#elixir_scope.filename}, V, S);

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
  transform(Clause, [self], S).

% Build a list transforming each expression and accumulating
% vars in one pass. It uses tail-recursive form.
%
% It receives a function to transform each expression given
% in Exprs, a Line used to build the List and the variables
% scope V is passed down item by item.
%
% The function needs to return a tuple where the first element
% is an erlang abstract form and the second is the new variables
% list.
build_list(Fun, Exprs, Line, V) ->
  build_list(Fun, Exprs, Line, V, {nil, Line}).

build_list(Fun, Exprs, Line, V, Tail) ->
  build_list_each(Fun, lists:reverse(Exprs), Line, V, Tail).

build_list_each(Fun, [], Line, V, Acc) ->
  { Acc, V };

build_list_each(Fun, [H|T], Line, V, Acc) ->
  { Expr, NV } = Fun(H, V),
  build_list_each(Fun, T, Line, NV, { cons, Line, Expr, Acc }).

% Build binaries
build_bin(Line, Exprs) ->
  Transformer = fun (X) -> { bin_element, Line, X, default, default } end,
  { bin, Line, lists:map(Transformer, Exprs) }.

% Build if/elsif/else clauses by nesting then one inside the other.
% Assumes expressions were already converted.
build_if_clauses({if_clause, Line, Bool, Expr, List}, Acc) ->
  True  = [{atom,Line,true}],
  False = [{atom,Line,false}],

  [{ 'case', Line, convert_to_boolean(Line, Expr, Bool), [
    { clause, Line, True, [], List },
    { clause, Line, False, [], Acc }
  ] }].

% Handles method calls. It performs no transformation and assumes
% all data is already transformed.
build_method_call(Name, Line, Args, Expr) ->
  FArgs = handle_new_call(Name, Line, Args),
  ?ELIXIR_WRAP_CALL(Line, elixir_dispatch, dispatch, [{var, Line, self}, Expr, {atom, Line, Name}, FArgs]).

% Builds a regexp.
build_regexp(Line, Expr, Operators, V, S) ->
  Args = [Expr, {string, Line, Operators}],
  { TArgs, _ } = transform({list, Line, Args, {nil, Line}}, V, S),
  { Constant, _ } = transform({constant, Line, 'Regexp'}, V, S),
  { build_method_call(new, Line, TArgs, Constant), V }.

% Handle method dispatches to new by wrapping everything in an array
% as we don't have a splat operator.
handle_new_call(new, Line, Args) ->
  {cons, Line, Args, {nil, Line}};

handle_new_call(_, _, Args) ->
  Args.

% Handle interpolation. The final result will be a parse tree that
% returns a flattened list.
handle_interpolations(String, Line, V, S) ->
  Interpolations = String,

  % Optimized cases interpolations actually has no interpolation.
  case Interpolations of
    [{s, String}] -> handle_string_extractions(hd(Interpolations), Line, V, S);
    _ ->
      Transformer = fun(X, Acc) -> handle_string_extractions(X, Line, Acc, S) end,
      build_list(Transformer, Interpolations, Line, V)
  end.

% Handle string extractions for interpolated strings.
handle_string_extractions({s, String}, Line, V, S) ->
  { { string, Line, String }, V };

handle_string_extractions({i, Interpolation}, Line, V, S) ->
  { Tree, NV } = parse(Interpolation, Line, V, S),
  Stringify = build_method_call(to_s, Line, {nil,Line}, hd(Tree)),
  { ?ELIXIR_WRAP_CALL(Line, erlang, element, [{integer, Line, 2}, Stringify]), NV }.

% Convert the given expression to a boolean value: true or false.
% Assumes the given expressions was already transformed.
convert_to_boolean(Line, Expr, Bool) ->
  Any   = [{var, Line, '_'}],
  Nil   = [{nil,Line}],
  False = [{atom,Line,false}],

  FalseResult = [{atom,Line,not Bool}],
  TrueResult  = [{atom,Line,Bool}],

  { 'case', Line, Expr, [
    { clause, Line, False, [], FalseResult },
    { clause, Line, Nil, [], FalseResult },
    { clause, Line, Any, [], TrueResult }
  ] }.

% Specially handle the '!' operator as it has different semantics from Erlang not.
build_unary_op(Line, '!', Right) ->
  convert_to_boolean(Line, Right, false);

% Specially handle two '!!' operators for performance.
build_unary_op(Line, '!!', Right) ->
  convert_to_boolean(Line, Right, true);

% Handle all others unary operators by simply passing them to Erlang.
build_unary_op(Line, Op, Right) ->
  { op, Line, Op, Right }.

% Build and handle comparision operators.
build_comp_op(Line, '&&', Left, Right, V) ->
  Any   = [{var, Line, '_'}],
  Nil   = [{nil,Line}],
  False = [{atom,Line,false}],

  { { 'case', Line, Left, [
    { clause, Line, False, [], False },
    { clause, Line, Nil, [], Nil },
    { clause, Line, Any, [], [Right] }
  ] }, V };

% Build and handle comparision operators.
build_comp_op(Line, Op, Left, Right, V) ->
  { { op, Line, convert_comp_op(Op), Left, Right }, V }.

% Convert comparison operators to erlang format.
convert_comp_op('=!=') -> '=/=';
convert_comp_op('!=') ->  '/=';
convert_comp_op('<=') ->  '=<';
convert_comp_op(Else) ->  Else.