-module(elixir_transform).
-export([parse/3]).
-include("elixir.hrl").
-import(lists, [umerge/2]).

parse(String, Binding, Filename) ->
  Vars = lists:usort(proplists:get_keys(Binding)),
  parse(String, 1, Filename, Vars, {false, false, []}).

parse(String, Line, Filename, V, S) ->
  Forms = forms(String, Line, Filename),
  transform_tree(Forms, Filename, V, S).

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
transform_tree(Forms, F, V, S) ->
  Transform = fun(X, Acc) -> transform(X, F, Acc, S) end,
  lists:mapfoldl(Transform, V, Forms).

% Handles identifiers, i.e. method calls or variable calls, allowing
% implicit self.
%
% = Variables
%
% If the scope has true for variables, it means new variables can be
% defined. In such cases, variables are added to the list if they don't
% exist yet. If we cannot define a variable and it does not belong to
% the list, make it a method call.
transform({identifier, Line, Name}, F, V, S) ->
  Var = element(1, S),
  case { Var, lists:member(Name, V) } of
    { _, true }      -> { {var, Line, Name}, V };
    { true, false }  -> { {var, Line, Name}, lists:sort([Name|V]) };
    { false, false } -> transform({local_call, Line, Name, []}, F, V, S)
  end;

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
% = new
%
% This method special cases new by wrapping all arguments into an array.
% This is required so Object.new() implementation can handle all arguments.
% This case could also be implemented in the dispatcher, but would affect
% performance.
transform({method_call, Line, Name, Args, Expr}, F, V, S) ->
  { TExpr, VE } = transform(Expr, F, V, S),
  { TArgs, VA } = transform({list, Line, Args, {nil, Line}}, F, V, S),
  { build_method_call(Name, Line, TArgs, TExpr), umerge(VA,VE) };

% Makes a local call. Local calls don't go through the method dispatching
% path and always call functions in the same module.
transform({local_call, Line, Name, Args}, F, V, S) ->
  case element(2, S) of
    true ->
      { TArgs, VA } = transform_tree(Args, F, V, S),
      FArgs = handle_new_call(Name, Line, [{var, Line, self}|TArgs]),
      { { call, Line, {atom, Line, Name}, FArgs }, VA };
    false -> transform({method_call, Line, Name, Args, {var, Line, self}}, F, V, S)
  end;

% Reference to a constant (that should then be loaded).
%
% = Variables
%
% It has no affect on variables scope.
transform({constant, Line, Name}, F, V, S) ->
  { ?ELIXIR_WRAP_CALL(Line, elixir_constants, lookup, [{atom, Line, Name}]), V };

% Reference to an instance variable (that should then be loaded).
%
% = Variables
%
% It has no affect on variables scope.
transform({ivar, Line, Name}, F, V, S) ->
  { ?ELIXIR_WRAP_CALL(Line, elixir_object_methods, get_ivar, [{var, Line, self}, {atom, Line, Name}]), V };

% Handle match declarations.
%
% = Variables
%
% Both the left and right side can contain variable declarations, as below:
%
%   a = (b = 1)
%
% So we need to take both into account.
transform({match, Line, Left, Right}, F, V, S) ->
  { Var, Def, Mod } = S,
  { TLeft, VL } = transform(Left, F, V, { true, Def, Mod }),
  { TRight, VR } = transform(Right, F, V, S),
  { {match, Line, TLeft, TRight }, umerge(VL, VR) };

% Handle tuple declarations.
%
% = Variables
%
% Each expression in the tuple can contain a match expression.
% Variables defined inside these expressions needs to be added to the var list.
transform({tuple, Line, Exprs }, F, V, S) ->
  { TExprs, VE } = transform_tree(Exprs, F, V, S),
  { {tuple, Line, TExprs}, VE };

% Handle list declarations.
%
% = Variables
%
% Each expression in the list can contain a match expression.
% Variables defined inside these expressions needs to be added to the var list.
transform({list, Line, Exprs, Tail }, F, V, S) ->
  Transformer = fun (X, Acc) -> transform(X, F, Acc, S) end,
  { TTail, VT }  = transform(Tail, F, V, S),
  { TExprs, VE } = build_list(Transformer, Exprs, Line, V, TTail),
  { TExprs, umerge(VE, VT) };

% Handle dict declarations. It simply delegates to list to build a list
% of args that is dispatched to dict:from_list/1. The final Dict
% object is created explicitly and not through Dict.new.
%
% = Variables
%
% See list.
transform({dict, Line, Exprs }, F, V, S) ->
  { List, NV } = transform({list, Line, Exprs, {nil, Line} }, F, V, S),
  Dict = ?ELIXIR_WRAP_CALL(Line, dict, from_list, [List]),
  { build_object(Line, 'Dict', [{dict, Dict}]), NV };

% Handle interpolated strings declarations. A string is created
% by explicitly creating an #elixir_object and not through String.new.
%
% = Variables
%
% Variables can be defined inside the interpolation.
transform({interpolated_string, Line, String }, F, V, S) ->
  { Flattened, VE } = handle_interpolations(String, Line, F, V, S),
  { build_object(Line, 'String', [{list, Flattened}]), VE };

% Handle strings by wrapping them in the String object. A string is created
% by explicitly creating an #elixir_object and not through String.new.
%
% = Variables
%
% No variables can be defined in a string without interpolation.
transform({string, Line, String } = Expr, F, V, S) ->
  { build_object(Line, 'String', [{list, Expr}]), V };

% Handle interpolated atoms by converting them to lists and calling atom_to_list.
%
% = Variables
%
% Variables can be defined inside the interpolation.
transform({interpolated_atom, Line, String}, F, V, S) ->
  { Flattened, VE } = handle_interpolations(String, Line, F, V, S),
  { ?ELIXIR_WRAP_CALL(Line, erlang, list_to_atom, [Flattened]), VE };

% Handle regexps by dispatching a list and its options to Regexp.new.
%
% = Variables
%
% No variables can be defined in a string without interpolation.
transform({regexp, Line, String, Operators }, F, V, S) ->
  build_regexp(Line, {string, Line, String}, Operators, F, V, S);

% Handle interpolated regexps by dispatching a list and its options to Regexp.new.
%
% = Variables
%
% Variables can be defined inside the interpolation.
transform({interpolated_regexp, Line, String, Operators }, F, V, S) ->
  { Flattened, VE } = handle_interpolations(String, Line, F, V, S),
  build_regexp(Line, Flattened, Operators, F, VE, S);

% Handle binary operations.
%
% = Variables
%
% The Left and Right values of the binary operation can be a match expression.
% Variables defined inside these expressions needs to be added to the list.
transform({binary_op, Line, Op, Left, Right}, F, V, S) ->
  { TLeft, VL } = transform(Left, F, V, S),
  { TRight, VR } = transform(Right, F, V, S),
  Args = { cons, Line, TRight, {nil, Line} },
  { build_method_call(Op, Line, Args, TLeft), umerge(VL, VR) };

% Handle unary operations.
%
% = Variables
%
% The target (Right) of the unary operation can be a match expression.
% Variables defined inside these expressions needs to be added to the list.
transform({unary_op, Line, Op, Right}, F, V, S) ->
  { TRight, V1} = transform(Right, F, V, S),
  { { op, Line, Op, TRight }, V1 };

% Handle functions declarations. They preserve the current binding.
%
% = Variables
%
% Variables defined inside functions do not leak to the outer scope
% but variables previously defined affect the current function.
transform({'fun', Line, {clauses, Clauses}}, F, V, S) ->
  TClauses = [transform(Clause, F, V, S) || Clause <- Clauses],
  { { 'fun', Line, {clauses, TClauses} }, V };

% Handle function clauses.
%
% = Variables
%
% Variables declared in args do affect the exprs and should be taken
% into account. Clauses do not return variables list as second argument
% because variables in one clause should not affect the other.
transform({clause, Line, Args, Guards, Exprs}, F, V, S) ->
  { Var, Def, Mod } = S,
  { TArgs, NV } = transform_tree(Args, F, V, { true, Def, Mod }),
  { TExprs, _ } = transform_tree(Exprs, F, NV, S),
  { clause, Line, TArgs, Guards, TExprs };

% Handles erlang function calls in the following format:
%
%   Erlang.lists.mapfoldr()
%
% = Variables
%
% Variables can be set inside the args hash, so they need
% to be taken into account on the variables list.
transform({erlang_call, Line, Prefix, Suffix, Args}, F, V, S) ->
  { TArgs, VA } = transform_tree(Args, F, V, S),
  case Prefix of
    [] -> { { call, Line, {atom, Line, Suffix}, TArgs }, VA };
    _  -> { ?ELIXIR_WRAP_CALL(Line, Prefix, Suffix, TArgs), VA }
  end;

% Method definitions are never executed by Elixir runtime. Their
% abstract form is stored into an ETS table and is just added to
% an Erlang module when they are compiled.
%
% = Variables
%
% Variables are handled in each function clause.
%
% TODO Test that a method declaration outside a module raises an error.
transform({def_method, Line, Name, Arity, Clauses}, F, V, S) ->
  {Var, _, Module} = S,
  Scope = {Var, true, Module},
  TClauses = [pack_method_clause(Clause, F, V, Scope) || Clause <- Clauses],
  Method = {function, Line, Name, Arity + 1, TClauses},
  { elixir_object:wrap_method_definition(Module, Line, F, Method), V };

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
transform({fun_call, Line, Var, Args }, F, V, S) ->
  case Var of
    { identifier, _, Name } -> Method = not lists:member(Name, V);
    Name -> Method = false
  end,

  case Method of
    true -> transform({local_call, Line, Name, Args}, F, V, S);
    false ->
      { TArgs, VA } = transform_tree(Args, F, V, S),
      { TVar, VV }  = transform(Var, F, V, S),
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
transform({object, Line, Name, Parent, Exprs}, F, V, S) ->
  {Var, _, Current} = S,
  NewName = elixir_object:scope_for(Current, Name),
  Scope = { Var, false, NewName },
  { TExprs, _ } = transform_tree(Exprs, F, [self], Scope),
  { elixir_object:transform(Line, F, NewName, Parent, TExprs), V };

% Match all other expressions.
% TODO Expand instead of catch all.
transform(Expr, F, V, S) -> { Expr, V }.

% Pack method clause in a format that receives Elixir metadata
% as first argument (like self) and annotates __current__ with
% the current module name (for super).
%
% = Variables
%
% It does not accummulate variables because variables in one
% clause do not affect the other. Each clause starts with an
% empty variable set as there is no binding.
pack_method_clause({clause, Line, Args, Guards, Exprs}, F, V, S) ->
  Clause = {clause, Line, [{var, Line, self}|Args], Guards, Exprs},
  transform(Clause, F, [self], S).

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

% Build an #elixir_object using tuples. It expects the parent
% and a proplist of Key/Value pairs to be used as instance variables.
build_object(Line, Parent, Ivars) ->
  Dict = fun ({Key, Value}, Acc) -> ?ELIXIR_WRAP_CALL(Line, dict, store, [{atom, Line, Key}, Value, Acc]) end,

  {tuple, Line,
    [
      {atom, Line, elixir_object},
      {nil, Line},          % Name
      {atom, Line, Parent}, % Parent
      {nil, Line},          % Mixins
      {nil, Line},          % Protos
      lists:foldl(Dict, ?ELIXIR_WRAP_CALL(Line, dict, new, []), Ivars)
    ]
  }.

% Handles method calls. It performs no transformation and assumes
% all data is already transformed.
build_method_call(Name, Line, Args, Expr) ->
  FArgs = handle_new_call(Name, Line, Args),
  ?ELIXIR_WRAP_CALL(Line, elixir_dispatch, dispatch, [{var, Line, self}, Expr, {atom, Line, Name}, FArgs]).

% Builds a regexp.
build_regexp(Line, Expr, Operators, F, V, S) ->
  Args = [Expr, {string, Line, Operators}],
  { TArgs, _ } = transform({list, Line, Args, {nil, Line}}, F, V, S),
  { Constant, _ } = transform({constant, Line, 'Regexp'}, F, V, S),
  { build_method_call(new, Line, TArgs, Constant), V }.

% Handle method dispatches to nil by wrapping everything in an array
% as we don't have a splat operator.
handle_new_call(new, Line, Args) ->
  {cons, Line, Args, {nil, Line}};

handle_new_call(_, _, Args) ->
  Args.

% Handle interpolation. The final result will be a parse tree that
% returns a flattened list.
handle_interpolations(String, Line, F, V, S) ->
  Interpolations = elixir_string_methods:extract_interpolations(String),

  % Optimized cases interpolations actually has no interpolation.
  case Interpolations of
    [{s, String}] -> handle_string_extractions(hd(Interpolations), Line, F, V, S);
    _ ->
      Transformer = fun(X, Acc) -> handle_string_extractions(X, Line, F, Acc, S) end,
      { List, VE } = build_list(Transformer, Interpolations, Line, V),
      { ?ELIXIR_WRAP_CALL(Line, lists, flatten, [List]), VE }
  end.

% Handle string extractions for interpolation strings.
handle_string_extractions({s, String}, Line, F, V, S) ->
  { { string, Line, String }, V };

handle_string_extractions({i, Interpolation}, Line, F, V, S) ->
  { Tree, NV } = parse(Interpolation, Line, F, V, S),
  Stringify = build_method_call(to_s, Line, {nil,Line}, hd(Tree)),
  { ?ELIXIR_WRAP_CALL(Line, elixir_object_methods, get_ivar, [Stringify, {atom, Line, list}]), NV }.
