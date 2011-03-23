-module(elixir_transform).
-export([parse/3]).
-include("elixir.hrl").

parse(String, Line, #elixir_scope{filename=Filename} = S) ->
  Forms = forms(String, Line, Filename),
  transform_tree(Forms, S).

forms(String, StartLine, Filename) ->
  case elixir_lexer:string(String, StartLine) of
    {ok, Tokens, _} ->
      case elixir_parser:parse(Tokens) of
        {ok, Forms} -> Forms;
        {error, {Line, _, [Error, Token]}} -> elixir_errors:syntax_error(Line, Filename, Error, Token)
      end;
    {error, {Line, _, {Error, Token}}, _} -> elixir_errors:syntax_error(Line, Filename, Error, Token)
  end.

% Receives two scopes and return a new scope based on the second
% with their variables merged.
umergev(S1, S2) ->
  V1 = S1#elixir_scope.vars,
  V2 = S2#elixir_scope.vars,
  A1 = S1#elixir_scope.assigned_vars,
  A2 = S2#elixir_scope.assigned_vars,
  S2#elixir_scope{vars=lists:umerge(V1, V2), assigned_vars=dict:merge(fun(_,_,F) -> F end, A1, A2)}.

% Receives two scopes and return a new scope based on the first
% with the counter values from the first one.
umergec(S1, S2) ->
  S1#elixir_scope{counter=S2#elixir_scope.counter}.

% Transform the given tree Forms.
%
% V is a list of variables that were bound (used for arranging
% implicit self) and S is the scope. The scope is a tuple with
% two elements. The first is a boolean that says if we are in a
% scope where new variables can be defined (i.e. the left side
% of a match or function clauses) and the second the nested
% module name.
transform_tree(Forms, S) ->
  lists:mapfoldl(fun transform/2, S, Forms).

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
transform({identifier, Line, Name}, S) ->
  Match = S#elixir_scope.assign,
  Vars = S#elixir_scope.vars,
  case { Match, lists:member(Name, Vars) } of
    { _, true }      -> { {var, Line, Name}, S };
    { true, false }  -> { {var, Line, Name}, S#elixir_scope{vars=lists:sort([Name|Vars])} };
    { false, false } -> transform({local_call, Line, Name, []}, S)
  end;

% Handles anonymous method calls as _.foo(1), transforming it to a function
% like like -> (x) x.foo(1).
transform({method_call, Line, Name, Args, {identifier,L,'_'}}, S) ->
  Var = { var, L, x },
  { Call, CS } = transform({method_call, Line, Name, Args, Var}, S),
  { { 'fun', Line, {
    clauses, [{ clause, Line, [Var], [], [Call] }]
  } }, CS };

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
transform({method_call, Line, Name, Args, Expr}, S) ->
  { TExpr, SE } = transform(Expr, S),
  { TArgs, SA } = transform({list, Line, Args, {nil, Line}}, umergec(S, SE)),
  { build_method_call(Name, Line, TArgs, TExpr), umergev(SE,SA) };

% Handles a call to super.
%
% = Variables
%
% Variables can be defined on method invocation.
transform({local_call, Line, super, Args}, S) ->
  case S#elixir_scope.method of
    [] -> elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid scope for super", "");
    Method ->
      { TArgs, SA } = transform({list, Line, Args, {nil, Line}}, S),

      Module = case S#elixir_scope.scope of
        {object, Name} -> ?ELIXIR_ATOM_CONCAT([Name, "::Proto"]);
        {module, Name} -> Name
      end,

      { ?ELIXIR_WRAP_CALL(Line, elixir_dispatch, super, [
        {var, Line, self}, {atom, Line, Module}, {atom, Line, Method}, TArgs
      ]), SA }
  end;

% Makes a local call.
%
% If a local call is done inside a method, they don't go through the method
% dispatching path and always call functions in the same module. Othewise,
% they are always treated as a remote call.
%
% = Variables
%
% New variables can be defined inside the arguments list.
transform({local_call, Line, Name, Args}, S) ->
  case S#elixir_scope.method of
    [] -> transform({method_call, Line, Name, Args, {var, Line, self}}, S);
    _  ->
      { TArgs, SA } = transform_tree(Args, S),
      FArgs = handle_new_call(Name, Line, [{var, Line, self}|TArgs], true),
      { { call, Line, {atom, Line, Name}, FArgs }, SA }
  end;

% Reference to a constant (that should then be loaded).
%
% = Variables
%
% It has no affect on variables scope.
transform({constant, Line, Name}, S) ->
  { ?ELIXIR_WRAP_CALL(Line, elixir_constants, lookup, [{atom, Line, Name}]), S };

% Reference to an instance variable (that should then be loaded).
%
% = Variables
%
% It has no affect on variables scope.
transform({ivar, Line, Name}, S) ->
  Args = [{var, Line, self}, {atom, Line, Name}],
  { ?ELIXIR_WRAP_CALL(Line, elixir_object_methods, get_ivar, Args), S };

% Syntax for easily updating instance variables.
%
% = Variables
%
% Variables can be defined inside parens as it was a method invocation.
transform({set_ivars, Line, Exprs}, S) ->
  { TExprs, SE } = transform_tree(Exprs, S),
  Args = [{var, Line, self}|TExprs],
  Call = case length(Args) of
    2 -> ?ELIXIR_WRAP_CALL(Line, elixir_object_methods, set_ivars, Args);
    _ -> ?ELIXIR_WRAP_CALL(Line, elixir_object_methods, set_ivar, Args)
  end,
  { Call, SE };

% Handle match declarations.
%
% = Variables
%
% Both the left and right side can contain variable declarations, as below:
%
%   a = (b = 1)
%
% So we need to take both into account.
transform({match, Line, Left, Right}, S) ->
  { TLeft, SL } = transform(Left, S#elixir_scope{assign=true}),
  { TRight, SR } = transform(Right, umergec(S, SL)),
  SM = umergev(SL, SR),
  SF = case TLeft of
    { var, _, Name } ->
      Current = SM#elixir_scope.assigned_vars,
      SM#elixir_scope{assigned_vars=dict:store(Name, {Right, TRight}, Current)};
    _ -> SM
  end,
  { {match, Line, TLeft, TRight }, SF };

% Handle tuple declarations.
%
% = Variables
%
% Each expression in the tuple can contain a match expression.
% Variables defined inside these expressions needs to be added to the var list.
transform({tuple, Line, Exprs }, S) ->
  { TExprs, SE } = transform_tree(Exprs, S),
  { {tuple, Line, TExprs}, SE };

% Handle list declarations.
%
% = Variables
%
% Each expression in the list can contain a match expression.
% Variables defined inside these expressions needs to be added to the var list.
transform({list, Line, Exprs, Tail }, S) ->
  { TTail, ST }  = transform(Tail, S),
  { TExprs, SE } = build_list(fun transform/2, Exprs, Line, umergec(S, ST), TTail),
  { TExprs, umergev(ST, SE) };

% Handle orddict declarations. It simply delegates to list to build a list
% of args that is dispatched to orddict:from_list/1. The final Dict
% object is created explicitly and not through Dict.new.
%
% = Variables
%
% See list.
transform({orddict, Line, Exprs }, S) ->
  { List, NS } = transform({list, Line, Exprs, {nil, Line} }, S),
  Dict = if
    S#elixir_scope.assign -> List;
    true -> ?ELIXIR_WRAP_CALL(Line, orddict, from_list, [List])
  end,
  { {tuple, Line, [{atom, Line, elixir_orddict__}, Dict] }, NS };

% Handle binaries declarations.
%
% = Variables
%
% Variables can not be defined inside each bin element.
transform({bin, Line, Exprs }, S) ->
  { TExprs, NS } = transform_tree(Exprs, S),
  { { bin, Line, TExprs }, NS };

transform({bin_element, Line, Expr, Type, Specifiers }, S) ->
  { TExpr, NS } = transform(Expr, S),
  { { bin_element, Line, TExpr, Type, Specifiers }, NS };

% Handle strings by wrapping them in the String object. A string is created
% by explicitly creating an #elixir_object__ and not through String.new.
%
% = Variables
%
% No variables can be defined in a string without interpolation.
transform({string, Line, String } = Expr, S) ->
  { { tuple, Line, [{atom, Line, elixir_string__}, build_bin(Line, [Expr])] }, S };

% Handle interpolated strings declarations. A string is created
% by explicitly creating an #elixir_object__ and not through String.new.
%
% = Variables
%
% Variables can be defined inside the interpolation.
transform({interpolated_string, Line, String }, S) ->
  { List, SE } = handle_interpolations(String, Line, S),
  Binary = ?ELIXIR_WRAP_CALL(Line, erlang, iolist_to_binary, [List]),
  { { tuple, Line, [{atom, Line, elixir_string__}, Binary] }, SE };

% Handle interpolated atoms by converting them to lists and calling atom_to_list.
%
% = Variables
%
% Variables can be defined inside the interpolation.
transform({interpolated_atom, Line, String}, S) ->
  { List, SE } = handle_interpolations(String, Line, S),
  Binary = ?ELIXIR_WRAP_CALL(Line, erlang, iolist_to_binary, [List]),
  { ?ELIXIR_WRAP_CALL(Line, erlang, binary_to_atom, [Binary, {atom, Line, utf8}]), SE };

% Handle regexps by dispatching a list and its options to Regexp.new.
%
% = Variables
%
% No variables can be defined in a string without interpolation.
transform({regexp, Line, String, Operators }, S) ->
  build_regexp(Line, {string, Line, String}, Operators, S);

% Handle interpolated regexps by dispatching a list and its options to Regexp.new.
%
% = Variables
%
% Variables can be defined inside the interpolation.
transform({interpolated_regexp, Line, String, Operators }, S) ->
  { List, SE } = handle_interpolations(String, Line, S),
  Binary = ?ELIXIR_WRAP_CALL(Line, erlang, iolist_to_binary, [List]),
  build_regexp(Line, Binary, Operators, SE);

% Handle char lists by simply converting them to Erlang strings.
%
% = Variables
%
% No variables can be defined in a char list without interpolation.
transform({char_list, Line, String } = Expr, S) ->
  { {string, Line, String}, S };

% Handle interpolated strings declarations. A string is created
% by explicitly creating an #elixir_object__ and not through String.new.
%
% = Variables
%
% Variables can be defined inside the interpolation.
transform({interpolated_char_list, Line, String }, S) ->
  { List, SE } = handle_interpolations(String, Line, S),
  Binary = ?ELIXIR_WRAP_CALL(Line, erlang, iolist_to_binary, [List]),
  Flattened = ?ELIXIR_WRAP_CALL(Line, erlang, binary_to_list, [Binary]),
  { Flattened, SE };

% Handle comparison operations.
%
% = Variables
%
% The Left and Right values of the comparison operation can be a match expression.
% Variables defined inside these expressions needs to be added to the list.
transform({comp_op, Line, Op, Left, Right}, S) ->
  { TLeft, SL } = transform(Left, S),
  { TRight, SR } = transform(Right, umergec(S, SL)),
  build_comp_op(Line, Op, TLeft, TRight, umergev(SL, SR));

% Handle binary operations.
%
% = Variables
%
% The Left and Right values of the binary operation can be a match expression.
% Variables defined inside these expressions needs to be added to the list.
transform({binary_op, Line, Op, { atom, _, _ } = Left, Right}, S) ->
  transform({tuple, Line, [Left, Right] }, S);

transform({binary_op, Line, Op, Left, Right}, S) ->
  { TLeft, SL } = transform(Left, S),
  { TRight, SR } = transform(Right, umergec(S, SL)),

  SF = umergev(SL, SR),

  case S#elixir_scope.assign orelse is_number_form(TLeft) orelse is_var_form(TLeft, S, fun({_,X}) -> is_number_form(X) end) of
    true -> { {op, Line, Op, TLeft, TRight}, SF };
    false ->
      Args = { cons, Line, TRight, {nil, Line} },
      case is_op_call_form(element(1, Left)) orelse is_var_form(TLeft, S, fun({X,_}) -> is_op_call_form(element(1, X)) end) of
        true -> { build_method_call(Op, Line, Args, TLeft), SF };
        false ->
          { Var, NS } = build_var_name(Line, SF),

          Match = [{match, Line, Var, TLeft}],
          True = [{atom,Line,true}],
          False = [{atom,Line,false}],
          IsNumber = {call,Line,{atom,Line,is_number},Match},

          { { 'case', Line, IsNumber, [
            { clause, Line, True, [], [{op, Line, Op, Var, TRight}] },
            { clause, Line, False, [], [build_method_call(Op, Line, Args, Var)] }
          ] }, NS }
      end
  end;

% Handle unary operations.
%
% = Variables
%
% The target (Right) of the unary operation can be a match expression.
% Variables defined inside these expressions needs to be added to the list.
transform({unary_op, Line, Op, Right}, S) ->
  { TRight, SE} = transform(Right, S),
  { build_unary_op(Line, Op, TRight), SE };

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
transform({'if', Line, [If|Elsifs], Else}, S) ->
  { TIf, IfS } = transform(If, {S,#elixir_scope{}}),
  { TElsifs, {ExprS, ListS} } = transform_tree(Elsifs, IfS),
  { TElse, ElseS } = transform_tree(Else, ExprS),
  { hd(lists:foldr(fun build_if_clauses/2, TElse, [TIf|TElsifs])), umergev(ListS, ElseS) };

% Handle case expressions.
%
% = Variables
%
% Variables are not shared between clauses but the overall variables
% list need to be passed forward. Also, variables counter need to be
% passed forward as well.
transform({'case', Line, Expr, Clauses}, S) ->
  { TExpr, NS } = transform(Expr, S),
  { TClauses, TS } = transform_clauses_tree(Clauses, NS),
  { { 'case', Line, TExpr, TClauses }, TS };

% Handle functions declarations. They preserve the current binding.
%
% = Variables
%
% Variables defined inside functions do not leak to the outer scope
% but variables previously defined affect the current function.
%
% Notice we use transform instead of transform_tree below because
% variables in different clauses do not mix.
transform({'fun', Line, {clauses, Clauses}}, S) ->
  TClauses = [element(1, transform(Clause, S)) || Clause <- Clauses],
  { { 'fun', Line, {clauses, TClauses} }, S };

% Handle begin/rescue/after blocks.
%
% = Variables
%
% Variables are never passed forward. The counter is always passed.
transform({'try', Line, Exprs}, S) ->
  { TExprs, SE } = transform_tree(Exprs, S),
  { { call, Line,
    { 'fun', Line,
      { clauses,
        [{ clause, Line, [], [], TExprs }]
      }
    },
  [] }, umergec(S, SE) };

transform({'try', Line, Body, Of, Clauses, After}, S) ->
  Transformer = fun(X, Acc) -> transform(X, umergec(S, Acc)) end,
  { TBody, SB } = transform_tree(Body, S),
  { TClauses, SC } = lists:mapfoldl(Transformer, SB, Clauses),
  { TAfter, SA } = transform_tree(After, umergec(S, SC)),
  { { 'try', Line, TBody, Of, TClauses, TAfter }, umergec(S, SA) };

% Handle receive expressions.
%
% = Variables
%
% Variables can be defined inside receive clauses as in case/match.
% Variables defined in after do not leak to the outer scope.
transform({'receive', Line, Clauses}, S) ->
  { TClauses, SC } = transform_clauses_tree(Clauses, S),
  { { 'receive', Line, TClauses }, umergec(S, SC) };

transform({'receive', Line, Clauses, Expr, After}, S) ->
  { TClauses, SC } = transform_clauses_tree(Clauses, S),
  { TExpr, _ } = transform(Expr, S),
  { TAfter, SA } = transform_tree(After, umergec(S, SC)),
  { { 'receive', Line, TClauses, TExpr, TAfter }, umergec(SC, SA) };

% Handle clauses. Those are the forms that handle clauses:
%
% * case -> Handle several clauses through transform_clauses_tree
% * receive/catch -> Handle several clauses through transform_clauses_tree
% * fun declarations -> Transform each clause manually
% * method declarations -> Transform each clause manually
%
% transform_clauses_tree transforms each clause by folding
% the variables counter and accumulating the list of variables.
%
% Both fun and method clauses allow the special operator := for default values.
%
% Notice that if expressions have an specialized kind of clause,
% called if_clause and else_clause.
%
% = Variables
%
% Variables declared in args do affect the exprs and should be taken
% into account. Clauses do not return variables list as second argument
% because variables in one clause should not affect the other.
transform({clause, Line, Args, Guards, Exprs}, S) ->
  { TArgs, SA } = transform_tree(Args, S#elixir_scope{assign=true}),
  { TExprs, SE } = transform_tree(Exprs, SA#elixir_scope{assign=false}),
  { { clause, Line, TArgs, Guards, TExprs }, SE };

transform({if_clause, Line, Bool, Expr, List}, {ExprS, ListS}) ->
  { TExpr, TExprS } = transform(Expr, ExprS),
  { TList, TListS } = transform_tree(List, TExprS),
  { {if_clause, Line, Bool, TExpr, TList }, { umergec(TExprS, TListS), umergev(ListS, TListS) } };

transform({else_clause, Line, Exprs}, S) ->
  transform({clause, Line, [{var, Line, '_'}], [], Exprs }, S);

% Handles erlang function calls in the following format:
%
%   Erlang.lists.mapfoldr()
%
% = Variables
%
% Variables can be set inside the args hash, so they need
% to be taken into account on the variables list.
transform({erlang_call, Line, Prefix, Suffix, Args}, S) ->
  { TArgs, SA } = transform_tree(Args, S),
  { ?ELIXIR_WRAP_CALL(Line, Prefix, Suffix, TArgs), SA };

% Method definitions are never executed by Elixir runtime. Their
% abstract form is stored into an ETS table and is just added to
% an Erlang module when they are compiled.
%
% = Variables
%
% Variables are handled in each function clause.
%
transform({def_method, Line, Name, Arity, [Clause]}, S) ->
  {_, Module} = S#elixir_scope.scope,
  case Module of
    [] -> elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid scope for method", "");
    _ ->
      NewScope = S#elixir_scope{method=Name},
      { TClause, _ } = pack_method_clause(Clause, NewScope),
      { Unpacked, Defaults } = elixir_def_method:unpack_default_clause(Name, TClause),
      Method = {function, Line, Name, Arity + 1, [Unpacked]},
      { elixir_def_method:wrap_method_definition(Module, Line, S#elixir_scope.filename, Method, Defaults), S }
  end;

transform({default_arg, Line, Expr, Default}, S) ->
  { TExpr, TS } = transform(Expr, S),
  { TDefault, _ } = transform(Default, S),
  { { default_arg, Line, TExpr, TDefault }, TS };

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
transform({fun_call, Line, Var, Args }, S) ->
  case Var of
    { identifier, _, Name } -> Method = not lists:member(Name, S#elixir_scope.vars);
    Name -> Method = false
  end,

  case Method of
    true -> transform({local_call, Line, Name, Args}, S);
    false ->
      { TArgs, SA } = transform_tree(Args, S),
      { TVar, SV }  = transform(Var, umergec(S, SA)),
      { {call, Line, TVar, TArgs}, umergev(SA, SV) }
  end;

% Handle list comprehensions.
%
% = Variables
%
% Variables defined inside the comprehensions do not leak.
transform({lc, Line, Expr, Cases} = Form, S) ->
  transform_comprehension(Form, S);

% Handle binary comprehensions.
%
% = Variables
%
% Variables defined inside the comprehensions do not leak.
transform({bc, Line, Elements, Cases}, S) ->
  Bin = { bin, Line, Elements },
  transform_comprehension({bc, Line, Bin, Cases}, S);

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
transform({Kind, Line, Name, Parent, Exprs}, S) when Kind == object; Kind == module->
  {_, Current} = S#elixir_scope.scope,
  Filename = S#elixir_scope.filename,
  NewName = elixir_object:scope_for(Current, Name),
  { TExprs, _ } = transform_tree(Exprs, S#elixir_scope{method=[],scope={Kind, NewName}}),
  { elixir_object:transform(Kind, Line, Filename, NewName, Parent, TExprs), S };

% Handles __FILE__
%
% = Variables
%
% No variables can be defined.
transform({filename, Line}, S) ->
  transform({string, Line, S#elixir_scope.filename}, S);

% Match all other expressions.
transform(Expr, S) -> { Expr, S }.

% Transform clauses tree
transform_clauses_tree(Clauses, S) ->
  Transformer = fun(X, Acc) ->
    % Pass variables counter forward, but always get the variable list from given S
    { TX, TAcc } = transform(X, umergec(S, Acc)),
    { TX, umergev(Acc, TAcc) }
  end,
  lists:mapfoldl(Transformer, S, Clauses).

% Handle transformations, generators and filters transformations.
transform_comprehension({Kind, Line, Expr, Cases}, S) ->
  Transformer = fun (X, Acc) -> transform_comprehension(X, Line, Acc) end,
  { TCases, SC } = lists:mapfoldl(Transformer, S, Cases),
  { TExpr, SE } = transform(Expr, SC),
  { { Kind, Line, TExpr, TCases }, umergec(S, SE) }.

transform_comprehension({undef_generate, Line, Left, Right}, L, S) ->
  Final = case Left of
    {bin, _Line, _Exprs} -> transform_comprehension({bin_generate, Line, Left, Right}, L, S);
    _ -> transform_comprehension({list_generate, Line, Left, Right}, L, S)
  end;

transform_comprehension({list_generate, Line, Left, Right}, L, S) ->
  { TLeft, SL } = transform(Left, S#elixir_scope{assign=true}),
  { TRight, SR } = transform(Right, SL#elixir_scope{assign=false}),
  { { generate, Line, TLeft, TRight }, SR };

transform_comprehension({bin_generate, Line, Left, Right}, L, S) ->
  { TLeft, SL } = transform(Left, S#elixir_scope{assign=true}),
  { TRight, SR } = transform(Right, SL#elixir_scope{assign=false}),
  { { b_generate, Line, TLeft, TRight }, SR };

transform_comprehension(X, L, S) ->
  { TX, TS } = transform(X, S),
  { convert_to_boolean(L, TX, true), TS }.

% Pack method clause in a format that receives Elixir metadata
% as first argument (like self) and annotates __current__ with
% the current module name (for super).
%
% = Variables
%
% It does not accummulate variables because variables in one
% clause do not affect the other. Each clause starts with an
% empty variable set as there is no binding.
pack_method_clause({clause, Line, Args, Guards, Exprs}, S) ->
  Clause = {clause, Line, [{var, Line, self}|Args], Guards, Exprs},
  transform(Clause, S#elixir_scope{vars=[self],counter=0,assigned_vars=dict:new()}).

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
build_list(Fun, Exprs, Line, S) ->
  build_list(Fun, Exprs, Line, S, {nil, Line}).

build_list(Fun, Exprs, Line, S, Tail) ->
  build_list_each(Fun, lists:reverse(Exprs), Line, S, Tail).

build_list_each(Fun, [], Line, S, Acc) ->
  { Acc, S };

build_list_each(Fun, [H|T], Line, S, Acc) ->
  { Expr, NS } = Fun(H, S),
  build_list_each(Fun, T, Line, NS, { cons, Line, Expr, Acc }).

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
  FArgs = handle_new_call(Name, Line, Args, false),
  ?ELIXIR_WRAP_CALL(Line, elixir_dispatch, dispatch, [Expr, {atom, Line, Name}, FArgs]).

% Builds a regexp.
build_regexp(Line, Expr, Operators, S) ->
  Args = [Expr, {string, Line, Operators}],
  { TArgs, _ } = transform({list, Line, Args, {nil, Line}}, S),
  { Constant, _ } = transform({constant, Line, 'Regexp'}, S),
  { build_method_call(new, Line, TArgs, Constant), S }.

% Handle method dispatches to new by wrapping everything in an array
% as we don't have a splat operator. If true is given as last option,
% the items should be wrapped in a list, by creating a list. If false,
% it is already an list so we just need a cons-cell.
handle_new_call(new, Line, Args, true) ->
  { List, [] } = build_list(fun(X,Y) -> {X,Y} end, Args, Line, []),
  [List];

handle_new_call(new, Line, Args, false) ->
  {cons, Line, Args, {nil, Line}};

handle_new_call(_, _, Args, _) ->
  Args.

% Handle interpolation. The final result will be a parse tree that
% returns a flattened list.
handle_interpolations(String, Line, S) ->
  Interpolations = String,

  % Optimized cases interpolations actually has no interpolation.
  case Interpolations of
    [{s, String}] -> handle_string_extractions(hd(Interpolations), Line, S);
    _ ->
      Transformer = fun(X, Acc) -> handle_string_extractions(X, Line, S) end,
      build_list(Transformer, Interpolations, Line, S)
  end.

% Handle string extractions for interpolated strings.
handle_string_extractions({s, String}, Line, S) ->
  { { string, Line, String }, S };

handle_string_extractions({i, Interpolation}, Line, S) ->
  { Tree, NS } = parse(Interpolation, Line, S),
  Stringify = build_method_call(to_s, Line, {nil,Line}, hd(Tree)),
  { ?ELIXIR_WRAP_CALL(Line, erlang, element, [{integer, Line, 2}, Stringify]), NS }.

% Convert the given expression to a boolean value: true or false.
% Assumes the given expressions was already transformed.
convert_to_boolean(Line, Expr, Bool) ->
  Any   = [{var, Line, '_'}],
  False = [{atom,Line,false}],
  Nil   = [{atom,Line,nil}],

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

build_var_name(Line, #elixir_scope{counter=Counter} = S) ->
  NS = S#elixir_scope{counter=Counter+1},
  Var = { var, Line, ?ELIXIR_ATOM_CONCAT(["X", Counter]) },
  { Var, NS }.

% Build and handle comparision operators.
build_comp_op(Line, '||', Left, Right, S) ->
  { Var, NS } = build_var_name(Line, S),

  Match = {match, Line, Var, Left},
  True = [{atom,Line,true}],
  False = [{atom,Line,false}],

  { { 'case', Line, convert_to_boolean(Line, Match, true), [
    { clause, Line, False, [], [Right] },
    { clause, Line, True, [], [Var] }
  ] }, NS };

% Build and handle comparision operators.
build_comp_op(Line, '&&', Left, Right, S) ->
  Any   = [{var, Line, '_'}],
  Nil   = [{atom,Line,nil}],
  False = [{atom,Line,false}],

  { { 'case', Line, Left, [
    { clause, Line, False, [], False },
    { clause, Line, Nil, [], Nil },
    { clause, Line, Any, [], [Right] }
  ] }, S };

% Build and handle comparision operators.
build_comp_op(Line, Op, Left, Right, S) ->
  { { op, Line, convert_comp_op(Op), Left, Right }, S }.

% Convert comparison operators to erlang format.
convert_comp_op('=!=') -> '=/=';
convert_comp_op('!=') ->  '/=';
convert_comp_op('<=') ->  '=<';
convert_comp_op(Else) ->  Else.

is_number_form({integer, _, _}) -> true;
is_number_form({float, _, _}) -> true;
is_number_form({op, _, Op, _ }) when Op == '+'; Op == '-' -> true;
is_number_form({op, _, Op, Left, _ }) when Op == '+'; Op == '-'; Op == '*'; Op == '/'; Op == 'div'; Op == 'rem' -> is_number_form(Left);
is_number_form(_) -> false.

is_op_call_form(string) -> true;
is_op_call_form(regexp) -> true;
is_op_call_form(char_list) -> true;
is_op_call_form(list) -> true;
is_op_call_form(tuple) -> true;
is_op_call_form(atom) -> true;
is_op_call_form(function) -> true;
is_op_call_form(orddict) -> true;
is_op_call_form(bin) -> true;
is_op_call_form(interpolated_string) -> true;
is_op_call_form(interpolated_regexp) -> true;
is_op_call_form(interpolated_atom) -> true;
is_op_call_form(interpolated_char_list) -> true;
is_op_call_form(_) -> false.

is_var_form({ var, _, Name }, #elixir_scope{assigned_vars=Dict}, Function) ->
  case dict:find(Name, Dict) of
    { ok, Var } -> Function(Var);
    error -> false
  end;

is_var_form(_, _, _) -> false.