% Handle main transformations + variable tracking.
% Code inlining is in elixir_inliner, general helpers in elixir_tree_helpers.
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
  C1 = S1#elixir_scope.clause_vars,
  C2 = S2#elixir_scope.clause_vars,
  A1 = S1#elixir_scope.assigned_vars,
  A2 = S2#elixir_scope.assigned_vars,
  S2#elixir_scope{
    vars=dict:merge(fun var_merger/3, V1, V2),
    clause_vars=dict:merge(fun var_merger/3, C1, C2),
    assigned_vars=dict:merge(fun unique_var_merge/3, A1, A2)
  }.

% Receives two scopes and return a new scope based on the first
% with the counter values from the first one.
umergec(S1, S2) ->
  S1#elixir_scope{counter=S2#elixir_scope.counter}.

% Merge variables and keep them only if they are equal.
unique_var_merge(_, V, V) -> V;
unique_var_merge(_, _, _) -> [].

% Merge variables trying to find the most recently created.
var_merger(Var, Var, K2) -> K2;
var_merger(Var, K1, Var) -> K1;
var_merger(Var, K1, K2) ->
  V1 = list_to_integer(tl(atom_to_list(K1))),
  V2 = list_to_integer(tl(atom_to_list(K2))),
  if V1 > V2 -> K1;
     true -> K2
  end.

% Transform considering assigns manipulation.
transform_assigns(Fun, Args, Scope) ->
  Merger = fun(_,_,F) -> F end,
  { Result, NewScope } = Fun(Args, Scope#elixir_scope{assign=true}),
  { Result, NewScope#elixir_scope{assign=false, temp_vars=[] } }.

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
  TempVars = S#elixir_scope.temp_vars,
  ClauseVars = S#elixir_scope.clause_vars,

  case Name of
    'self' -> { {var, Line, Name}, S };
    '_' -> { {var, Line, Name}, S };
    _ ->
      case { Match, dict:is_key(Name, Vars), lists:member(Name, TempVars) } of
        { true, true, true } -> { {var, Line, dict:fetch(Name, Vars) }, S };
        { true, Else, _ } ->
          % If it was already assigned or in a noname scope, build a new var
          { NewVar, NS } = case Else or S#elixir_scope.noname of
            true -> elixir_tree_helpers:build_var_name(Line, S);
            false -> { {var, Line, Name}, S }
          end,
          RealName = element(3, NewVar),
          { NewVar, NS#elixir_scope{
            vars=dict:store(Name, RealName, Vars),
            temp_vars=[RealName|TempVars],
            clause_vars=dict:store(Name, RealName, ClauseVars)
          } };
        { false, false, _ } -> transform({local_call, Line, Name, []}, S);
        { false, true, _ }  -> { {var, Line, dict:fetch(Name, Vars) }, S }
      end
  end;

% Handle identifiers that are certainly bounded
transform({bound_identifier, Line, Name}, S) ->
  case S#elixir_scope.assign of
    false ->
      elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid scope to bound variable", atom_to_list(Name));
    true ->
      case dict:find(Name, S#elixir_scope.vars) of
        { ok, Value } -> { {var, Line, Value}, S };
        error -> error({unbound_var, Name})
      end
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
  { TArgs, SA } = transform_tree(Args, umergec(S, SE)),
  Else = elixir_tree_helpers:build_method_call(Name, Line, TArgs, TExpr),
  { elixir_inliner:method_call(Line, Expr, Name, TArgs, Else), umergev(SE,SA) };

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
      FArgs = elixir_tree_helpers:handle_new_call(Name, Line, [{var, Line, self}|TArgs]),
      { { call, Line, {atom, Line, Name}, FArgs }, SA }
  end;

% Handles binding calls.
%
% = Variables
%
% Can be defined on both sides and args.
transform({bind_call, Line, [], Right, Args}, S) ->
  { TRight, SR } = transform(Right, S),
  { TArgs, SA } = transform_tree(Args, umergec(S, SR)),
  { ?ELIXIR_WRAP_CALL(elixir_bind, slate_bind, [TRight, TArgs]), umergev(SR,SA) };

transform({bind_call, Line, Left, Right, Args}, S) ->
  { TLeft,  SL } = transform(Left, S),
  { TRight, SR } = transform(Right, umergec(S, SL)),
  { TArgs,  SA } = transform_tree(Args, umergec(S, SR)),
  { ?ELIXIR_WRAP_CALL(elixir_bind, slate_bind, [TLeft, TRight, TArgs]), umergev(SL, umergev(SR,SA)) };

% Reference to a constant (that should then be loaded).
%
% = Variables
%
% It has no affect on variables scope.
transform({constant, Line, Name}, S) ->
  % Try to localize constants at compile time.
  % This may not be a good idea because we may point to old references
  % if we don't recompile the whole code, all the time.
  Final = try
    Snapshot = elixir_constants:lookup(Name),
    elixir_tree_helpers:abstract_syntax(Snapshot)
  catch
    error:{noconstant,Name} ->
      ?ELIXIR_WRAP_CALL(Line, elixir_constants, lookup, [{atom, Line, Name}])
  end,
  { Final, S };

% Reference to an instance variable (that should then be loaded).
%
% = Variables
%
% It has no affect on variables scope.
transform({ivar, Line, Name}, S) ->
  Args = [{var, Line, self}, {atom, Line, Name}],
  Else = ?ELIXIR_WRAP_CALL(Line, elixir_object_methods, get_ivar, Args),
  { elixir_inliner:get_ivar(Line, Name, Else), S };

% Syntax for easily updating instance variables.
%
% = Variables
%
% Variables can be defined inside parens as it was a method invocation.
transform({set_ivars, Line, Exprs}, S) ->
  { TExprs, SE } = transform_tree(Exprs, S),
  Args = [{var, Line, self}|TExprs],
  Call = case length(TExprs) of
    1 ->
      Else = ?ELIXIR_WRAP_CALL(Line, elixir_object_methods, set_ivars, Args),
      elixir_inliner:set_ivars(Line, TExprs, Else, SE);
    2 ->
      Else = ?ELIXIR_WRAP_CALL(Line, elixir_object_methods, set_ivar, Args),
      elixir_inliner:set_ivar(Line, TExprs, Else, SE);
    _ ->
      % TODO raise an exception
      { ?ELIXIR_WRAP_CALL(Line, elixir_object_methods, set_ivar, Args), SE }
  end;

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
  { TLeft, SL } = transform_assigns(fun transform/2, Left, S),
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
  { TExprs, SE } = elixir_tree_helpers:build_list(fun transform/2, Exprs, Line, umergec(S, ST), TTail),
  { TExprs, umergev(ST, SE) };

% Handle orddict declarations. It simply delegates to list to build a list
% of args that is dispatched to orddict:from_list/1. The final Dict
% object is created explicitly and not through Dict.new.
%
% = Variables
%
% See list.
transform({orddict, Line, Exprs }, S) ->
  Value = is_atoms_dict(Exprs),
  
  % If the dict is made of atoms, order them at compile time
  % so we don't need to do that at runtime.
  OExprs = if
    Value -> ordered_dict_tree(Exprs);
    true  -> Exprs
  end,

  { List, NS } = transform({list, Line, OExprs, {nil, Line} }, S),

  Dict = if
    Value or S#elixir_scope.assign -> List;
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

% Handle strings by wrapping them in the String object.
%
% = Variables
%
% No variables can be defined in a string without interpolation.
transform({string, Line, String } = Expr, S) ->
  { elixir_tree_helpers:build_bin(Line, [Expr]), S };

% Handle interpolated strings declarations.
%
% = Variables
%
% Variables can be defined inside the interpolation.
transform({interpolated_string, Line, String }, S) ->
  { List, SE } = elixir_interpolation:transform(String, Line, S),
  Binary = ?ELIXIR_WRAP_CALL(Line, erlang, iolist_to_binary, [List]),
  { Binary, SE };

% Handle interpolated atoms by converting them to lists and calling atom_to_list.
%
% = Variables
%
% Variables can be defined inside the interpolation.
transform({interpolated_atom, Line, String}, S) ->
  { List, SE } = elixir_interpolation:transform(String, Line, S),
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
  { List, SE } = elixir_interpolation:transform(String, Line, S),
  Binary = ?ELIXIR_WRAP_CALL(Line, erlang, iolist_to_binary, [List]),
  build_regexp(Line, Binary, Operators, SE);

% Handle char lists by simply converting them to Erlang strings.
%
% = Variables
%
% No variables can be defined in a char list without interpolation.
transform({char_list, Line, String } = Expr, S) ->
  { {string, Line, String}, S };

% Handle interpolated strings declarations.
%
% = Variables
%
% Variables can be defined inside the interpolation.
transform({interpolated_char_list, Line, String }, S) ->
  { List, SE } = elixir_interpolation:transform(String, Line, S),
  Binary = ?ELIXIR_WRAP_CALL(Line, erlang, iolist_to_binary, [List]),
  Flattened = ?ELIXIR_WRAP_CALL(Line, erlang, binary_to_list, [Binary]),
  { Flattened, SE };

% Handle comparison operations.
%
% = Variables
%
% The Left and Right values of the comparison operation can be a match expression.
% Variables defined inside these expressions needs to be added to the list.
transform({comp_op, Line, '||', Left, Right}, S) ->
  { Var, NS } = elixir_tree_helpers:build_var_name(Line, S),
  { TLeft, SL } = transform(Left, NS),
  { TRight, SR } = transform(Right, umergec(NS, SL)),

  Match = {match, Line, Var, TLeft},
  True  = [{atom,Line,true}],
  False = [{atom,Line,false}],

  { { 'case', Line, elixir_tree_helpers:convert_to_boolean(Line, Match, true), [
    { clause, Line, False, [], [TRight] },
    { clause, Line, True, [], [Var] }
  ] }, umergev(SL, SR) };

transform({comp_op, Line, '&&', Left, Right}, S) ->
  { TLeft, SL } = transform(Left, S),
  { TRight, SR } = transform(Right, umergec(S, SL)),

  Any   = [{var, Line,'_'}],
  Nil   = [{atom,Line,nil}],
  False = [{atom,Line,false}],

  { { 'case', Line, TLeft, [
    { clause, Line, False, [], False },
    { clause, Line, Nil, [], Nil },
    { clause, Line, Any, [], [TRight] }
  ] }, umergev(SL, SR) };

transform({comp_op, Line, Op, Left, Right}, S) ->
  { TLeft, SL } = transform(Left, S),
  { TRight, SR } = transform(Right, umergec(S, SL)),
  { { op, Line, convert_comp_op(Op), TLeft, TRight }, umergev(SL, SR) };

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
  elixir_inliner:binary_op(Line, Left, Right, TLeft, TRight, Op, S, umergev(SL, SR));

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
transform({'if', Line, Exprs, Else}, S) ->
  { TExprs, SE } = transform_clauses_tree(Line, Exprs ++ [Else], S),
  { TIfs, [TElse] } = lists:split(length(TExprs) - 1, TExprs),
  { hd(lists:foldr(fun build_if_clauses/2, element(5, TElse), TIfs)), SE };

% Handle case expressions.
%
% = Variables
%
% Variables are not shared between clauses but the overall variables
% list need to be passed forward. Also, variables counter need to be
% passed forward as well.
transform({'case', Line, Expr, Clauses}, S) ->
  { TExpr, NS } = transform(Expr, S),
  { TClauses, TS } = transform_clauses_tree(Line, Clauses, NS),
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

% Begin blocks.
%
% = Variables
%
% Variables are passed forward.
transform({'begin', Line, Exprs}, S) ->
  { TExprs, SE } = transform_tree(Exprs, S),
  { { block, Line, TExprs }, SE };

% Handle try/catch/after blocks.
%
% = Variables
%
% Variables are never passed forward. The counter is always passed.
transform({'try', Line, Exprs}, S) ->
  { TExprs, SE } = transform_tree(Exprs, S#elixir_scope{noname=true}),
  { { call, Line,
    { 'fun', Line,
      { clauses,
        [{ clause, Line, [], [], TExprs }]
      }
    },
  [] }, umergec(S, SE) };

transform({'try', Line, Body, Of, Clauses, After}, RS) ->
  % Just pass the variable counter forward between each clause.
  S = RS#elixir_scope{noname=true},
  Transformer = fun(X, Acc) -> transform(X, umergec(S, Acc)) end,

  { TBody, SB } = transform_tree(Body, S),
  { TClauses, SC } = lists:mapfoldl(Transformer, umergec(S, SB), Clauses),
  { TAfter, SA } = transform_tree(After, umergec(S, SC)),
  { { 'try', Line, TBody, Of, TClauses, TAfter }, umergec(RS, SA) };

% Handle receive expressions.
%
% = Variables
%
% Variables can be defined inside receive clauses as in case/match.
% Variables defined in after do not leak to the outer scope.
transform({'receive', Line, Clauses}, S) ->
  { TClauses, SC } = transform_clauses_tree(Line, Clauses, S),
  { { 'receive', Line, TClauses }, umergec(S, SC) };

transform({'receive', Line, Clauses, After}, S) ->
  { TClauses, SC } = transform_clauses_tree(Line, Clauses ++ [After], S),
  { FClauses, [TAfter] } = lists:split(length(TClauses) - 1, TClauses),
  { _, _, FExpr, _, FAfter } = TAfter,
  { { 'receive', Line, FClauses, FExpr, FAfter }, SC };

% Handle clauses. Those are the forms that handle clauses:
%
% * case -> Handle several clauses through transform_clauses_tree
% * receive/catch -> Handle several clauses through transform_clauses_tree
% * if/elsif/else -> Handle several clauses through transform_clauses_tree
% * try/catch/after -> Transform each clause manually
% * fun declarations -> Transform each clause manually
% * method declarations -> Transform each clause manually
%
% transform_clauses_tree transforms each clause keeping a list
% of the variables changed and allowing them to leak to the outer scope.
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
  { TArgs, SA } = transform_assigns(fun transform_tree/2, Args, S),
  { TGuards, SG } = transform_tree(Guards, SA#elixir_scope{guard=true}),
  { TExprs, SE } = transform_tree(Exprs, SG#elixir_scope{guard=false}),
  FGuards = case TGuards of
    [] -> [];
    _  -> [TGuards]
  end,
  { { clause, Line, TArgs, FGuards, TExprs }, SE };

transform({if_clause, Line, Bool, Expr, List}, S) ->
  { TExpr, SE } = transform(Expr, S),
  { TList, SL } = transform_tree(List, SE),
  { {if_clause, Line, Bool, TExpr, TList }, SL };

transform({else_clause, Line, Exprs}, S) ->
  transform({clause, Line, [{var, Line, '_'}], [], Exprs }, S);

transform({after_clause, Line, Expr, Else, Clauses}, S) ->
  { TExpr, SE } = transform(Expr, S),
  { TClauses, SA } = transform_tree(Clauses, SE),
  { { after_clause, Line, TExpr, Else, TClauses }, SA };

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
  case (Module == []) or (S#elixir_scope.method /= []) of
    true -> elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid scope for method");
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

% Special cased function calls.
%
% = Variables
%
% Both the left and right side can contain variable declarations, as below:
%
%   (a = -> (x) x + 2).(b = 1)
%
% So we need to take both into account.
transform({fun_call, Line, Var, Args }, S) ->
  { TArgs, SA } = transform_tree(Args, S),
  { TVar, SV }  = transform(Var, umergec(S, SA)),
  { {call, Line, TVar, TArgs}, umergev(SA, SV) };

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
  case S#elixir_scope.method of
    [] -> 
      {_, Current} = S#elixir_scope.scope,
      NewName = elixir_object:scope_for(Current, Name),
      { TExprs, _ } = transform_tree(Exprs, S#elixir_scope{method=[],scope={Kind, NewName}}),
      { elixir_object:transform(Kind, Line, NewName, Parent, TExprs, S), S };
    _ ->
      elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid scope for " ++ atom_to_list(Kind))
  end;

% Handles __FILE__
%
% = Variables
%
% No variables can be defined.
transform({filename, Line}, S) ->
  transform({string, Line, S#elixir_scope.filename}, S);

% Match all other expressions.
transform(Expr, S) -> { Expr, S }.

% Transform a tree of clauses by keeping a dict with all variables
% defined inside each clause. Variables defined in a clause but not
% in the other have their default set to nil unless a default value
% in the parent scope exists.

% Special case if clause has just one element. There is no need to pass
% through all the drama below.
transform_clauses_tree(Line, [Clause], S) ->
  { TClause, TS } = transform(Clause, S),
  { [TClause], TS };

transform_clauses_tree(Line, Clauses, RawS) ->
  S = RawS#elixir_scope{clause_vars=dict:new()},

  % Transform tree just passing the variables counter forward
  % and storing variables defined inside each clause.
  Transformer = fun(X, {Acc, CV}) ->
    { TX, TAcc } = transform(X, Acc),
    { TX, { umergec(S, TAcc), [TAcc#elixir_scope.clause_vars|CV] } }
  end,

  { TClauses, { TS, RawCV } } = lists:mapfoldl(Transformer, {S, []}, Clauses),

  % Now get all the variables defined inside each clause
  CV = lists:reverse(RawCV),
  NewVars = lists:umerge([lists:sort(dict:fetch_keys(X)) || X <- CV]),

  case NewVars of
    [] -> { TClauses, TS };
    _  ->
      % Create a new scope that contains a list of all variables
      % defined inside all the clauses. It returns this new scope and
      % a list of tuples where the first element is the variable name,
      % the second one is the new pointer to the variable and the third
      % is the old pointer.
      { FinalVars, FS } = lists:mapfoldl(fun normalize_vars/2, TS, NewVars),

      % Defines a tuple that will be used as left side of the match operator
      LeftTuple = { tuple, Line, [{var, Line, NewValue} || {_, NewValue,_} <- FinalVars] },
      { StorageVar, SS } = elixir_tree_helpers:build_var_name(Line, FS),

      % Expand all clauses by adding a match operation at the end that assigns
      % variables missing in one clause to the others.
      Expander = fun(Clause, Counter) ->
        ClauseVars = lists:nth(Counter, CV),
        RightTuple = [normalize_clause_var(Var, OldValue, ClauseVars) || {Var, _, OldValue} <- FinalVars],

        AssignExpr = { match, Line, LeftTuple, { tuple, Line, RightTuple } },
        [Final|RawClauses] = lists:reverse(element(5, Clause)),

        % If the last sentence has a match clause, we need to assign its value
        % in the variable list. If not, we insert the variable list before the
        % final clause in order to keep it tail call optimized.
        FinalClause = case has_match_tuple(Final) of
          true -> 
            StorageExpr = { match, Line, StorageVar, Final },
            [StorageVar,AssignExpr,StorageExpr|RawClauses];
          false ->
            [Final,AssignExpr|RawClauses]
        end,

        { setelement(5, Clause, lists:reverse(FinalClause)), Counter + 1 }
      end,

      { FClauses, _ } = lists:mapfoldl(Expander, 1, TClauses),
      { FClauses, SS }
  end.

% Helpers to transform clauses tree

has_match_tuple({match, _, _, _}) ->
  true;

has_match_tuple(H) when is_tuple(H) ->
  has_match_tuple(tuple_to_list(H));

has_match_tuple(H) when is_list(H) ->
  lists:any(fun has_match_tuple/1, H);

has_match_tuple(H) -> false.

% For orddict compile time ordering
is_atoms_dict([]) -> true;
is_atoms_dict([{tuple,_,[{atom,_,_},_]}|T]) -> is_atoms_dict(T);
is_atoms_dict(_) -> false.

ordered_dict_tree(Dict) ->
  lists:sort(fun({tuple,_,[{_,_,A},_]},{tuple,_,[{_,_,B},_]}) -> A =< B end, Dict).

% If the var was defined in the clause, use it, otherwise use from main scope.
normalize_clause_var(Var, OldValue, ClauseVars) ->
  case dict:find(Var, ClauseVars) of
    { ok, ClauseValue } -> { var, 0, ClauseValue };
    error -> OldValue
  end.

% Normalize the given var checking its existence in the scope var dictionary.
normalize_vars(Var, #elixir_scope{vars=Dict} = S) ->
  { { _, _, NewValue }, NS } = elixir_tree_helpers:build_var_name(0, S),
  FS = NS#elixir_scope{vars=dict:store(Var, NewValue, Dict)},

  Expr = case dict:find(Var, Dict) of
    { ok, OldValue } -> { var, 0, OldValue };
    error -> { atom, 0, nil }
  end,

  { { Var, NewValue, Expr }, FS }.

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
  { TLeft, SL } = transform_assigns(fun transform/2, Left, S),
  { TRight, SR } = transform(Right, SL),
  { { generate, Line, TLeft, TRight }, SR };

transform_comprehension({bin_generate, Line, Left, Right}, L, S) ->
  { TLeft, SL } = transform_assigns(fun transform/2, Left, S),
  { TRight, SR } = transform(Right, SL),
  { { b_generate, Line, TLeft, TRight }, SR };

transform_comprehension(X, L, S) ->
  { TX, TS } = transform(X, S),
  { elixir_tree_helpers:convert_to_boolean(L, TX, true), TS }.

% Pack method clause in a format that receives Elixir metadata
% as first argument (like self).
%
% = Variables
%
% It does not accummulate variables because variables in one
% clause do not affect the other. Each clause starts with an
% empty variable set as there is no binding.
pack_method_clause({clause, Line, Args, Guards, Exprs}, S) ->
  Clause = {clause, Line, [{var, Line, self}|Args], Guards, Exprs},
  transform(Clause, S#elixir_scope{vars=dict:new(),counter=0,assigned_vars=dict:new()}).

% Build if/elsif/else clauses by nesting then one inside the other.
% Assumes expressions were already converted.
build_if_clauses({if_clause, Line, Bool, Expr, List}, Acc) ->
  True  = [{atom,Line,true}],
  False = [{atom,Line,false}],

  [{ 'case', Line, elixir_tree_helpers:convert_to_boolean(Line, Expr, Bool), [
    { clause, Line, True, [], List },
    { clause, Line, False, [], Acc }
  ] }].

% Builds a regexp.
build_regexp(Line, Expr, Operators, S) ->
  Args = [Expr, {string, Line, Operators}],
  { TArgs, _ } = transform_tree(Args, S),
  { Constant, _ } = transform({constant, Line, 'Regexp'}, S),
  { elixir_tree_helpers:build_method_call(new, Line, TArgs, Constant), S }.

% Specially handle the '!' operator as it has different semantics from Erlang not.
build_unary_op(Line, '!', Right) ->
  elixir_tree_helpers:convert_to_boolean(Line, Right, false);

% Specially handle two '!!' operators for performance.
build_unary_op(Line, '!!', Right) ->
  elixir_tree_helpers:convert_to_boolean(Line, Right, true);

% Handle all others unary operators by simply passing them to Erlang.
build_unary_op(Line, Op, Right) ->
  { op, Line, Op, Right }.

% Convert comparison operators to erlang format.
convert_comp_op('=!=') -> '=/=';
convert_comp_op('!=') ->  '/=';
convert_comp_op('<=') ->  '=<';
convert_comp_op(Else) ->  Else.