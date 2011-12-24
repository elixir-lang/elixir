-module(elixir_translator).
-export([translate/2, translate_each/2, parse/3]).
-include("elixir.hrl").

parse(String, Line, #elixir_scope{filename=Filename} = S) ->
  Forms = forms(String, Line, Filename),
  { Translated, FS } = translate(Forms, S),
  Final = case FS#elixir_scope.namespace of
    [] -> Translated;
    _  -> Translated ++ [elixir_namespace:transform(0, compile, FS)]
  end,
  { Final, FS }.

% TODO test error inside interpolation are properly handled.
forms(String, StartLine, Filename) ->
  case elixir_tokenizer:tokenize(String, StartLine) of
    {ok, Tokens} ->
      try elixir_parser:parse(Tokens) of
        {ok, Forms} -> Forms;
        {error, {Line, _, [Error, Token]}} -> elixir_errors:syntax_error(Line, Filename, Error, Token)
      catch
        { interpolation_error, { Line, Error, Token } } -> elixir_errors:syntax_error(Line, Filename, Error, Token)
      end;
    {error, {Line, Error, Token}} -> elixir_errors:syntax_error(Line, Filename, Error, Token)
  end.

translate(Forms, S) ->
  lists:mapfoldl(fun translate_each/2, S, Forms).

%% Assignment operator

translate_each({'=', Line, [Left, Right]}, S) ->
  { TRight, SR } = translate_each(Right, S),
  { TLeft, SL } = elixir_clauses:assigns(fun translate_each/2, Left, SR),
  { { match, Line, TLeft, TRight }, SL };

%% Operators

translate_each({ '+', Line, [Expr] }, S) when is_number(Expr) ->
  translate_each(Expr, S);

translate_each({ '-', Line, [Expr] }, S) when is_number(Expr) ->
  translate_each(-1 * Expr, S);

translate_each({ Op, Line, Exprs }, S) when is_list(Exprs),
  Op == '+'; Op == '-'; Op == '*'; Op == '/'; Op == '<-';
  Op == '++'; Op == '--'; Op == 'andalso'; Op == 'orelse';
  Op == 'not'; Op == 'and'; Op == 'or'; Op == 'xor';
  Op == '<'; Op == '>'; Op == '<='; Op == '>=';
  Op == '=='; Op == '!='; Op == '==='; Op == '!==' ->
  translate_each({ erlang_op, Line, [Op|Exprs] }, S);

%% Operators

translate_each({ erlang_op, Line, [Op, Left, Right] }, S) when is_atom(Op) ->
  { TLeft, SL }  = translate_each(Left, S),
  { TRight, SR } = translate_each(Right, umergec(S, SL)),
  { { op, Line, convert_op(Op), TLeft, TRight }, umergev(SL, SR) };

translate_each({ erlang_op, Line, [Op, Expr] }, S) when is_atom(Op) ->
  { TExpr, NS } = translate_each(Expr, S),
  { { op, Line, convert_op(Op), TExpr }, NS };

%% Case

translate_each({'case', Line, [Expr, RawClauses]}, S) ->
  Clauses = orddict:erase(do, RawClauses),
  { TExpr, NS } = translate_each(Expr, S),
  { TClauses, TS } = elixir_clauses:match(Line, Clauses, NS),
  { { 'case', Line, TExpr, TClauses }, TS };

% TODO: Handle tree errors properly
translate_each({'case', _, Args} = Clause, S) when is_list(Args) ->
  error({invalid_arguments_for_case, Clause});

%% Blocks

translate_each({ block, Line, [] }, S) ->
  { { atom, Line, nil }, S };

translate_each({ block, Line, [Arg] }, S) ->
  translate_each(Arg, S);

translate_each({ block, Line, Args }, S) when is_list(Args) ->
  { TArgs, NS } = translate(Args, S),
  { { block, Line, TArgs }, NS };

% TODO: Add tests that kv_block not handled by macros raises
% a meaningful exception

%% Bit strings

translate_each({ bitstr, Line, Args }, S) when is_list(Args) ->
  elixir_tree_helpers:build_bitstr(fun translate_each/2, Args, Line, S);

%% Containers

translate_each({'{}', Line, Args}, S) when is_list(Args) ->
  { TArgs, SE } = translate(Args, S),
  { {tuple, Line, TArgs}, SE };

%% Namespaces

translate_each({ns, Line, [Ref]}, S) ->
  case S#elixir_scope.method of
    [] ->
      { TRef, NS } = translate_each(Ref, S),
      case TRef of
        { atom, _, Namespace } ->
          FS = NS#elixir_scope{namespace = Namespace},
          Expr = case S#elixir_scope.namespace of
            [] -> elixir_namespace:transform(Line, build, FS);
            _  ->
              { block, Line, [
                elixir_namespace:transform(Line, compile, S),
                elixir_namespace:transform(Line, build, FS)
              ] }
          end,
          { Expr, FS };
        _ ->
          elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid namespace name")
      end;
    _ ->
      elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid scope for namespace")
  end;

translate_each({'__NAMESPACE__', Line, []}, S) ->
  case S#elixir_scope.namespace of
    [] -> elixir_errors:syntax_error(Line, S#elixir_scope.filename, "no namespace defined");
    Namespace  -> {{atom, Line, Namespace }, S}
  end;

translate_each({endns, Line, []}, S) ->
  case S#elixir_scope.namespace of
    [] -> elixir_errors:syntax_error(Line, S#elixir_scope.filename, "no namespace defined");
    _  -> { elixir_namespace:transform(Line, compile, S), S#elixir_scope{namespace=[]} }
  end;

%% References

translate_each({ref, Line, [Ref]}, S) when is_atom(Ref) ->
  String = atom_to_list(Ref),
  Atom = case String of
    "::" ++ _ -> Ref;
    _ -> list_to_atom("::" ++ String)
  end,
  { {atom, Line, Atom }, S };

translate_each({'::', Line, [Left, Right]}, S) ->
  { TLeft, SL } = translate_each(Left, S),
  { TRight, SR } = translate_each(Right, umergec(S, SL)),

  % TODO: Handle the case were TLeft or TRight
  % are not an atom at compile time.
  Final = case {TLeft,TRight} of
    {{atom,Line,ALeft}, {atom,_,ARight}} ->
      Atom = list_to_atom(lists:concat([atom_to_list(ALeft), atom_to_list(ARight)])),
      { atom, Line, Atom }
  end,

  { Final, umergev(SL, SR) };

%% Def

translate_each({Kind, Line, [{Name,_,false},Else]}, S) when Kind == def orelse Kind == defmacro ->
  translate_each({Kind, Line, [{Name,Line,[]},Else]}, S);

translate_each({Kind, Line, [Call,[{do, Expr}]]}, S) when Kind == def orelse Kind == defmacro ->
  Namespace = S#elixir_scope.namespace,
  case (Namespace == []) or (S#elixir_scope.method /= []) of
    true -> elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid scope for method");
    _ ->
      { { Name, _, Args}, Guards } = elixir_clauses:extract_guards(Call),
      ClauseScope = S#elixir_scope{method=Name, counter=0, vars=dict:new()},
      { TClause, _ } = elixir_clauses:assigns_blocks(fun translate/2, Args, [Expr], Guards, ClauseScope),

      Arity = length(element(3, TClause)),
      { Unpacked, Defaults } = elixir_def_method:unpack_default_clause(Name, TClause),
      Method = { function, Line, Name, Arity, [Unpacked] },
      { elixir_def_method:wrap_method_definition(Kind, Line, S#elixir_scope.filename, Namespace, Method, Defaults), S }
  end;

% TODO: Handle tree errors properly
translate_each({Kind, Line, Args}, S) when is_list(Args), Kind == def orelse Kind == defmacro ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid args for " ++ atom_to_list(Kind));

%% Quoting

translate_each({quote, Line, [Expr]}, S) ->
  elixir_quote:translate_each(Expr, S);

% TODO: Handle tree errors properly
translate_each({quote, _, Args} = Clause, S) when is_list(Args) ->
  error({invalid_arguments_for_quote, Clause});

%% Functions

% TODO: Handle tree errors properly
translate_each({fn, Line, RawArgs}, S) when is_list(RawArgs) ->
  { Args, [[{do,Expr}]] } = lists:split(length(RawArgs) - 1, RawArgs),
  { TClause, NS } = elixir_clauses:assigns_blocks(fun translate/2, Args, [Expr], S),
  { { 'fun', Line, {clauses, [TClause]} }, NS };

%% Try

translate_each({'try', Line, [Clauses]}, RawS) ->
  Do    = proplists:get_value('do',    Clauses, []),
  Catch = proplists:get_value('catch', Clauses, []),
  After = proplists:get_value('after', Clauses, []),

  S = RawS#elixir_scope{noname=true},

  { TDo, SB }    = translate([Do], S),
  { TCatch, SC } = elixir_clauses:try_catch(Line, [{'catch',Catch}], umergec(S, SB)),
  { TAfter, SA } = translate([After], umergec(S, SC)),
  { { 'try', Line, unpack_try(do, TDo), [], TCatch, unpack_try('after', TAfter) }, umergec(RawS, SA) };

% TODO: Handle tree errors properly
translate_each({'try', _, Args} = Clause, S) when is_list(Args) ->
  error({invalid_arguments_for_try, Clause});

%% Receive

translate_each({'receive', Line, [RawClauses] }, S) ->
  Clauses = orddict:erase(do, RawClauses),
  case orddict:find('after', Clauses) of
    { ok, After } ->
      AClauses = orddict:erase('after', Clauses),
      { TClauses, SC } = elixir_clauses:match(Line, AClauses ++ [{'after',After}], S),
      { FClauses, [TAfter] } = lists:split(length(TClauses) - 1, TClauses),
      { _, _, [FExpr], _, FAfter } = TAfter,
      { { 'receive', Line, FClauses, FExpr, FAfter }, SC };
    error ->
      { TClauses, SC } = elixir_clauses:match(Line, Clauses, S),
      { { 'receive', Line, TClauses }, SC }
  end;

%% Variables & Function calls

translate_each({Name, Line, false}, S) when is_atom(Name) ->
  Match = S#elixir_scope.assign,
  Vars = S#elixir_scope.vars,
  TempVars = S#elixir_scope.temp_vars,
  ClauseVars = S#elixir_scope.clause_vars,

  case Name of
    '_' -> { {var, Line, Name}, S };
    _ ->
      case { Match, dict:is_key(Name, Vars), lists:member(Name, TempVars) } of
        { true, true, true } -> { {var, Line, dict:fetch(Name, Vars) }, S };
        { true, Else, _ } ->
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
        { false, false, _ } -> translate_each({Name, Line, []}, S);
        { false, true, _ }  -> { {var, Line, dict:fetch(Name, Vars) }, S }
      end
  end;

translate_each({Atom, Line, Args}, S) when is_atom(Atom) ->
  Callback = fun() ->
    { TArgs, NS } = translate(Args, S),
    { { call, Line, { atom, Line, Atom }, TArgs }, NS }
  end,
  elixir_macro:dispatch_one('::Elixir::Macros', Atom, Args, S, Callback);

%% Erlang calls

translate_each({{'.', _, [{ ref, _, ['Erlang']}, Atom]}, Line, Args}, S) when is_atom(Atom) ->
  { TArgs, NS } = translate(Args, S),
  { { call, Line, { atom, Line, Atom }, TArgs }, NS };

translate_each({{'.', _, [{{ '.', _, [{ref, _, ['Erlang']}, Remote]}, _, _}, Atom]}, Line, Args}, S) when is_atom(Atom) and is_atom(Remote) ->
  { TArgs, NS } = translate(Args, S),
  { ?ELIXIR_WRAP_CALL(Line, Remote, Atom, TArgs), NS };

%% Dot calls

translate_each({{'.', _, [Left, Right]}, Line, Args}, S) ->
  { TLeft,  SL } = translate_each(Left, S),
  { TRight,  SR } = translate_each(Right, umergec(S, SL)),

  Callback = fun() ->
    { TArgs, SA } = translate(Args, umergec(S, SR)),
    { { call, Line, { remote, Line, TLeft, TRight }, TArgs }, umergev(SL, umergev(SR,SA)) }
  end,

  case { TLeft, TRight } of
    { { atom, _, Receiver }, { atom, _, Name } }  ->
      elixir_macro:dispatch_one(Receiver, Name, Args, umergev(SL, SR), Callback);
    { { var, _, _ }, { var, _, _ } }  ->
      Callback();
    _ ->
      { TArgs, SA } = translate(Args, umergec(S, SR)),
      Apply = [TLeft, TRight, elixir_tree_helpers:build_simple_list(1, TArgs)],
      { ?ELIXIR_WRAP_CALL(Line, erlang, apply, Apply), umergev(SL, umergev(SR,SA)) }
  end;

%% Anonymous function calls

translate_each({{'.', _, [Expr]}, Line, Args}, S) ->
  { TExpr, SE } = translate_each(Expr, S),
  { TArgs, SA } = translate(Args, umergec(S, SE)),
  { {call, Line, TExpr, TArgs}, umergev(SE, SA) };

%% Literals

translate_each({ Left, Right }, S) ->
  { TLeft, SL }  = translate_each(Left, S),
  { TRight, SR } = translate_each(Right, SL),
  { { tuple, 0, [TLeft, TRight] }, SR };

translate_each([], S) ->
  { { nil, 0 }, S };

translate_each(Args, S) when is_list(Args) ->
  [RTail|RArgs] = lists:reverse(Args),

  case RTail of
    {'|',_,[Left,Right]} ->
      Exprs = [Left|RArgs],
      { Tail, ST } = translate_each(Right, S);
    _ ->
      Exprs = [RTail|RArgs],
      Tail = { nil, 0 },
      ST = S
  end,

  { TExprs, SE } = elixir_tree_helpers:build_reverse_list(fun translate_each/2, Exprs, 0, umergec(S, ST), Tail),
  { TExprs, umergev(ST, SE) };

translate_each(Number, S) when is_integer(Number) ->
  { { integer, 0, Number }, S };

translate_each(Number, S) when is_float(Number) ->
  { { float, 0, Number }, S };

translate_each(Atom, S) when is_atom(Atom) ->
  { { atom, 0, Atom }, S };

translate_each(Bitstring, S) when is_bitstring(Bitstring) ->
  { elixir_tree_helpers:abstract_syntax(Bitstring), S }.

%% Helpers

% Unpack a list of expressions from a block.
% Return an empty list in case it is an empty expression on after.
unpack_try(_, [{ block, _, Exprs }]) -> Exprs;
unpack_try('after', [{ nil, _ }])    -> [];
unpack_try(_, Exprs)                 -> Exprs.

% Receives two scopes and return a new scope based on the second
% with their variables merged.
umergev(S1, S2) ->
  V1 = S1#elixir_scope.vars,
  V2 = S2#elixir_scope.vars,
  C1 = S1#elixir_scope.clause_vars,
  C2 = S2#elixir_scope.clause_vars,
  S2#elixir_scope{
    vars=dict:merge(fun var_merger/3, V1, V2),
    clause_vars=dict:merge(fun var_merger/3, C1, C2)
  }.

% Receives two scopes and return a new scope based on the first
% with the counter values from the first one.
umergec(S1, S2) ->
  S1#elixir_scope{counter=S2#elixir_scope.counter}.

% Merge variables trying to find the most recently created.
var_merger(Var, Var, K2) -> K2;
var_merger(Var, K1, Var) -> K1;
var_merger(Var, K1, K2) ->
  V1 = list_to_integer(tl(atom_to_list(K1))),
  V2 = list_to_integer(tl(atom_to_list(K2))),
  if V1 > V2 -> K1;
     true -> K2
  end.

convert_op('!==') -> '=/=';
convert_op('===') -> '=:=';
convert_op('!=')  ->  '/=';
convert_op('<=')  ->  '=<';
convert_op('<-')  ->  '!';
convert_op(Else)  ->  Else.