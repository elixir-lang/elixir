-module(elixir_translator).
-export([translate/2, translate_each/2, parse/3, umergec/2, umergev/2]).
-include("elixir.hrl").

parse(String, Line, #elixir_scope{filename=Filename} = S) ->
  Forms = forms(String, Line, Filename),
  { Translated, FS } = translate(Forms, S),
  Final = case FS#elixir_scope.namespace of
    [] -> Translated;
    _  -> Translated ++ [elixir_namespace:transform(0, compile, FS)]
  end,
  { Final, FS }.

forms(String, StartLine, Filename) ->
  case elixir_tokenizer:tokenize(String, StartLine) of
    {ok, Tokens} ->
      case elixir_parser:parse(Tokens) of
        {ok, Forms} -> Forms;
        {error, {Line, _, [Error, Token]}} -> elixir_errors:syntax_error(Line, Filename, Error, Token)
      end;
    {error, {Line, Error, Token}} -> elixir_errors:syntax_error(Line, Filename, Error, Token)
  end.

translate(Forms, S) ->
  lists:mapfoldl(fun translate_each/2, S, Forms).

%% Assignment operator

translate_each({'=', Line, [Left, Right]}, S) ->
  { TLeft, SL } = translate_assigns(fun translate_each/2, Left, S),
  { TRight, SR } = translate_each(Right, umergec(S, SL)),
  SM = umergev(SL, SR),
  SF = case TLeft of
    { var, _, Name } ->
      Current = SM#elixir_scope.assigned_vars,
      SM#elixir_scope{assigned_vars=dict:store(Name, {Right, TRight}, Current)};
    _ -> SM
  end,
  { { match, Line, TLeft, TRight }, SF };

%% Math Operators

translate_each({ Op, Line, [Left, Right] }, S) when Op == '+'; Op == '-'; Op == '*'; Op == '/' ->
  { TLeft, SL }  = translate_each(Left, S),
  { TRight, SR } = translate_each(Right, umergec(S, SL)),
  { { op, Line, Op, TLeft, TRight }, umergev(SL, SR) };

% Unary Math Operators

translate_each({ '+', Line, [Expr] }, S) when is_number(Expr) ->
  translate_each(Expr, S);

translate_each({ '-', Line, [Expr] }, S) when is_number(Expr) ->
  translate_each(-1 * Expr, S);

translate_each({ Op, Line, [Expr] }, S) when Op == '+'; Op == '-' ->
  { TExpr, NS } = translate_each(Expr, S),
  { { op, Line, Op, TExpr }, NS };

%% Short-circuit operators

translate_each({'||', Line, [Left, Right]}, S) ->
  { Var, NS } = elixir_tree_helpers:build_var_name(Line, S),
  { TLeft, SL } = translate_each(Left, NS),
  { TRight, SR } = translate_each(Right, umergec(NS, SL)),

  Match = {match, Line, Var, TLeft},
  True  = [{atom,Line,true}],
  False = [{atom,Line,false}],

  { { 'case', Line, elixir_tree_helpers:convert_to_boolean(Line, Match, true), [
    { clause, Line, False, [], [TRight] },
    { clause, Line, True, [], [Var] }
  ] }, umergev(SL, SR) };

translate_each({'&&', Line, [Left, Right]}, S) ->
  { TLeft, SL } = translate_each(Left, S),
  { TRight, SR } = translate_each(Right, umergec(S, SL)),

  Any   = [{var, Line,'_'}],
  Nil   = [{atom,Line,nil}],
  False = [{atom,Line,false}],

  { { 'case', Line, TLeft, [
    { clause, Line, False, [], False },
    { clause, Line, Nil, [], Nil },
    { clause, Line, Any, [], [TRight] }
  ] }, umergev(SL, SR) };

%% If

translate_each({'if', Line, [Condition, [{do,_}|_] = Keywords]}, S) ->
  [{do,Exprs}|ElsesKeywords] = Keywords,

  WithCondition = case is_list(Exprs) of
    true  -> [Condition|Exprs];
    false -> [Condition,Exprs]
  end,

  IfKeywords = {do, WithCondition},

  case ElsesKeywords of
    [{else,_} = ElseKeywords | ElsifsKeywords] -> [];
    ElsifsKeywords -> ElseKeywords = {else,nil}
  end,

  { Clauses, FS } = elixir_clauses:translate(Line, fun translate/2, [IfKeywords|ElsifsKeywords] ++ [ElseKeywords], S),
  [Else|Others] = lists:reverse(Clauses),
  { build_if_clauses(Line, Others, Else), FS };

% TODO: Handle tree errors properly
translate_each({'if', _, _} = Clause, S) ->
  error({invalid_arguments_for_if, Clause});

%% Blocks

translate_each({ block, Line, Args }, S) when is_list(Args) ->
  { TArgs, NS } = translate(Args, S),
  { { block, Line, TArgs }, NS };

%% Containers

translate_each({'{}', Line, Args}, S) ->
  { TArgs, SE } = translate(Args, S),
  { {tuple, Line, TArgs}, SE };

%% References and namespaces

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

  % TODO: Handle the case were TLeft or TRight are not an atom
  Final = case {TLeft,TRight} of
    {{atom,Line,ALeft}, {atom,_,ARight}} ->
      Atom = list_to_atom(lists:concat([atom_to_list(ALeft), atom_to_list(ARight)])),
      { atom, Line, Atom }
  end,

  { Final, umergev(SL, SR) };

%% Def

translate_each({Kind, Line, [[X, Y]]}, S) when Kind == def; Kind == defmacro->
  Namespace = S#elixir_scope.namespace,
  case (Namespace == []) or (S#elixir_scope.method /= []) of
    true -> elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid scope for method");
    _ ->
      case X of
        {do,Exprs} -> {Name, Args} = Y;
        _ ->
          {do, Exprs}  = Y,
          {Name, Args} = X
      end,

      ClauseScope = S#elixir_scope{method=Name, counter=0, vars=dict:new(), assigned_vars=dict:new()},
      { TClause, _ } = translate_clause(Line, Args, Exprs, [], ClauseScope),
      { Unpacked, Defaults } = elixir_def_method:unpack_default_clause(Name, TClause),
      Method = { function, Line, Name, length(Args), [Unpacked] },
      { elixir_def_method:wrap_method_definition(Kind, Line, S#elixir_scope.filename, Namespace, Method, Defaults), S }
  end;

% TODO: Handle tree errors properly
translate_each({Kind, Line, Args}, S) when Kind == def; Kind == defmacro ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid args for " ++ atom_to_list(Kind));

%% Quoting

translate_each({quote, Line, [Expr]}, S) ->
  elixir_quote:translate_each(Expr, S);

% TODO: Handle tree errors properly
translate_each({quote, _, _} = Clause, S) ->
  error({invalid_arguments_for_quote, Clause});

%% Functions

translate_each({function, Line, [Args, [{do,Exprs}]]}, S) when is_list(Args) ->
  { TClause, NS } = translate_clause(Line, Args, Exprs, [], S),
  { { 'fun', Line, {clauses, [TClause]} }, NS };

% TODO: Handle tree errors properly
translate_each({function, _, _} = Clause, S) ->
  error({invalid_arguments_for_function, Clause});

%% Variables & Methods

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
          { NewVar, NS } = case Else of
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
  { TArgs, NS } = translate(Args, S),
  { { call, Line, { atom, Line, Atom }, TArgs }, NS };

%% Erlang calls

translate_each({{'.', _, [{ ref, _, ['Erlang']}, Atom]}, Line, Args}, S) when is_atom(Atom) ->
  { TArgs, NS } = translate(Args, S),
  { { call, Line, { atom, Line, Atom }, TArgs }, NS };

translate_each({{'.', _, [{{ '.', _, [{ref, _, ['Erlang']}, Remote]}, _, _}, Atom]}, Line, Args}, S) when is_atom(Atom) and is_atom(Remote) ->
  { TArgs, NS } = translate(Args, S),
  { ?ELIXIR_WRAP_CALL(Line, Remote, Atom, TArgs), NS };

%% Dot calls

% TODO Support when Left and TRight are not atoms
translate_each({{'.', _, [Left, Right]}, Line, Args}, S) when is_atom(Right) ->
  { TLeft,  SL } = translate_each(Left, S),
  { TRight,  SR } = translate_each(Right, umergec(S, SL)),

  Namespace = case TLeft of
    { atom, _, Option } ->
      try
        case lists:member({Right, length(Args)}, Option:'__macros__'()) of
          true  -> Option;
          false -> []
        end
      catch
        error:undef -> []
      end;
    _ -> []
  end,

  case Namespace of
    [] ->
      { TArgs,  SA } = translate(Args, umergec(S, SR)),
      { { call, Line, { remote, Line, TLeft, TRight }, TArgs }, umergev(SL, umergev(SR,SA)) };
    Namespace -> 
      Tree = apply(Namespace, Right, Args),
      translate_each(Tree, S)
  end;

%% Fun calls

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
  { { atom, 0, Atom }, S }.

%% Helpers

%% Listify

listify(Expr) when not is_list(Expr) -> [Expr];
listify(Expr) -> Expr.

%% Assigns helpers

translate_assigns(Fun, Args, Scope) ->
  { Result, NewScope } = Fun(Args, Scope#elixir_scope{assign=true}),
  { Result, NewScope#elixir_scope{assign=false, temp_vars=[] } }.

%% Clauses helpers for methods and functions

translate_clause(Line, Args, Exprs, Guards, S) ->
  { TArgs, SA }   = translate_assigns(fun translate/2, Args, S),
  { TGuards, SG } = translate(Guards, SA#elixir_scope{guard=true}),
  { TExprs, SE }  = translate_each(Exprs, SG#elixir_scope{guard=false}),
  { { clause, Line, TArgs, TGuards, listify(TExprs) }, SE }.

%% Build if clauses by nesting

build_if_clauses(Line, [], [Acc]) ->
  Acc;

build_if_clauses(Line, [[Condition|Exprs]|Others], Acc) ->
  True  = [{atom,Line,true}],
  False = [{atom,Line,false}],

  Case = { 'case', Line, elixir_tree_helpers:convert_to_boolean(Line, Condition, true), [
    { clause, Line, True,  [], Exprs },
    { clause, Line, False, [], Acc }
  ] },

  build_if_clauses(Line, Others, [Case]).

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