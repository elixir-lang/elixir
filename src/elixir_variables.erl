%% Convenience functions used to manipulate variables
%% and the variables scope.
-module(elixir_variables).
-export([translate_each/3,
  build_erl/2, build_ex/2,
  umergev/2, umergec/2,
  serialize_scope/1, deserialize_scope/1]).
-include("elixir.hrl").

translate_each(Line, Name, S) ->
  Match = S#elixir_scope.assign,
  Vars = S#elixir_scope.vars,
  TempVars = S#elixir_scope.temp_vars,
  ClauseVars = S#elixir_scope.clause_vars,

  case Name of
    '_' -> { {var, Line, Name}, S };
    _ ->
      case { Match, dict:is_key(Name, Vars), dict:is_key(Name, TempVars) } of
        { true, true, true } -> { {var, Line, dict:fetch(Name, Vars) }, S };
        { true, Else, _ } ->
          { NewVar, NS } = case Else or S#elixir_scope.noname of
            true -> build_erl(Line, S);
            false -> { {var, Line, Name}, S }
          end,
          RealName = element(3, NewVar),
          { NewVar, NS#elixir_scope{
            vars=dict:store(Name, RealName, Vars),
            temp_vars=dict:store(Name, RealName, TempVars),
            clause_vars=dict:store(Name, RealName, ClauseVars)
          } };
        { false, false, _ } -> elixir_translator:translate_each({Name, Line, []}, S);
        { false, true, _ }  -> { {var, Line, dict:fetch(Name, Vars) }, S }
      end
  end.

% Handle variables translation

build_erl(Line, #elixir_scope{counter=Counter} = S) ->
  NS = S#elixir_scope{counter=Counter+1},
  Var = { var, Line, ?ELIXIR_ATOM_CONCAT(["_EX", Counter]) },
  { Var, NS }.

build_ex(Line, #elixir_scope{counter=Counter} = S) ->
  NS = S#elixir_scope{counter=Counter+1},
  Var = { ?ELIXIR_ATOM_CONCAT(["_EX", Counter]), Line, nil },
  { Var, NS }.

% Provides a tuple with only the scope information we want to serialize.

serialize_scope(S) ->
  elixir_tree_helpers:abstract_syntax(
    { S#elixir_scope.filename, S#elixir_scope.functions, S#elixir_scope.check_clauses, S#elixir_scope.forwarded,
      S#elixir_scope.macros, S#elixir_scope.refer, S#elixir_scope.scheduled, S#elixir_scope.compile }
  ).

% Fill in the scope with the variables serialization set in serialize_scope.

deserialize_scope({ Filename, Functions, CheckClauses, Forwarded, Macros, Refer, Scheduled, Compile }) ->
  #elixir_scope{
    filename=Filename,
    functions=Functions,
    check_clauses=CheckClauses,
    macros=Macros,
    refer=Refer,
    scheduled=Scheduled,
    compile=Compile,
    forwarded=Forwarded
  }.

% Receives two scopes and return a new scope based on the second
% with their variables merged.

umergev(S1, S2) ->
  V1 = S1#elixir_scope.vars,
  V2 = S2#elixir_scope.vars,
  Q1 = S1#elixir_scope.quote_vars,
  Q2 = S2#elixir_scope.quote_vars,
  C1 = S1#elixir_scope.clause_vars,
  C2 = S2#elixir_scope.clause_vars,
  S2#elixir_scope{
    vars=dict:merge(fun var_merger/3, V1, V2),
    quote_vars=dict:merge(fun var_merger/3, Q1, Q2),
    clause_vars=dict:merge(fun var_merger/3, C1, C2)
  }.

% Receives two scopes and return a new scope based on the first
% with the counter values from the first one.

umergec(S1, S2) ->
  S1#elixir_scope{
    counter=S2#elixir_scope.counter,
    super=S1#elixir_scope.super or S2#elixir_scope.super
  }.

% Merge variables trying to find the most recently created.

var_merger(Var, Var, K2) -> K2;
var_merger(Var, K1, Var) -> K1;
var_merger(_Var, K1, K2) ->
  V1 = list_to_integer(var_number(atom_to_list(K1))),
  V2 = list_to_integer(var_number(atom_to_list(K2))),
  if V1 > V2 -> K1;
     true -> K2
  end.

var_number([_,_,_|T]) -> T.