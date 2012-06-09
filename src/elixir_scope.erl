%% Convenience functions used to manipulate scope
%% and its variables.
-module(elixir_scope).
-export([translate_var/3,
  build_erl_var/2, build_ex_var/2,
  serialize/1, deserialize/1,
  to_erl_env/1, to_ex_env/1, filename/1,
  umergev/2, umergec/2
  ]).
-include("elixir.hrl").

translate_var(Line, Name, S) ->
  Match = S#elixir_scope.context == assign,
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
            true -> build_erl_var(Line, S);
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

build_erl_var(Line, #elixir_scope{counter=Counter} = S) ->
  NS = S#elixir_scope{counter=Counter+1},
  Var = { var, Line, ?ELIXIR_ATOM_CONCAT(["_@", Counter]) },
  { Var, NS }.

build_ex_var(Line, #elixir_scope{counter=Counter} = S) ->
  NS = S#elixir_scope{counter=Counter+1},
  Var = { ?ELIXIR_ATOM_CONCAT(["_@", Counter]), Line, nil },
  { Var, NS }.

% Handle Macro.Env conversion

to_erl_env(Scope) ->
  elixir_tree_helpers:abstract_syntax(to_ex_env(Scope)).

to_ex_env({ Line, Tuple }) when element(1, Tuple) == '__MAIN__.Macro.Env' ->
  setelement(4, Tuple, Line);

to_ex_env({ Line, #elixir_scope{module=Module,file=File,
    function=Function,aliases=Aliases,context=Context,
    requires=Requires,macros=Macros} }) ->
  { '__MAIN__.Macro.Env', Module, File, Line, Function, Aliases, Context, Requires, Macros }.

filename(#elixir_scope{file=File}) -> File;
filename(Other) -> element(3, Other).

% Provides a tuple with only the scope information we want to serialize.

serialize(S) ->
  elixir_tree_helpers:abstract_syntax(
    { S#elixir_scope.file, S#elixir_scope.functions, S#elixir_scope.check_clauses, S#elixir_scope.macro,
      S#elixir_scope.requires, S#elixir_scope.macros, S#elixir_scope.aliases, S#elixir_scope.scheduled }
  ).

% Fill in the scope with the variables serialization set in serialize_scope.

deserialize({ File, Functions, CheckClauses, Macro, Requires, Macros, Aliases, Scheduled }) ->
  #elixir_scope{
    file=File,
    functions=Functions,
    check_clauses=CheckClauses,
    macro=Macro,
    requires=Requires,
    macros=Macros,
    aliases=Aliases,
    scheduled=Scheduled
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
% with the counter values from the second one.

umergec(S1, S2) ->
  S1#elixir_scope{
    counter=S2#elixir_scope.counter,
    extra_guards=S2#elixir_scope.extra_guards,
    super=S1#elixir_scope.super orelse S2#elixir_scope.super,
    caller=S1#elixir_scope.caller orelse S2#elixir_scope.caller,
    name_args=S1#elixir_scope.name_args orelse S2#elixir_scope.name_args
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

var_number([_,_|T]) -> T.