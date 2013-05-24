%% Convenience functions used to manipulate scope
%% and its variables.
-module(elixir_scope).
-export([translate_var/5,
  build_erl_var/2, build_ex_var/2,
  serialize/1, deserialize/1,
  serialize_with_vars/2, deserialize_with_vars/2,
  to_erl_env/1, to_ex_env/1,
  umergev/2, umergec/2, merge_clause_vars/2
]).
-include("elixir.hrl").

translate_var(Meta, Name, Kind, S, Callback) ->
  Line = ?line(Meta),
  Vars = S#elixir_scope.vars,

  case Name of
    '_' ->
      { { var, Line, Name }, S };
    _ ->
      case S#elixir_scope.context of
        match ->
          TempVars = S#elixir_scope.temp_vars,
          case { orddict:is_key({ Name, Kind }, Vars), orddict:find(Name, TempVars) } of
            { true, { ok, Kind } } ->
              { { var, Line, orddict:fetch({ Name, Kind }, Vars) }, S };
            { Else, _ } ->
              { NewVar, NS } = if
                Kind /= nil -> build_erl_var(Line, S);
                Else -> build_erl_var(Line, Name, Name, S);
                S#elixir_scope.noname -> build_erl_var(Line, Name, Name, S);
                true -> { { var, Line, Name }, S }
              end,
              RealName = element(3, NewVar),
              ClauseVars = S#elixir_scope.clause_vars,
              { NewVar, NS#elixir_scope{
                vars=orddict:store({ Name, Kind }, RealName, Vars),
                temp_vars=orddict:store(Name, Kind, TempVars),
                clause_vars=if
                  ClauseVars == nil -> nil;
                  true -> orddict:store({ Name, Kind }, RealName, ClauseVars)
                end
              } }
          end;
        _ ->
          case orddict:find({ Name, Kind }, Vars) of
            { ok, VarName } -> { { var, Line, VarName }, S };
            error -> Callback()
          end
      end
  end.

% Handle variables translation

build_ex_var(Line, S)  -> build_ex_var(Line, '', "_", S).
build_erl_var(Line, S) -> build_erl_var(Line, '', "_", S).

build_var_counter(Key, #elixir_scope{counter=Counter} = S) ->
  New = orddict:update_counter(Key, 1, Counter),
  { orddict:fetch(Key, New), S#elixir_scope{counter=New} }.

build_erl_var(Line, Key, Name, S) when is_integer(Line) ->
  { Counter, NS } = build_var_counter(Key, S),
  Var = { var, Line, ?atom_concat([Name, "@", Counter]) },
  { Var, NS }.

build_ex_var(Line, Key, Name, S) when is_integer(Line) ->
  Context = case S#elixir_scope.module of
    nil -> 'Elixir';
    Mod -> Mod
  end,

  { Counter, NS } = build_var_counter(Key, S),
  Var = { ?atom_concat([Name, "@", Counter]), [{line,Line}], Context },
  { Var, NS }.

% Handle Macro.Env conversion

to_erl_env({ 'Elixir.Macro.Env', Module, File, _Line, Function, Aliases, Context,
    Requires, Functions, Macros, FileModules, FunctionKind }) ->
  #elixir_scope{module=Module,file=File,
    function=Function,aliases=Aliases,context=Context,
    requires=Requires,macros=Macros,functions=Functions,
    context_modules=FileModules,function_kind=FunctionKind}.

to_ex_env({ Line, #elixir_scope{module=Module,file=File,
    function=Function,aliases=Aliases,context=Context,
    requires=Requires,macros=Macros,functions=Functions,
    context_modules=FileModules,function_kind=FunctionKind} }) when is_integer(Line) ->
  { 'Elixir.Macro.Env', Module, File, Line, Function, Aliases,
    Context, Requires, Functions, Macros, FileModules, FunctionKind }.

% Provides a tuple with only the scope information we want to serialize.

serialize(S) ->
  elixir_tree_helpers:elixir_to_erl(
    { S#elixir_scope.file, S#elixir_scope.functions,
      S#elixir_scope.requires, S#elixir_scope.macros, S#elixir_scope.aliases,
      S#elixir_scope.macro_functions, S#elixir_scope.macro_macros, S#elixir_scope.macro_aliases,
      S#elixir_scope.context_modules }
  ).

serialize_with_vars(Line, S) when is_integer(Line) ->
  { Vars, _ } = orddict:fold(fun({ Key, Kind }, Value, { Acc, Counter }) ->
    { { cons, Line, { tuple, Line, [
      { atom, Line, Key },
      { atom, Line, Kind },
      { atom, Line, ?atom_concat(["_@", Counter]) },
      { var,  Line, Value }
    ] }, Acc }, Counter + 1 }
  end, { { nil, Line }, 0 }, S#elixir_scope.vars),
  { serialize(S), Vars }.

% Fill in the scope with the variables serialization set in serialize_scope.

deserialize(Tuple) -> deserialize_with_vars(Tuple, []).

deserialize_with_vars({ File, Functions, Requires, Macros,
                        Aliases, MacroFunctions, MacroMacros, MacroAliases, FileModules }, Vars) ->
  #elixir_scope{
    file=File,
    functions=Functions,
    requires=Requires,
    macros=Macros,
    aliases=Aliases,
    macro_functions=MacroFunctions,
    macro_macros=MacroMacros,
    macro_aliases=MacroAliases,
    context_modules=FileModules,
    vars=orddict:from_list(Vars),
    counter=[{'',length(Vars)}]
  }.

% Receives two scopes and return a new scope based on the second
% with their variables merged.

umergev(S1, S2) ->
  V1 = S1#elixir_scope.vars,
  V2 = S2#elixir_scope.vars,
  C1 = S1#elixir_scope.clause_vars,
  C2 = S2#elixir_scope.clause_vars,
  S2#elixir_scope{
    vars=merge_vars(V1, V2),
    clause_vars=merge_clause_vars(C1, C2)
  }.

% Receives two scopes and return the later scope
% keeping the variables from the first (counters,
% imports and everything else are passed forward).

umergec(S1, S2) ->
  S2#elixir_scope{
    vars=S1#elixir_scope.vars,
    temp_vars=S1#elixir_scope.temp_vars,
    clause_vars=S1#elixir_scope.clause_vars
  }.

% Merge variables trying to find the most recently created.

merge_vars(V, V) -> V;
merge_vars(V1, V2) ->
  orddict:merge(fun var_merger/3, V1, V2).

merge_clause_vars(nil, _C2) -> nil;
merge_clause_vars(_C1, nil) -> nil;
merge_clause_vars(C, C)     -> C;
merge_clause_vars(C1, C2)   ->
  orddict:merge(fun var_merger/3, C1, C2).

var_merger({ Var, _ }, Var, K2) -> K2;
var_merger({ Var, _ }, K1, Var) -> K1;
var_merger(_Var, K1, K2) ->
  V1 = var_number(atom_to_list(K1), []),
  V2 = var_number(atom_to_list(K2), []),
  case V1 > V2 of
    true  -> K1;
    false -> K2
  end.

var_number([$@|T], _Acc) -> var_number(T, []);
var_number([H|T], Acc)   -> var_number(T, [H|Acc]);
var_number([], Acc)      -> list_to_integer(lists:reverse(Acc)).
