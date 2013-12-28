%% Convenience functions used to manipulate scope and its variables.
-module(elixir_scope).
-export([translate_var/5,
  build_erl_var/2, build_ex_var/2,
  load_binding/2, dump_binding/2,
  umergev/2, umergec/2, umergea/2, merge_clause_vars/2
]).
-include("elixir.hrl").

%% VAR HANDLING

translate_var(Meta, Name, Kind, S, Callback) when is_atom(Kind); is_integer(Kind) ->
  Line = ?line(Meta),
  Vars = S#elixir_scope.vars,
  Tuple = { Name, Kind },

  case Name of
    '_' ->
      { { var, Line, Name }, S };
    _ ->
      case S#elixir_scope.context of
        match ->
          TempVars = S#elixir_scope.temp_vars,
          case { orddict:is_key(Tuple, Vars), is_list(TempVars) andalso ordsets:is_element(Tuple, TempVars) } of
            { true, true } ->
              { { var, Line, orddict:fetch(Tuple, Vars) }, S };
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
                vars=orddict:store(Tuple, RealName, Vars),
                temp_vars=if
                  TempVars == nil -> TempVars;
                  true -> ordsets:add_element(Tuple, TempVars)
                end,
                clause_vars=if
                  ClauseVars == nil -> ClauseVars;
                  true -> orddict:store(Tuple, RealName, ClauseVars)
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

%% SCOPE MERGING

%% Receives two scopes and return a new scope based on the second
%% with their variables merged.

umergev(S1, S2) ->
  V1 = S1#elixir_scope.vars,
  V2 = S2#elixir_scope.vars,
  C1 = S1#elixir_scope.clause_vars,
  C2 = S2#elixir_scope.clause_vars,
  S2#elixir_scope{
    vars=merge_vars(V1, V2),
    clause_vars=merge_clause_vars(C1, C2)
  }.

%% Receives two scopes and return the first scope with
%% counters and flags from the later.

umergec(S1, S2) ->
  S1#elixir_scope{
    counter=S2#elixir_scope.counter,
    macro_counter=S2#elixir_scope.macro_counter,
    extra_guards=S2#elixir_scope.extra_guards,
    super=S2#elixir_scope.super,
    caller=S2#elixir_scope.caller
  }.

%% Receives two scopes and return the later scope
%% keeping the variables from the first (counters,
%% imports and everything else are passed forward).

umergea(S1, S2) ->
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

%% BINDINGS

load_binding(Binding, Scope) ->
  { NewBinding, NewVars, NewCounter } = load_binding(Binding, [], [], 0),
  { NewBinding, Scope#elixir_scope{
    vars=NewVars,
    temp_vars=[],
    clause_vars=nil,
    counter=[{'',NewCounter}]
  } }.

load_binding([{Key,Value}|T], Binding, Vars, Counter) ->
  Actual = case Key of
    { _Name, _Kind } -> Key;
    Name when is_atom(Name) -> { Name, nil }
  end,
  InternalName = ?atom_concat(["_@", Counter]),
  load_binding(T,
    [{InternalName,Value}|Binding],
    orddict:store(Actual, InternalName, Vars), Counter + 1);
load_binding([], Binding, Vars, Counter) ->
  { lists:reverse(Binding), Vars, Counter }.

dump_binding(Binding, #elixir_scope{vars=Vars}) ->
  dump_binding(Vars, Binding, []).

dump_binding([{{Var,Kind}=Key,InternalName}|T], Binding, Acc) when is_atom(Kind) ->
  Actual = case Kind of
    nil -> Var;
    _   -> Key
  end,
  Value = proplists:get_value(InternalName, Binding, nil),
  dump_binding(T, Binding, orddict:store(Actual, Value, Acc));
dump_binding([_|T], Binding, Acc) ->
  dump_binding(T, Binding, Acc);
dump_binding([], _Binding, Acc) -> Acc.
