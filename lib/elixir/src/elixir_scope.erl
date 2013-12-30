%% Convenience functions used to manipulate scope and its variables.
-module(elixir_scope).
-export([translate_var/4, build_var/2,
  load_binding/2, dump_binding/2,
  format_error/1, warn_unsafe/3,
  mergev/2, mergec/2, merge_vars/2, merge_opt_vars/2
]).
-include("elixir.hrl").

%% VAR HANDLING

translate_var(Meta, Name, Kind, RS) when is_atom(Kind); is_integer(Kind) ->
  Line  = ?line(Meta),
  Tuple = { Name, Kind },

  BS = if
    RS#elixir_scope.extra == do_match ->
      RS#elixir_scope{temp_vars=ordsets:add_element(Tuple, RS#elixir_scope.temp_vars)};
    (RS#elixir_scope.context == match) andalso (RS#elixir_scope.temp_vars /= nil) ->
      RS#elixir_scope{temp_vars=ordsets:del_element(Tuple, RS#elixir_scope.temp_vars)};
    true ->
      RS
  end,

  S = case BS#elixir_scope.context of
    match -> BS#elixir_scope{unsafe_vars=ordsets:del_element(Tuple, BS#elixir_scope.unsafe_vars)};
    _     -> BS
  end,

  warn_unsafe(Meta, Tuple, S),
  Vars = S#elixir_scope.vars,

  case orddict:find({ Name, Kind }, Vars) of
    { ok, { Current, _ } } -> Exists = true;
    error -> Current = nil, Exists = false
  end,

  case S#elixir_scope.context of
    match ->
      MatchVars = S#elixir_scope.match_vars,

      case Exists andalso ordsets:is_element(Tuple, MatchVars) of
        true ->
          { { var, Line, Current }, S };
        false ->
          { NewVar, Counter, NS } =
            if
              Kind /= nil -> build_var('_', S);
              true -> build_var(Name, S)
            end,

          FS = NS#elixir_scope{
            vars=orddict:store(Tuple, { NewVar, Counter }, Vars),
            match_vars=ordsets:add_element(Tuple, MatchVars),
            clause_vars=case S#elixir_scope.clause_vars of
              nil -> nil;
              CV  -> orddict:store(Tuple, { NewVar, Counter }, CV)
            end
          },

          { { var, Line, NewVar }, FS }
      end;
    _  when Exists ->
      { { var, Line, Current }, S }
  end.

build_var(Key, #elixir_scope{counter=Dict} = S) ->
  New = orddict:update(Key, fun(Old) -> Old + 1 end, 0, Dict),
  Cnt = orddict:fetch(Key, New),
  Var =
    case Cnt of
      0 when Key /= '_' -> Key;
      _ -> ?atom_concat([Key, "@", Cnt])
    end,
  { Var, Cnt, S#elixir_scope{counter=New} }.

warn_unsafe(Meta, Tuple, S) ->
  case ordsets:is_element(Tuple, S#elixir_scope.unsafe_vars) andalso
       (lists:keyfind(unsafe, 1, Meta) /= { unsafe, false }) of
    true  -> elixir_errors:handle_file_warning(S#elixir_scope.file, { ?line(Meta), ?MODULE, { unsafe_var, Tuple } });
    false -> ok
  end.

format_error({ unsafe_var, { Var, nil } }) ->
  io_lib:format("variable ~ts is defined in a case clause and is unsafe, please assign it explicitly", [Var]);
format_error({ unsafe_var, { Var, Ctx } }) ->
  io_lib:format("variable ~ts (context ~p) is defined in a case clause and is unsafe, please assign it explicitly", [Var, Ctx]).

%% SCOPE MERGING

%% Receives two scopes and return a new scope based on the second
%% with their variables merged.

mergev(S1, S2) ->
  V1 = S1#elixir_scope.vars,
  V2 = S2#elixir_scope.vars,
  C1 = S1#elixir_scope.clause_vars,
  C2 = S2#elixir_scope.clause_vars,
  T1 = S1#elixir_scope.temp_vars,
  T2 = S2#elixir_scope.temp_vars,
  U1 = S1#elixir_scope.unsafe_vars,
  U2 = S2#elixir_scope.unsafe_vars,
  S2#elixir_scope{
    vars=merge_vars(V1, V2),
    unsafe_vars=merge_vars(U1, U2),
    clause_vars=merge_opt_vars(C1, C2),
    temp_vars=merge_opt_vars(T1, T2)
  }.

%% Receives two scopes and return the first scope with
%% counters and flags from the later.

mergec(S1, S2) ->
  S1#elixir_scope{
    counter=S2#elixir_scope.counter,
    super=S2#elixir_scope.super,
    caller=S2#elixir_scope.caller
  }.

% Merge variables trying to find the most recently created.

merge_vars(V, V) -> V;
merge_vars(V1, V2) ->
  orddict:merge(fun var_merger/3, V1, V2).

merge_opt_vars(nil, _C2) -> nil;
merge_opt_vars(_C1, nil) -> nil;
merge_opt_vars(C, C)     -> C;
merge_opt_vars(C1, C2)   ->
  orddict:merge(fun var_merger/3, C1, C2).

var_merger(_Var, { _, V1 } = K1, { _, V2 }) when V1 > V2 -> K1;
var_merger(_Var, _K1, K2) -> K2.

%% BINDINGS

load_binding(Binding, Scope) ->
  { NewBinding, NewVars, NewCounter } = load_binding(Binding, [], [], 0),
  { NewBinding, Scope#elixir_scope{
    vars=NewVars,
    match_vars=[],
    clause_vars=nil,
    counter=[{'_',NewCounter}]
  } }.

load_binding([{Key,Value}|T], Binding, Vars, Counter) ->
  Actual = case Key of
    { _Name, _Kind } -> Key;
    Name when is_atom(Name) -> { Name, nil }
  end,
  InternalName = ?atom_concat(["_@", Counter]),
  load_binding(T,
    [{InternalName,Value}|Binding],
    orddict:store(Actual, { InternalName, 0 }, Vars), Counter + 1);
load_binding([], Binding, Vars, Counter) ->
  { lists:reverse(Binding), Vars, Counter }.

dump_binding(Binding, #elixir_scope{vars=Vars}) ->
  dump_binding(Vars, Binding, []).

dump_binding([{{ Var, Kind } = Key, { InternalName,_ }}|T], Binding, Acc) when is_atom(Kind) ->
  Actual = case Kind of
    nil -> Var;
    _   -> Key
  end,
  Value = proplists:get_value(InternalName, Binding, nil),
  dump_binding(T, Binding, orddict:store(Actual, Value, Acc));
dump_binding([_|T], Binding, Acc) ->
  dump_binding(T, Binding, Acc);
dump_binding([], _Binding, Acc) -> Acc.
