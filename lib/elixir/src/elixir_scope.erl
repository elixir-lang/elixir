%% Convenience functions used to manipulate scope and its variables.
-module(elixir_scope).
-export([translate_var/4, build_var/2, context_info/1,
  load_binding/2, dump_binding/2,
  mergev/2, mergec/2, mergef/2,
  merge_vars/2, merge_opt_vars/2,
  format_error/1
]).
-include("elixir.hrl").

%% VAR HANDLING

translate_var(Meta, Name, Kind, S) when is_atom(Kind); is_integer(Kind) ->
  Line  = ?line(Meta),
  Tuple = {Name, Kind},
  Vars  = S#elixir_scope.vars,

  case orddict:find({Name, Kind}, Vars) of
    {ok, {Current, _}} -> Exists = true;
    error -> Current = nil, Exists = false
  end,

  case S#elixir_scope.context of
    match ->
      MatchVars = S#elixir_scope.match_vars,

      case Exists andalso ordsets:is_element(Tuple, MatchVars) of
        true ->
          warn_underscored_var(Line, S#elixir_scope.file, Name, Kind),
          {{var, Line, Current}, S};
        false ->
          %% We attempt to give vars a nice name because we
          %% still use the unused vars warnings from erl_lint.
          %%
          %% Once we move the warning to Elixir compiler, we
          %% can name vars as _@COUNTER.
          {NewVar, Counter, NS} =
            if
              Kind /= nil ->
                build_var('_', S);
              true ->
                build_var(Name, S)
            end,

          FS = NS#elixir_scope{
            vars=orddict:store(Tuple, {NewVar, Counter}, Vars),
            match_vars=ordsets:add_element(Tuple, MatchVars),
            export_vars=case S#elixir_scope.export_vars of
              nil -> nil;
              EV  -> orddict:store(Tuple, {NewVar, Counter}, EV)
            end
          },

          {{var, Line, NewVar}, FS}
      end;
    _  when Exists ->
      {{var, Line, Current}, S}
  end.

build_var(Key, S) ->
  New = orddict:update_counter(Key, 1, S#elixir_scope.counter),
  Cnt = orddict:fetch(Key, New),
  {elixir_utils:atom_concat([Key, "@", Cnt]), Cnt, S#elixir_scope{counter=New}}.

context_info(Kind) when Kind == nil; is_integer(Kind) -> "";
context_info(Kind) -> io_lib:format(" (context ~ts)", [elixir_aliases:inspect(Kind)]).

warn_underscored_var(Line, File, Name, Kind) ->
  case atom_to_list(Name) of
    "_@" ++ _ ->
      ok; %% Automatically generated variables
    "_" ++ _ ->
      elixir_errors:form_warn([{line, Line}], File, ?MODULE, {unused_match, Name, Kind});
    _ ->
      ok
  end.

%% SCOPE MERGING

%% Receives two scopes and return a new scope based on
%% the second with their variables merged.

mergev(S1, S2) ->
  S2#elixir_scope{
    vars=merge_vars(S1#elixir_scope.vars, S2#elixir_scope.vars),
    export_vars=merge_opt_vars(S1#elixir_scope.export_vars, S2#elixir_scope.export_vars)
 }.

%% Receives two scopes and return the first scope with
%% counters and flags from the later.

mergec(S1, S2) ->
  S1#elixir_scope{
    counter=S2#elixir_scope.counter,
    super=S2#elixir_scope.super,
    caller=S2#elixir_scope.caller
 }.

%% Similar to mergec but does not merge the user vars counter.

mergef(S1, S2) ->
  S1#elixir_scope{
    super=S2#elixir_scope.super,
    caller=S2#elixir_scope.caller
 }.

%% Mergers.

merge_vars(V, V) -> V;
merge_vars(V1, V2) ->
  orddict:merge(fun var_merger/3, V1, V2).

merge_opt_vars(nil, _C2) -> nil;
merge_opt_vars(_C1, nil) -> nil;
merge_opt_vars(C, C)     -> C;
merge_opt_vars(C1, C2)   ->
  orddict:merge(fun var_merger/3, C1, C2).

var_merger(_Var, {_, V1} = K1, {_, V2}) when V1 > V2 -> K1;
var_merger(_Var, _K1, K2) -> K2.

%% BINDINGS

load_binding(Binding, Scope) ->
  {NewBinding, NewVars, NewCounter} = load_binding(Binding, [], [], 0),
  {NewBinding, Scope#elixir_scope{
    vars=NewVars,
    counter=[{'_', NewCounter}]
 }}.

load_binding([{Key, Value}|T], Binding, Vars, Counter) ->
  Actual = case Key of
    {_Name, _Kind} -> Key;
    Name when is_atom(Name) -> {Name, nil}
  end,
  InternalName = elixir_utils:atom_concat(["_@", Counter]),
  load_binding(T,
    orddict:store(InternalName, Value, Binding),
    orddict:store(Actual, {InternalName, 0}, Vars), Counter + 1);
load_binding([], Binding, Vars, Counter) ->
  {Binding, Vars, Counter}.

dump_binding(Binding, #elixir_scope{vars=Vars}) ->
  dump_binding(Vars, Binding, []).

dump_binding([{{Var, Kind} = Key, {InternalName, _}}|T], Binding, Acc) when is_atom(Kind) ->
  Actual = case Kind of
    nil -> Var;
    _   -> Key
  end,
  Value = proplists:get_value(InternalName, Binding, nil),
  dump_binding(T, Binding, orddict:store(Actual, Value, Acc));
dump_binding([_|T], Binding, Acc) ->
  dump_binding(T, Binding, Acc);
dump_binding([], _Binding, Acc) -> Acc.

%% Errors

format_error({unused_match, Name, Kind}) ->
  io_lib:format("the underscored variable \"~ts\"~ts appears more than once in a "
                "match. This means the pattern will only match if all \"~ts\" bind "
                "to the same value. If this is the intended behaviour, please "
                "remove the leading underscore from the variable name, otherwise "
                "give the variables different names", [Name, context_info(Kind), Name]).
