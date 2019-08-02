%% Module responsible for tracking lexical information.
-module(elixir_lexical).
-export([run/2, with_file/3, trace/2, format_error/1]).
-include("elixir.hrl").

-define(tracker, 'Elixir.Kernel.LexicalTracker').

run(#{tracers := Tracers} = E, Callback) ->
  case elixir_config:get(bootstrap) of
    false ->
      {ok, Pid} = ?tracker:start_link(),

      try Callback(E#{lexical_tracker := Pid, tracers := [?MODULE | Tracers]}) of
        Res ->
          warn_unused_aliases(Pid, E),
          warn_unused_imports(Pid, E),
          Res
      after
        unlink(Pid),
        ?tracker:stop(Pid)
      end;

    true ->
      Callback(E)
  end.

trace({import, Meta, Module, Opts}, #{lexical_tracker := Pid}) ->
  {imported, Imported} = lists:keyfind(imported, 1, Meta),

  Only =
    case lists:keyfind(only, 1, Opts) of
      {only, List} when is_list(List) -> List;
      _ -> []
    end,

  ?tracker:add_import(Pid, Module, Only, ?line(Meta), Imported and should_warn(Meta, Opts)),
  ok;
trace({alias, Meta, _Old, New, Opts}, #{lexical_tracker := Pid}) ->  
  ?tracker:add_alias(Pid, New, ?line(Meta), should_warn(Meta, Opts)),
  ok;
trace({alias_expansion, _Meta, Lookup, _Result}, #{lexical_tracker := Pid}) ->
  ?tracker:alias_dispatch(Pid, Lookup),
  ok;
trace({require, _Meta, Module, _Opts}, #{lexical_tracker := Pid}) ->
  %% We always record requires when they are defined
  %% as they expect the reference at compile time.
  ?tracker:remote_dispatch(Pid, Module, compile),
  ok;
trace({struct_expansion, _Meta, Module}, #{lexical_tracker := Pid}) ->
  ?tracker:remote_struct(Pid, Module),
  ok;
trace({remote_reference, _Meta, Module}, #{lexical_tracker := Pid} = E) ->
  ?tracker:remote_dispatch(Pid, Module, mode(E)),
  ok;
trace({remote_function, _Meta, Module, _Function, _Arity}, #{lexical_tracker := Pid} = E) ->
  ?tracker:remote_dispatch(Pid, Module, mode(E)),
  ok;
trace({remote_macro, _Meta, Module, _Function, _Arity}, #{lexical_tracker := Pid}) ->
  ?tracker:remote_dispatch(Pid, Module, compile),
  ok;
trace({imported_function, _Meta, Module, Function, Arity}, #{lexical_tracker := Pid}) ->
  ?tracker:import_dispatch(Pid, Module, {Function, Arity}),
  ok;
trace({imported_macro, _Meta, Module, Function, Arity}, #{lexical_tracker := Pid}) ->
  ?tracker:import_dispatch(Pid, Module, {Function, Arity}),
  ok;
trace(_, _) ->
  ok.

mode(#{function := nil}) -> compile;
mode(#{}) -> runtime.

should_warn(Meta, Opts) ->
  case lists:keyfind(warn, 1, Opts) of
    {warn, false} -> false;
    {warn, true} -> true;
    false -> not lists:keymember(context, 1, Meta)
  end.

%% EXTERNAL SOURCES

with_file(File, #{lexical_tracker := nil} = E, Callback) ->
  Callback(E#{file := File});
with_file(File, #{lexical_tracker := Pid} = E, Callback) ->
  try
    ?tracker:set_file(Pid, File),
    Callback(E#{file := File})
  after
    ?tracker:reset_file(Pid)
  end.

%% ERROR HANDLING

warn_unused_imports(Pid, E) ->
  {ModuleImports, MFAImports} =
    lists:partition(fun({M, _}) -> is_atom(M) end, ?tracker:collect_unused_imports(Pid)),

  Modules = [M || {M, _L} <- ModuleImports],
  MFAImportsFiltered = [T || {{M, _, _}, _} = T <- MFAImports, not lists:member(M, Modules)],

  [begin
    elixir_errors:form_warn([{line, L}], ?key(E, file), ?MODULE, {unused_import, M})
   end || {M, L} <- ModuleImports ++ MFAImportsFiltered],
  ok.

warn_unused_aliases(Pid, E) ->
  [begin
    elixir_errors:form_warn([{line, L}], ?key(E, file), ?MODULE, {unused_alias, M})
   end || {M, L} <- ?tracker:collect_unused_aliases(Pid)],
  ok.

format_error({unused_alias, Module}) ->
  io_lib:format("unused alias ~ts", [elixir_aliases:inspect(Module)]);
format_error({unused_import, {Module, Function, Arity}}) ->
  io_lib:format("unused import ~ts.~ts/~w", [elixir_aliases:inspect(Module), Function, Arity]);
format_error({unused_import, Module}) ->
  io_lib:format("unused import ~ts", [elixir_aliases:inspect(Module)]).
