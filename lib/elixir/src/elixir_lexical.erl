%% Module responsible for tracking lexical information.
-module(elixir_lexical).
-export([run/2, set_file/2, reset_file/1,
  record_alias/4, record_alias/2,
  record_import/4, record_import/5,
  record_remote/3, record_struct/2,
  format_error/1
]).
-include("elixir.hrl").

-define(tracker, 'Elixir.Kernel.LexicalTracker').

run(File, Callback) ->
  case elixir_config:get(bootstrap) of
    false ->
      {ok, Pid} = ?tracker:start_link(),
      try Callback(Pid) of
        Res ->
          warn_unused_aliases(File, Pid),
          warn_unused_imports(File, Pid),
          Res
      after
        unlink(Pid),
        ?tracker:stop(Pid)
      end;
    true ->
      Callback(nil)
  end.

%% RECORD

record_alias(Module, Line, Warn, Ref) ->
  if_tracker(Ref, fun(Pid) -> ?tracker:add_alias(Pid, Module, Line, Warn), ok end).

record_import(Module, FAs, Line, Warn, Ref) ->
  if_tracker(Ref, fun(Pid) -> ?tracker:add_import(Pid, Module, FAs, Line, Warn), ok end).

record_alias(Module, Ref) ->
  if_tracker(Ref, fun(Pid) -> ?tracker:alias_dispatch(Pid, Module), ok end).

record_import(Module, Function, Arity, Ref) ->
  if_tracker(Ref, fun(Pid) -> ?tracker:import_dispatch(Pid, Module, {Function, Arity}), ok end).

record_remote(Module, EnvFunction, Ref) ->
  if_tracker(Ref, fun(Pid) -> ?tracker:remote_dispatch(Pid, Module, mode(EnvFunction)), ok end).

record_struct(Module, Ref) ->
  if_tracker(Ref, fun(Pid) -> ?tracker:remote_struct(Pid, Module), ok end).

%% EXTERNAL SOURCES

set_file(File, Ref) ->
  if_tracker(Ref, fun(Pid) -> ?tracker:set_file(Pid, File), ok end).

reset_file(Ref) ->
  if_tracker(Ref, fun(Pid) -> ?tracker:reset_file(Pid), ok end).

%% HELPERS

mode(nil) -> compile;
mode({_, _}) -> runtime.

if_tracker(nil, _Callback) -> ok;
if_tracker(Pid, Callback) when is_pid(Pid) -> Callback(Pid).

%% ERROR HANDLING

warn_unused_imports(File, Pid) ->
  {ModuleImports, MFAImports} =
    lists:partition(fun({M, _}) -> is_atom(M) end, ?tracker:collect_unused_imports(Pid)),
  Modules = [M || {M, _L} <- ModuleImports],
  MFAImportsFiltered = [T || {{M, _, _}, _} = T <- MFAImports, not lists:member(M, Modules)],

  [begin
    elixir_errors:form_warn([{line, L}], File, ?MODULE, {unused_import, M})
   end || {M, L} <- ModuleImports ++ MFAImportsFiltered],
  ok.

warn_unused_aliases(File, Pid) ->
  [begin
    elixir_errors:form_warn([{line, L}], File, ?MODULE, {unused_alias, M})
   end || {M, L} <- ?tracker:collect_unused_aliases(Pid)],
  ok.

format_error({unused_alias, Module}) ->
  io_lib:format("unused alias ~ts", [elixir_aliases:inspect(Module)]);
format_error({unused_import, {Module, Function, Arity}}) ->
  io_lib:format("unused import ~ts.~ts/~w", [elixir_aliases:inspect(Module), Function, Arity]);
format_error({unused_import, Module}) ->
  io_lib:format("unused import ~ts", [elixir_aliases:inspect(Module)]).
