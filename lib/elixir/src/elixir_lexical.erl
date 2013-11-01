%% Module responsible for tracking lexical information.
-module(elixir_lexical).
-export([run/2,
  record_alias/4, record_alias/2,
  record_import/4, record_import/2,
  record_remote/2, format_error/1
]).
-include("elixir.hrl").

-define(tracker, 'Elixir.Kernel.LexicalTracker').

run(File, Callback) ->
  case code:is_loaded(?tracker) of
    { file, _ } ->
      Pid = ?tracker:start_link(),

      try
        Callback(Pid)
      after
        warn_unused_aliases(File, Pid),
        warn_unused_imports(File, Pid),
        unlink(Pid), ?tracker:stop(Pid)
      end;
    false ->
      Callback(nil)
  end.

%% RECORD

record_alias(Module, Line, Warn, Ref) ->
  if_tracker(Ref, fun(Pid) ->
    ?tracker:add_alias(Pid, Module, Line, Warn),
    true
  end).

record_import(Module, Line, Warn, Ref) ->
  if_tracker(Ref, fun(Pid) ->
    ?tracker:add_import(Pid, Module, Line, Warn),
    true
  end).

record_alias(Module, Ref) ->
  if_tracker(Ref, fun(Pid) ->
    ?tracker:alias_dispatch(Pid, Module),
    true
  end).

record_import(Module, Ref) ->
  if_tracker(Ref, fun(Pid) ->
    ?tracker:import_dispatch(Pid, Module),
    true
  end).

record_remote(Module, Ref) ->
  if_tracker(Ref, fun(Pid) ->
    ?tracker:remote_dispatch(Pid, Module),
    true
  end).

%% HELPERS

if_tracker(nil, _Callback) -> false;
if_tracker(Pid, Callback) when is_pid(Pid) -> Callback(Pid).

%% ERROR HANDLING

warn_unused_imports(File, Pid) ->
  [ begin
      elixir_errors:handle_file_warning(File, { L, ?MODULE, { unused_import, M } })
    end || { M, L } <- ?tracker:collect_unused_imports(Pid)].

warn_unused_aliases(File, Pid) ->
  [ begin
      elixir_errors:handle_file_warning(File, { L, ?MODULE, { unused_alias, M } })
    end || { M, L } <- ?tracker:collect_unused_aliases(Pid)].

format_error({unused_alias, Module}) ->
  io_lib:format("unused alias ~ts", [elixir_errors:inspect(Module)]);
format_error({unused_import, Module}) ->
  io_lib:format("unused import ~ts", [elixir_errors:inspect(Module)]).
