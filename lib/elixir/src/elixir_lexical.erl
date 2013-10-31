%% Module responsible for tracking lexical information.
-module(elixir_lexical).
-export([run/2, pid/1,
  record_alias/4, record_alias/2,
  record_import/4, record_import/2,
  record_remote/2, format_error/1
]).
-include("elixir.hrl").

-define(tracker, 'Elixir.Kernel.LexicalTracker').

pid(File) ->
  if_tracker(File, fun(Pid) -> Pid end).

%% TODO: This only works if the file is not duplicated.
%% We need to store the PID in the lexical scope.
run(File, Callback) ->
  case code:is_loaded(?tracker) of
    { file, _ } ->
      Pid = ?tracker:start_link(),
      elixir_code_server:cast({ register_lexical, File, Pid }),
      try
        Callback()
      after
        %% Emit the warnings
        warn_unused_imports(File, Pid),

        %% Unlink and turn off tracker
        unlink(Pid),
        ?tracker:stop(Pid),
        elixir_code_server:cast({ unregister_lexical, File })
      end;
    false ->
      Callback()
  end.

%% RECORD

record_alias(File, Module, Line, Warn) ->
  if_tracker(File, fun(Pid) ->
    ?tracker:add_alias(Pid, Module, Line, Warn),
    true
  end).

record_import(File, Module, Line, Warn) ->
  if_tracker(File, fun(Pid) ->
    ?tracker:add_import(Pid, Module, Line, Warn),
    true
  end).

record_alias(File, Module) ->
  if_tracker(File, fun(Pid) ->
    ?tracker:alias_dispatch(Pid, Module),
    true
  end).

record_import(File, Module) ->
  if_tracker(File, fun(Pid) ->
    ?tracker:import_dispatch(Pid, Module),
    true
  end).

record_remote(File, Module) ->
  if_tracker(File, fun(Pid) ->
    ?tracker:remote_dispatch(Pid, Module),
    true
  end).

%% HELPERS

if_tracker(File, Callback) ->
  case elixir_code_server:call({ lexical, File }) of
    { ok, Pid } -> Callback(Pid);
    error -> false
  end.

%% ERROR HANDLING

warn_unused_imports(File, Pid) ->
  [ begin
      elixir_errors:handle_file_warning(File, { L, ?MODULE, { unused_import, M } })
    end || { M, L } <- ?tracker:collect_unused_imports(Pid)].

format_error({unused_import, Module}) ->
  io_lib:format("unused import ~ts", [elixir_errors:inspect(Module)]).
