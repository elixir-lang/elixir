%% Module responsible for tracking invocations of locals,
%% imports and requires.
-module(elixir_tracker).
-export([
  setup/1, cleanup/1,
  record_local/2, record_import/3,
  record_warn/4, record_definition/3,
  ensure_no_import_conflict/4, ensure_all_imports_used/3,
  warn_unused_local/3, format_error/1
]).
-include("elixir.hrl").

-define(attr, '__dispatch_tracker').
-define(tracker, 'Elixir.Module.DispatchTracker').

%% RECORD

setup(Module) ->
  case code:ensure_loaded(?tracker) of
    { module, _ } -> ets:insert(Module, { ?attr, ?tracker:start_link() });
    { error, _ }  -> ok
  end.

cleanup(Module) ->
  if_tracker(Module, fun(Pid) -> ?tracker:stop(Pid) end).

record_local(Tuple, Module) when is_atom(Module) ->
  if_tracker(Module, fun(Pid) ->
    ?tracker:add_local(Pid, Tuple),
    true
  end);
record_local(Tuple, #elixir_scope{function=Function})
  when Function == nil; Function == Tuple -> false;
record_local(Tuple, #elixir_scope{module=Module, function=Function}) ->
  if_tracker(Module, fun(Pid) ->
    ?tracker:add_local(Pid, Function, Tuple),
    true
  end).

record_warn(Ref, Warn, Line, Module) ->
  if_tracker(Module, fun(Pid) ->
    ?tracker:add_warnable(Pid, Ref, Warn, Line),
    true
  end).

record_import(_Tuple, Receiver, Module)
  when Module == nil; Module == Receiver -> false;
record_import(Tuple, Receiver, Module) ->
  try
    Pid = ets:lookup_element(Module, ?attr, 2),
    ?tracker:add_import(Pid, Receiver, Tuple)
  catch
    error:badarg -> false
  end.

record_definition(Tuple, Kind, Module) ->
  if_tracker(Module, fun(Pid) ->
    ?tracker:add_definition(Pid, Kind, Tuple),
    true
  end).

%% HELPERS

if_tracker(Module, Callback) ->
  case ets:lookup(Module, ?attr) of
    [{ ?attr, Pid }] -> Callback(Pid);
    _ -> false
  end.

%% ERROR HANDLING

%% Ensure no import conflicts with any of the local definitions.

ensure_no_import_conflict(Meta, File, Module, AllDefined) ->
  if_tracker(Module, fun(Pid) ->
    [do_ensure_no_import_conflict(Meta, File, Pid, X) || X  <- AllDefined]
  end),
  ok.

do_ensure_no_import_conflict(Meta, File, Pid, { Name, Arity } = Tuple) ->
  Matches = ?tracker:imports_with_dispatch(Pid, Tuple),

  case Matches of
    []  -> ok;
    Key ->
      Error = { import_conflict, { hd(Key), Name, Arity } },
      elixir_errors:form_error(Meta, File, ?MODULE, Error)
  end.

%% Ensure all imports are used.

ensure_all_imports_used(_Line, File, Module) ->
  if_tracker(Module, fun(Pid) ->
    [ begin
        elixir_errors:handle_file_warning(File, { L, ?MODULE, { unused_import, M } })
      end || { M, L } <- ?tracker:collect_unused_imports(Pid)]
  end),
  ok.

%% Warn for unused/unreachable locals and default arguments.

warn_unused_local(File, Module, Private) ->
  if_tracker(Module, fun(Pid) ->
    Args = [ { Fun, Kind, Defaults } ||
             { Fun, Kind, _Line, true, Defaults } <- Private],

    Unused = ?tracker:collect_unused_locals(Pid, Args),

    [ begin
        { _, _, Line, _, _ } = lists:keyfind(element(2, Error), 1, Private),
        elixir_errors:handle_file_warning(File, { Line, ?MODULE, Error })
      end || Error <- Unused ]
  end).

format_error({import_conflict,{Receiver, Name, Arity}}) ->
  io_lib:format("imported ~ts.~ts/~B conflicts with local function",
    [elixir_errors:inspect(Receiver), Name, Arity]);

format_error({ unused_import, Module }) ->
  io_lib:format("unused import ~ts", [elixir_errors:inspect(Module)]);

format_error({unused_args,{Name, Arity}}) ->
  io_lib:format("default arguments in ~ts/~B are never used", [Name, Arity]);

format_error({unused_args,{Name, Arity},1}) ->
  io_lib:format("the first default argument in ~ts/~B is never used", [Name, Arity]);

format_error({unused_args,{Name, Arity},Count}) ->
  io_lib:format("the first ~B default arguments in ~ts/~B are never used", [Count, Name, Arity]);

format_error({unused_def,{Name, Arity},defp}) ->
  io_lib:format("function ~ts/~B is unused", [Name, Arity]);

format_error({unused_def,{Name, Arity},defmacrop}) ->
  io_lib:format("macro ~ts/~B is unused", [Name, Arity]).
