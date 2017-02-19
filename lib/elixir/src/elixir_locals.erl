%% Module responsible for tracking invocations of module calls.
-module(elixir_locals).
-export([
  setup/1, cleanup/1, cache_env/1, get_cached_env/1,
  record_local/2, record_local/3, record_import/4,
  record_definition/3, record_defaults/4,
  ensure_no_import_conflict/3, warn_unused_local/3, format_error/1
]).

-include("elixir.hrl").
-define(attr, {elixir, locals_tracker}).
-define(tracker, 'Elixir.Module.LocalsTracker').

setup(Module) ->
  case elixir_compiler:get_opt(internal) of
    false ->
      {ok, Pid} = ?tracker:start_link(),
      ets:insert(elixir_module:data_table(Module), {?attr, Pid}),
      ok;
    true ->
      ok
  end.

cleanup(Module) ->
  if_tracker(Module, fun(Pid) -> unlink(Pid), ?tracker:stop(Pid), ok end).

record_local(Tuple, Module) when is_atom(Module) ->
  if_tracker(Module, fun(Pid) -> ?tracker:add_local(Pid, Tuple), ok end).
record_local(Tuple, _Module, Function)
  when Function == nil; Function == Tuple -> ok;
record_local(Tuple, Module, Function) ->
  if_tracker(Module, fun(Pid) -> ?tracker:add_local(Pid, Function, Tuple), ok end).

record_import(_Tuple, Receiver, Module, _Function)
  when Module == nil; Module == Receiver -> false;
record_import(Tuple, Receiver, Module, Function) ->
  if_tracker(Module, fun(Pid) -> ?tracker:add_import(Pid, Function, Receiver, Tuple), ok end).

record_definition(Tuple, Kind, Module) ->
  if_tracker(Module, fun(Pid) -> ?tracker:add_definition(Pid, Kind, Tuple), ok end).

record_defaults(_Tuple, _Kind, _Module, 0) ->
  ok;
record_defaults(Tuple, Kind, Module, Defaults) ->
  if_tracker(Module, fun(Pid) -> ?tracker:add_defaults(Pid, Kind, Tuple, Defaults), ok end).

if_tracker(Module, Callback) ->
  if_tracker(Module, ok, Callback).

if_tracker(Module, Default, Callback) ->
  try ets:lookup_element(elixir_module:data_table(Module), ?attr, 2) of
    Pid -> Callback(Pid)
  catch
    error:badarg -> Default
  end.

%% CACHING

cache_env(#{module := Module} = RE) ->
  E = RE#{line := nil, vars := []},
  try ets:lookup_element(elixir_module:data_table(Module), ?attr, 2) of
    Pid ->
      {Pid, ?tracker:cache_env(Pid, E)}
  catch
    error:badarg ->
      {Escaped, _} = elixir_quote:escape(E, false),
      Escaped
  end.

get_cached_env({Pid, Ref}) -> ?tracker:get_cached_env(Pid, Ref);
get_cached_env(Env) -> Env.

%% ERROR HANDLING

ensure_no_import_conflict(_File, 'Elixir.Kernel', _All) ->
  ok;
ensure_no_import_conflict(File, Module, All) ->
  if_tracker(Module, ok, fun(Pid) ->
    [elixir_errors:form_error(Meta, File, ?MODULE, {function_conflict, Error})
     || {Meta, Error} <- ?tracker:collect_imports_conflicts(Pid, All)],
    ok
  end).

warn_unused_local(File, Module, Private) ->
  if_tracker(Module, [], fun(Pid) ->
    {Unreachable, Warnings} = ?tracker:collect_unused_locals(Pid, Private),
    [elixir_errors:form_warn(Meta, File, ?MODULE, Error) || {Meta, Error} <- Warnings],
    Unreachable
  end).

format_error({function_conflict, {Receivers, Name, Arity}}) ->
  io_lib:format("imported ~ts.~ts/~B conflicts with local function",
    [elixir_aliases:inspect(hd(Receivers)), Name, Arity]);

format_error({unused_args, {Name, Arity}}) ->
  io_lib:format("default arguments in ~ts/~B are never used", [Name, Arity]);

format_error({unused_args, {Name, Arity}, 1}) ->
  io_lib:format("the first default argument in ~ts/~B is never used", [Name, Arity]);

format_error({unused_args, {Name, Arity}, Count}) ->
  io_lib:format("the first ~B default arguments in ~ts/~B are never used", [Count, Name, Arity]);

format_error({unused_def, {Name, Arity}, defp}) ->
  io_lib:format("function ~ts/~B is unused", [Name, Arity]);

format_error({unused_def, {Name, Arity}, defmacrop}) ->
  io_lib:format("macro ~ts/~B is unused", [Name, Arity]).
