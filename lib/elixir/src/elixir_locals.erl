%% Module responsible for tracking invocations of module calls.
-module(elixir_locals).
-export([
  setup/1, stop/1, cache_env/1, get_cached_env/1,
  record_local/3, record_import/4, record_defaults/4,
  yank/2, reattach/5, ensure_no_import_conflict/3,
  warn_unused_local/4, ensure_no_undefined_local/3, format_error/1
]).

-include("elixir.hrl").
-define(cache, {elixir, cache_env}).
-define(locals, {elixir, locals}).
-define(tracker, 'Elixir.Module.LocalsTracker').

setup({DataSet, _DataBag}) ->
  ets:insert(DataSet, {?cache, 0, nil}),

  case elixir_config:get(bootstrap) of
    false -> ets:insert(DataSet, {?locals, true});
    true -> ok
  end,

  ok.

stop({DataSet, _DataBag}) ->
  ets:delete(DataSet, ?locals).

yank(Tuple, Module) ->
  if_tracker(Module, fun(Tracker) -> ?tracker:yank(Tracker, Tuple) end).

reattach(Tuple, Kind, Module, Function, Neighbours) ->
  if_tracker(Module, fun(Tracker) -> ?tracker:reattach(Tracker, Tuple, Kind, Function, Neighbours) end).

record_local(Tuple, _Module, Function)
  when Function == nil; Function == Tuple -> ok;
record_local(Tuple, Module, Function) ->
  if_tracker(Module, fun(Tracker) -> ?tracker:add_local(Tracker, Function, Tuple), ok end).

record_import(_Tuple, Receiver, Module, Function)
  when Function == nil; Module == Receiver -> false;
record_import(Tuple, Receiver, Module, Function) ->
  if_tracker(Module, fun(Tracker) -> ?tracker:add_import(Tracker, Function, Receiver, Tuple), ok end).

record_defaults(_Tuple, _Kind, _Module, 0) ->
  ok;
record_defaults(Tuple, Kind, Module, Defaults) ->
  if_tracker(Module, fun(Tracker) -> ?tracker:add_defaults(Tracker, Kind, Tuple, Defaults), ok end).

if_tracker(Module, Callback) ->
  if_tracker(Module, ok, Callback).

if_tracker(Module, Default, Callback) ->
  try
    {DataSet, _} = Tables = elixir_module:data_tables(Module),
    {ets:member(DataSet, ?locals), Tables}
  of
    {true, Tracker} -> Callback(Tracker);
    {false, _} -> Default
  catch
    error:badarg -> Default
  end.

%% CACHING

cache_env(#{line := Line, module := Module} = E) ->
  {Set, _} = elixir_module:data_tables(Module),
  Cache = elixir_env:reset_vars(E#{line := nil}),

  Pos =
    case ets:lookup(Set, ?cache) of
      [{_, Key, Cache}] ->
        Key;
      [{_, PrevKey, _}] ->
        Key = PrevKey + 1,
        ets:insert(Set, [{{cache_env, Key}, Cache},{?cache, Key, Cache}]),
        Key
    end,

  {Module, {Line, Pos}}.

get_cached_env({Module, {Line, Pos}}) ->
  {Set, _} = elixir_module:data_tables(Module),
  (ets:lookup_element(Set, {cache_env, Pos}, 2))#{line := Line};
get_cached_env(Env) ->
  Env.

%% ERROR HANDLING

ensure_no_import_conflict(_File, 'Elixir.Kernel', _All) ->
  ok;
ensure_no_import_conflict(File, Module, All) ->
  if_tracker(Module, ok, fun(Tracker) ->
    [elixir_errors:form_error(Meta, File, ?MODULE, {function_conflict, Error})
     || {Meta, Error} <- ?tracker:collect_imports_conflicts(Tracker, All)],
    ok
  end).

warn_unused_local(File, Module, All, Private) ->
  if_tracker(Module, [], fun(Tracker) ->
    {Unreachable, Warnings} = ?tracker:collect_unused_locals(Tracker, All, Private),
    [elixir_errors:form_warn(Meta, File, ?MODULE, Error) || {Meta, Error} <- Warnings],
    Unreachable
  end).

ensure_no_undefined_local(File, Module, All) ->
  if_tracker(Module, [], fun(Tracker) ->
    Warnings = ?tracker:collect_undefined_locals(Tracker, All),
    [elixir_errors:form_error(Meta, File, ?MODULE, Error) || {Meta, Error} <- Warnings]
  end).

format_error({function_conflict, {Receiver, {Name, Arity}}}) ->
  io_lib:format("imported ~ts.~ts/~B conflicts with local function",
    [elixir_aliases:inspect(Receiver), Name, Arity]);

format_error({unused_args, {Name, Arity}}) ->
  io_lib:format("default arguments in ~ts/~B are never used", [Name, Arity]);

format_error({unused_args, {Name, Arity}, 1}) ->
  io_lib:format("the first default argument in ~ts/~B is never used", [Name, Arity]);

format_error({unused_args, {Name, Arity}, Count}) ->
  io_lib:format("the first ~B default arguments in ~ts/~B are never used", [Count, Name, Arity]);

format_error({unused_def, {Name, Arity}, defp}) ->
  io_lib:format("function ~ts/~B is unused", [Name, Arity]);

format_error({unused_def, {Name, Arity}, defmacrop}) ->
  io_lib:format("macro ~ts/~B is unused", [Name, Arity]);

format_error({undefined_function, {F, A}}) ->
  io_lib:format("undefined function ~ts/~B", [F, A]).
