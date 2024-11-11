%% Module responsible for tracking invocations of module calls.
-module(elixir_locals).
-export([
  setup/1, stop/1, cache_env/1, get_cached_env/1,
  record_local/5, record_import/4, record_defaults/5,
  yank/2, reattach/6, ensure_no_import_conflict/3,
  format_error/1
]).

-include("elixir.hrl").
-define(cache_key, {elixir, cache_env}).
-define(locals_key, {elixir, locals}).
-define(locals, 'Elixir.Module.LocalsTracker').

setup({DataSet, _DataBag}) ->
  ets:insert(DataSet, {?cache_key, 0}),

  case elixir_config:is_bootstrap() of
    false -> ets:insert(DataSet, {?locals_key, true});
    true -> ok
  end,

  ok.

stop({DataSet, _DataBag}) ->
  ets:delete(DataSet, ?locals_key).

yank(Tuple, Module) ->
  if_tracker(Module, fun(Tracker) -> ?locals:yank(Tracker, Tuple) end).

reattach(Tuple, Kind, Module, Function, Neighbours, Meta) ->
  if_tracker(Module, fun(Tracker) -> ?locals:reattach(Tracker, Tuple, Kind, Function, Neighbours, Meta) end).

record_local(_Tuple, _Module, nil, _Meta, _IsMacroDispatch) ->
  ok;
record_local(Tuple, Module, Function, Meta, IsMacroDispatch) ->
  if_tracker(Module, fun(Tracker) -> ?locals:add_local(Tracker, Function, Tuple, Meta, IsMacroDispatch), ok end).

record_import(_Tuple, Receiver, Module, Function)
  when Function == nil; Module == Receiver -> false;
record_import(Tuple, Receiver, Module, Function) ->
  if_tracker(Module, fun(Tracker) -> ?locals:add_import(Tracker, Function, Receiver, Tuple), ok end).

record_defaults(_Tuple, _Kind, _Module, 0, _Meta) ->
  ok;
record_defaults(Tuple, Kind, Module, Defaults, Meta) ->
  if_tracker(Module, fun(Tracker) -> ?locals:add_defaults(Tracker, Kind, Tuple, Defaults, Meta), ok end).

if_tracker(Module, Callback) ->
  if_tracker(Module, ok, Callback).

if_tracker(Module, Default, Callback) ->
  try
    {DataSet, _} = Tables = elixir_module:data_tables(Module),
    {ets:member(DataSet, ?locals_key), Tables}
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
  PrevKey = ets:lookup_element(Set, ?cache_key, 2),

  Pos =
    case ets:lookup(Set, {cache_env, PrevKey}) of
      [{_, Cache}] ->
        PrevKey;
      _ ->
        NewKey = PrevKey + 1,
        ets:insert(Set, [{{cache_env, NewKey}, Cache}, {?cache_key, NewKey}]),
        NewKey
    end,

  {Module, {Line, Pos}}.

get_cached_env({Module, {Line, Pos}}) ->
  {Set, _} = elixir_module:data_tables(Module),
  (ets:lookup_element(Set, {cache_env, Pos}, 2))#{line := Line};
get_cached_env(Env) ->
  Env.

%% ERROR HANDLING

ensure_no_import_conflict('Elixir.Kernel', _All, _E) ->
  ok;
ensure_no_import_conflict(Module, All, E) ->
  if_tracker(Module, ok, fun(Tracker) ->
    [elixir_errors:module_error(Meta, E, ?MODULE, {function_conflict, Error})
     || {Meta, Error} <- ?locals:collect_imports_conflicts(Tracker, All)],
    ok
  end).

format_error({function_conflict, {Receiver, {Name, Arity}}}) ->
  io_lib:format("imported ~ts.~ts/~B conflicts with local function",
    [elixir_aliases:inspect(Receiver), Name, Arity]).
