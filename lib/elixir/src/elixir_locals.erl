%% Module responsible for tracking invocations of module calls.
-module(elixir_locals).
-export([
  setup/1, cache_env/1, get_cached_env/1,
  record_import/4,
  ensure_no_import_conflict/3,
  format_error/1
]).

-include("elixir.hrl").
-define(cache_key, {elixir, cache_env}).

setup({DataSet, _DataBag}) ->
  ets:insert(DataSet, {?cache_key, 0}),
  ok.

record_import(_Tuple, Receiver, Module, Function)
  when Function == nil; Module == Receiver -> false;
record_import(Tuple, Receiver, Module, _Function) ->
  try
    {Set, _Bag} = elixir_module:data_tables(Module),
    ets:insert(Set, {{import, Tuple}, Receiver}),
    true
  catch
    error:badarg -> false
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
ensure_no_import_conflict(Module, AllDefinitions, E) ->
  {Set, _} = elixir_module:data_tables(Module),

  [try
     Receiver = ets:lookup_element(Set, {import, Pair}, 2),
     elixir_errors:module_error(Meta, E, ?MODULE, {import_conflict, Receiver, Pair})
   catch
    error:badarg -> false
   end || {Pair, _, Meta, _} <- AllDefinitions].

format_error({import_conflict, Receiver, {Name, Arity}}) ->
  io_lib:format("imported ~ts.~ts/~B conflicts with local function",
    [elixir_aliases:inspect(Receiver), Name, Arity]).
