defmodule Module.ParallelChecker do
  @moduledoc false

  def verify(modules, schedulers) do
    ets = :ets.new(:checker_cache, [:set, :public, {:read_concurrency, true}])
    preload_cache(ets, modules)
    {:ok, server} = :gen_server.start_link(__MODULE__, [], [])

    try do
      # TODO: Move spawning inside gen_server
      result = spawn(modules, 0, schedulers, {server, ets}, [])
      {binaries, warnings} = Enum.unzip(result)
      {binaries, Enum.concat(warnings)}
    after
      :ets.delete(ets)
      :gen_server.stop(server)
    end
  end

  def init([]) do
    {:ok, %{waiting: %{}}}
  end

  def handle_call({:lock, module}, from, %{waiting: waiting} = state) do
    case :maps.get(module, waiting, nil) do
      nil ->
        waiting = :maps.put(module, [], waiting)
        {:reply, true, %{state | waiting: waiting}}

      froms ->
        waiting = :maps.put(module, [from | froms], waiting)
        {:noreply, %{state | waiting: waiting}}
    end
  end

  def handle_call({:unlock, module}, _from, %{waiting: waiting} = state) do
    froms = :maps.get(module, waiting)
    Enum.each(froms, &:gen_server.reply(&1, false))
    waiting = :maps.remove(module, waiting)
    {:reply, :ok, %{state | waiting: waiting}}
  end

  def preload_module({server, ets}, module) do
    case :ets.lookup(ets, {:cached, module}) do
      [{_key, _}] -> :ok
      [] -> cache_module({server, ets}, module)
    end
  end

  def fetch_export({_server, ets}, module, fun, arity) do
    case :ets.lookup(ets, {:cached, module}) do
      [{_key, true}] ->
        case :ets.lookup(ets, {:export, {module, fun, arity}}) do
          [{_key, kind, _reason}] -> {:ok, kind}
          [] -> {:error, :function}
        end

      [{_key, false}] ->
        {:error, :module}
    end
  end

  def fetch_deprecated({_server, ets}, module, fun, arity) do
    # This is only called after we have checked undefined
    # so we can assume the mfa exists
    case :ets.lookup(ets, {:export, {module, fun, arity}}) do
      [{_key, _kind, nil}] -> :error
      [{_key, _kind, reason}] -> {:ok, reason}
    end
  end

  def all_exports({_server, ets}, module) do
    # This is only called after we get a deprecation notice
    # so we can assume it's a cached module
    [{_key, exports}] = :ets.lookup(ets, {:all_exports, module})

    exports
    |> Enum.map(fn {function, _kind} -> function end)
    |> Enum.sort()
  end

  def definitions_to_exports(definitions) do
    Enum.flat_map(definitions, fn {function, kind, _meta, _clauses} ->
      if kind in [:def, :defmacro] do
        [{function, kind}]
      else
        []
      end
    end)
  end

  defp preload_cache(ets, modules) do
    Enum.each(modules, fn {map, _binary} ->
      exports = [{{:__info__, 1}, :def} | definitions_to_exports(map.definitions)]
      deprecated = :maps.from_list(map.deprecated)
      cache_module(ets, map.module, exports, deprecated)
    end)
  end

  defp lock(server, module) do
    :gen_server.call(server, {:lock, module}, :infinity)
  end

  defp unlock(server, module) do
    :gen_server.call(server, {:unlock, module})
  end

  defp spawn([{map, binary} | results], spawned, schedulers, cache, acc)
       when spawned < schedulers do
    parent = self()

    :erlang.spawn_link(fn ->
      {binary, warnings} = Module.Checker.verify(map, binary, cache)
      send(parent, {__MODULE__, map.module, binary, warnings})
    end)

    spawn(results, spawned + 1, schedulers, cache, acc)
  end

  defp spawn(results, spawned, schedulers, cache, acc)
       when spawned == schedulers or (results == [] and spawned > 0) do
    receive do
      {__MODULE__, module, binary, warnings} ->
        acc = [{{module, binary}, warnings} | acc]
        spawn(results, spawned - 1, schedulers, cache, acc)
    end
  end

  defp spawn([], 0, _schedulers, _cache, acc) do
    acc
  end

  defp cache_module({server, ets}, module) do
    if lock(server, module) do
      cache_from_chunk(ets, module) || cache_from_info(ets, module)
      unlock(server, module)
    end
  end

  defp cache_from_chunk(ets, module) do
    with {^module, binary, _filename} <- :code.get_object_code(module),
         {:ok, chunk} <- get_chunk(binary),
         {:elixir_checker_v1, map} <- :erlang.binary_to_term(chunk) do
      exports = :maps.put({:__info__, 1}, :def, map.exports)
      cache_module(ets, module, exports, map.deprecated)
      true
    else
      _ -> false
    end
  end

  defp get_chunk(binary) do
    case :beam_lib.chunks(binary, ['ExCk'], [:allow_missing_chunks]) do
      {:ok, {_module, [{'ExCk', :missing_chunk}]}} -> :error
      {:ok, {_module, [{'ExCk', chunk}]}} -> {:ok, chunk}
      :error -> :error
    end
  end

  defp cache_from_info(ets, module) do
    if Code.ensure_loaded?(module) do
      exports = info_exports(module)
      deprecated = info_deprecated(module)
      cache_module(ets, module, exports, deprecated)
    else
      :ets.insert(ets, {{:cached, module}, false})
    end
  end

  defp info_exports(module) do
    :maps.from_list(
      Enum.map(module.__info__(:macros), &{&1, :defmacro}) ++
        Enum.map(module.__info__(:functions), &{&1, :def})
    )
  rescue
    _ -> :maps.from_list(Enum.map(module.module_info(:exports), &{&1, :def}))
  end

  defp info_deprecated(module) do
    :maps.from_list(module.__info__(:deprecated))
  rescue
    _ -> %{}
  end

  defp cache_module(ets, module, exports, deprecated) do
    all_exports =
      Enum.map(exports, fn {{fun, arity}, kind} ->
        reason = :maps.get({fun, arity}, deprecated, nil)
        :ets.insert(ets, {{:export, {module, fun, arity}}, kind, reason})

        {{fun, arity}, kind}
      end)

    :ets.insert(ets, {{:all_exports, module}, all_exports})
    :ets.insert(ets, {{:cached, module}, true})
  end
end
