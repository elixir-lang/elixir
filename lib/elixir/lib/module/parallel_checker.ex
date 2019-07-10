defmodule Module.ParallelChecker do
  @moduledoc false

  @type cache() :: {pid(), :ets.tid()}
  @type warning() :: term()
  @type kind() :: :def | :defmacro

  @doc """
  Receives pairs of module maps and BEAM binaries. In parallel it verifies
  the modules and adds the ExCk chunk to the binaries. Returns the updated
  binaries and a list of warnings from the verification.
  """
  @spec verify([%{}], pos_integer()) :: {[{module(), binary()}], [warning()]}
  def verify([], _schedulers) do
    {[], []}
  end

  def verify(modules, schedulers) do
    {:ok, server} = :gen_server.start_link(__MODULE__, [modules, schedulers], [])

    try do
      start_and_wait(server)
    after
      :gen_server.stop(server)
    end
  end

  @doc """
  Preloads a module into the cache. Call this function before any other
  cache lookups for the module.
  """
  @spec preload_module(cache(), module()) :: :ok
  def preload_module({server, ets}, module) do
    case :ets.lookup(ets, {:cached, module}) do
      [{_key, _}] -> :ok
      [] -> cache_module({server, ets}, module)
    end
  end

  @doc """
  Returns the export kind and deprecation reason for the given MFA from
  the cache. If the module does not exist return `{:error, :module}`,
  or if the function does not exist return `{:error, :function}`.
  """
  @spec fetch_export(cache(), module(), atom(), arity()) ::
          {:ok, kind(), binary() | nil} | {:error, :function | :module}
  def fetch_export({_server, ets}, module, fun, arity) do
    case :ets.lookup(ets, {:cached, module}) do
      [{_key, true}] ->
        case :ets.lookup(ets, {:export, {module, fun, arity}}) do
          [{_key, kind, reason}] -> {:ok, kind, reason}
          [] -> {:error, :function}
        end

      [{_key, false}] ->
        {:error, :module}
    end
  end

  @doc """
  Returns all exported functions and macros for the given module from
  the cache.
  """
  @spec all_exports(cache(), module()) :: [{atom(), arity()}]
  def all_exports({_server, ets}, module) do
    # This is only called after we get a deprecation notice
    # so we can assume it's a cached module
    [{_key, exports}] = :ets.lookup(ets, {:all_exports, module})

    exports
    |> Enum.map(fn {function, _kind} -> function end)
    |> Enum.sort()
  end

  @doc """
  Collects all exported functions and macros from the module definition ASTs.
  """
  @spec definitions_to_exports([{atom(), arity(), term(), term()}]) ::
          [{{atom(), arity()}, kind()}]
  def definitions_to_exports(definitions) do
    Enum.flat_map(definitions, fn {function, kind, _meta, _clauses} ->
      if kind in [:def, :defmacro] do
        [{function, kind}]
      else
        []
      end
    end)
  end

  def init([modules, schedulers]) do
    ets = :ets.new(:checker_cache, [:set, :public, {:read_concurrency, true}])
    preload_cache(ets, modules)

    state = %{
      ets: ets,
      waiting: %{},
      reply_when_finished: nil,
      modules: modules,
      spawned: 0,
      schedulers: schedulers,
      binaries: [],
      warnings: []
    }

    {:ok, state}
  end

  def handle_call(:start_and_wait, from, %{reply_when_finished: nil} = state) do
    state = spawn_checkers(%{state | reply_when_finished: from})
    {:noreply, state}
  end

  def handle_call({:lock, module}, from, %{waiting: waiting} = state) do
    case waiting do
      %{^module => froms} ->
        waiting = :maps.put(module, [from | froms], state.waiting)
        {:noreply, %{state | waiting: waiting}}

      %{} ->
        waiting = :maps.put(module, [], state.waiting)
        {:reply, true, %{state | waiting: waiting}}
    end
  end

  def handle_call({:unlock, module}, _from, %{waiting: waiting} = state) do
    froms = :maps.get(module, waiting)
    Enum.each(froms, &:gen_server.reply(&1, false))
    waiting = :maps.remove(module, waiting)
    {:reply, :ok, %{state | waiting: waiting}}
  end

  def handle_info({__MODULE__, module, binary, warnings}, state) do
    state = %{
      state
      | binaries: [{module, binary} | state.binaries],
        warnings: warnings ++ state.warnings,
        spawned: state.spawned - 1
    }

    if state.spawned == 0 and state.modules == [] do
      :gen_server.reply(state.reply_when_finished, {state.binaries, state.warnings})
      {:noreply, state}
    else
      state = spawn_checkers(state)
      {:noreply, state}
    end
  end

  defp start_and_wait(server) do
    :gen_server.call(server, :start_and_wait, :infinity)
  end

  defp lock(server, module) do
    :gen_server.call(server, {:lock, module}, :infinity)
  end

  defp unlock(server, module) do
    :gen_server.call(server, {:unlock, module})
  end

  defp preload_cache(ets, modules) do
    Enum.each(modules, fn {map, _binary} ->
      exports = [{{:__info__, 1}, :def} | definitions_to_exports(map.definitions)]
      deprecated = :maps.from_list(map.deprecated)
      cache_info(ets, map.module, exports, deprecated)
    end)
  end

  defp spawn_checkers(%{modules: []} = state) do
    state
  end

  defp spawn_checkers(%{spawned: spawned, schedulers: schedulers} = state)
       when spawned >= schedulers do
    state
  end

  defp spawn_checkers(%{modules: [{map, binary} | modules]} = state) do
    parent = self()
    ets = state.ets

    spawn_link(fn ->
      {binary, warnings} = Module.Checker.verify(map, binary, {parent, ets})
      send(parent, {__MODULE__, map.module, binary, warnings})
    end)

    spawn_checkers(%{state | modules: modules, spawned: state.spawned + 1})
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
         {:elixir_checker_v1, contents} <- :erlang.binary_to_term(chunk) do
      cache_chunk(ets, module, contents)
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
      cache_info(ets, module, exports, deprecated)
    else
      :ets.insert(ets, {{:cached, module}, false})
    end
  end

  defp info_exports(module) do
    :maps.from_list(
      [{{:__info__, 1}, :def}] ++
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

  defp cache_info(ets, module, exports, deprecated) do
    exports =
      Enum.map(exports, fn {{fun, arity}, kind} ->
        reason = :maps.get({fun, arity}, deprecated, nil)
        :ets.insert(ets, {{:export, {module, fun, arity}}, kind, reason})

        {{fun, arity}, kind}
      end)

    :ets.insert(ets, {{:all_exports, module}, exports})
    :ets.insert(ets, {{:cached, module}, true})
  end

  defp cache_chunk(ets, module, exports) do
    exports =
      Enum.map(exports, fn {{fun, arity}, {kind, deprecated}} ->
        :ets.insert(ets, {{:export, {module, fun, arity}}, kind, deprecated})

        {{fun, arity}, kind}
      end)

    :ets.insert(ets, {{:export, {module, :__info__, 1}}, :def, nil})
    exports = [{{:__info__, 1}, :def} | exports]

    :ets.insert(ets, {{:all_exports, module}, exports})
    :ets.insert(ets, {{:cached, module}, true})
  end
end
