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
  @spec verify([{map(), binary()}], [{module(), binary()}], pos_integer() | nil) :: [warning()]
  def verify(compiled_modules, runtime_binaries, schedulers \\ nil) do
    compiled_maps = Enum.map(compiled_modules, fn {map, _binary} -> {map.module, map} end)

    case compiled_maps ++ runtime_binaries do
      [] ->
        []

      check_modules ->
        schedulers = schedulers || max(:erlang.system_info(:schedulers_online), 2)

        {:ok, server} =
          :gen_server.start_link(__MODULE__, [check_modules, self(), schedulers], [])

        preload_cache(get_ets(server), check_modules)
        start(server)
        collect_results(length(check_modules), [])
    end
  end

  defp collect_results(0, warnings) do
    warnings
  end

  defp collect_results(count, warnings) do
    receive do
      {__MODULE__, _module, new_warnings} ->
        collect_results(count - 1, new_warnings ++ warnings)
    end
  end

  @doc """
  Test cache.
  """
  def test_cache do
    {:ok, pid} = :gen_server.start_link(__MODULE__, [[], self(), 1], [])
    {pid, get_ets(pid)}
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

  ## Module checking

  defp check_module(module, cache) do
    case extract_definitions(module) do
      {:ok, module, file, definitions, no_warn_undefined} ->
        Module.Types.warnings(module, file, definitions, no_warn_undefined, cache)
        |> group_warnings()
        |> emit_warnings()

      :error ->
        []
    end
  end

  defp extract_definitions({module, module_map}) when is_map(module_map) do
    no_warn_undefined =
      module_map.compile_opts
      |> extract_no_warn_undefined()
      |> merge_compiler_no_warn_undefined()

    {:ok, module, module_map.file, module_map.definitions, no_warn_undefined}
  end

  defp extract_definitions({module, binary}) when is_binary(binary) do
    with {:ok, {_, [debug_info: chunk]}} <- :beam_lib.chunks(binary, [:debug_info]),
         {:debug_info_v1, backend, data} <- chunk,
         {:ok, module_map} <- backend.debug_info(:elixir_v1, module, data, []) do
      extract_definitions({module, module_map})
    else
      _ -> :error
    end
  end

  defp extract_no_warn_undefined(compile_opts) do
    for(
      {:no_warn_undefined, values} <- compile_opts,
      value <- List.wrap(values),
      do: value
    )
  end

  defp merge_compiler_no_warn_undefined(no_warn_undefined) do
    case Code.get_compiler_option(:no_warn_undefined) do
      :all ->
        :all

      list when is_list(list) ->
        no_warn_undefined ++ list
    end
  end

  ## Warning helpers

  def group_warnings(warnings) do
    warnings
    |> Enum.reduce(%{}, fn {module, warning, location}, acc ->
      locations = MapSet.new([location])
      Map.update(acc, {module, warning}, locations, &MapSet.put(&1, location))
    end)
    |> Enum.map(fn {{module, warning}, locations} -> {module, warning, Enum.sort(locations)} end)
    |> Enum.sort()
  end

  def emit_warnings(warnings) do
    Enum.flat_map(warnings, fn {module, warning, locations} ->
      message = module.format_warning(warning)
      print_warning([message, ?\n, format_locations(locations)])

      Enum.map(locations, fn {file, line, _mfa} ->
        {file, line, message}
      end)
    end)
  end

  defp format_locations([location]) do
    format_location(location)
  end

  defp format_locations(locations) do
    [
      "Found at #{length(locations)} locations:\n",
      Enum.map(locations, &format_location/1)
    ]
  end

  defp format_location({file, line, {module, fun, arity}}) do
    mfa = Exception.format_mfa(module, fun, arity)
    [format_file_line(file, line), ": ", mfa, ?\n]
  end

  defp format_location({file, line, nil}) do
    [format_file_line(file, line), ?\n]
  end

  defp format_location({file, line, module}) do
    [format_file_line(file, line), ": ", inspect(module), ?\n]
  end

  defp format_file_line(file, line) do
    file = Path.relative_to_cwd(file)
    line = if line > 0, do: [?: | Integer.to_string(line)], else: []
    ["  ", file, line]
  end

  defp print_warning(message) do
    IO.puts(:stderr, [:elixir_errors.warning_prefix(), message])
  end

  ## Server callbacks

  def init([modules, send_results, schedulers]) do
    ets = :ets.new(:checker_cache, [:set, :public, {:read_concurrency, true}])

    state = %{
      ets: ets,
      waiting: %{},
      send_results: send_results,
      modules: modules,
      spawned: 0,
      schedulers: schedulers
    }

    {:ok, state}
  end

  def handle_call({:lock, module}, from, %{waiting: waiting} = state) do
    case waiting do
      %{^module => froms} ->
        waiting = Map.put(state.waiting, module, [from | froms])
        {:noreply, %{state | waiting: waiting}}

      %{} ->
        waiting = Map.put(state.waiting, module, [])
        {:reply, true, %{state | waiting: waiting}}
    end
  end

  def handle_call({:unlock, module}, _from, %{waiting: waiting} = state) do
    froms = Map.fetch!(waiting, module)
    Enum.each(froms, &:gen_server.reply(&1, false))
    waiting = Map.delete(waiting, module)
    {:reply, :ok, %{state | waiting: waiting}}
  end

  def handle_call(:get_ets, _from, %{ets: ets} = state) do
    {:reply, ets, state}
  end

  def handle_cast(:start, state) do
    {:noreply, spawn_checkers(state)}
  end

  def handle_info({__MODULE__, :done}, state) do
    state = %{state | spawned: state.spawned - 1}

    if state.spawned == 0 and state.modules == [] do
      {:stop, :normal, state}
    else
      state = spawn_checkers(state)
      {:noreply, state}
    end
  end

  defp lock(server, module) do
    :gen_server.call(server, {:lock, module}, :infinity)
  end

  defp unlock(server, module) do
    :gen_server.call(server, {:unlock, module})
  end

  defp get_ets(server) do
    :gen_server.call(server, :get_ets)
  end

  defp start(server) do
    :gen_server.cast(server, :start)
  end

  defp preload_cache(ets, modules) do
    Enum.each(modules, fn
      {_module, map} when is_map(map) -> cache_from_module_map(ets, map)
      {module, binary} when is_binary(binary) -> cache_from_chunk(ets, module, binary)
    end)
  end

  defp spawn_checkers(%{modules: []} = state) do
    state
  end

  defp spawn_checkers(%{spawned: spawned, schedulers: schedulers} = state)
       when spawned >= schedulers do
    state
  end

  defp spawn_checkers(%{modules: [{module, _} = verify | modules]} = state) do
    parent = self()
    ets = state.ets
    send_results_pid = state.send_results

    spawn_link(fn ->
      warnings = check_module(verify, {parent, ets})
      send(send_results_pid, {__MODULE__, module, warnings})
      send(parent, {__MODULE__, :done})
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
    case :code.get_object_code(module) do
      {^module, binary, _filename} -> cache_from_chunk(ets, module, binary)
      _other -> false
    end
  end

  defp cache_from_chunk(ets, module, binary) do
    with {:ok, {_, [{'ExCk', chunk}]}} <- :beam_lib.chunks(binary, ['ExCk']),
         {:elixir_checker_v1, contents} <- :erlang.binary_to_term(chunk) do
      cache_chunk(ets, module, contents.exports)
      true
    else
      _ -> false
    end
  end

  defp cache_from_module_map(ets, map) do
    exports =
      [{{:__info__, 1}, :def}] ++
        behaviour_exports(map) ++
        definitions_to_exports(map.definitions)

    deprecated = Map.new(map.deprecated)
    cache_info(ets, map.module, exports, deprecated)
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
    Map.new(
      [{{:__info__, 1}, :def}] ++
        behaviour_exports(module) ++
        Enum.map(module.__info__(:macros), &{&1, :defmacro}) ++
        Enum.map(module.__info__(:functions), &{&1, :def})
    )
  rescue
    _ -> Map.new(Enum.map(module.module_info(:exports), &{&1, :def}))
  end

  defp info_deprecated(module) do
    Map.new(module.__info__(:deprecated))
  rescue
    _ -> %{}
  end

  defp cache_info(ets, module, exports, deprecated) do
    exports =
      Enum.map(exports, fn {{fun, arity}, kind} ->
        reason = Map.get(deprecated, {fun, arity})
        :ets.insert(ets, {{:export, {module, fun, arity}}, kind, reason})

        {{fun, arity}, kind}
      end)

    :ets.insert(ets, {{:all_exports, module}, exports})
    :ets.insert(ets, {{:cached, module}, true})
  end

  defp cache_chunk(ets, module, exports) do
    exports =
      Enum.map(exports, fn {{fun, arity}, %{kind: kind, deprecated_reason: reason}} ->
        :ets.insert(ets, {{:export, {module, fun, arity}}, kind, reason})

        {{fun, arity}, kind}
      end)

    :ets.insert(ets, {{:export, {module, :__info__, 1}}, :def, nil})
    exports = [{{:__info__, 1}, :def} | exports]

    :ets.insert(ets, {{:all_exports, module}, exports})
    :ets.insert(ets, {{:cached, module}, true})
  end

  defp behaviour_exports(%{is_behaviour: true}), do: [{{:behaviour_info, 1}, :def}]
  defp behaviour_exports(%{is_behaviour: false}), do: []

  defp behaviour_exports(module) when is_atom(module) do
    if {:behaviour_info, 1} in module.module_info(:functions) do
      [{{:behaviour_info, 1}, :def}]
    else
      []
    end
  end

  defp definitions_to_exports(definitions) do
    Enum.flat_map(definitions, fn {function, kind, _meta, _clauses} ->
      if kind in [:def, :defmacro] do
        [{function, kind}]
      else
        []
      end
    end)
  end
end
