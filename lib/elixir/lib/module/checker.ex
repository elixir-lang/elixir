defmodule Module.Checker do
  @moduledoc false

  defmodule Coordinator do
    @moduledoc false

    def start_link(maps) do
      ets = :ets.new(:checker_cache, [:set, :public])

      Enum.each(maps, fn map ->
        exports = definitions_to_exports(map.definitions)
        deprecated = :maps.from_list(map.deprecated)
        cache_module(ets, map.module, exports, deprecated)
      end)

      {:ok, server} = :gen_server.start_link(__MODULE__, [], [])
      {server, ets}
    end

    def init([]) do
      {:ok, %{waiting: %{}}}
    end

    def stop({server, ets}) do
      :ets.delete(ets)
      :gen_server.stop(server)
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

    defp lock(server, module) do
      :gen_server.call(server, {:lock, module}, :infinity)
    end

    defp unlock(server, module) do
      :gen_server.call(server, {:unlock, module})
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
        cache_module(ets, module, map.exports, map.deprecated)
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

  def create_cache(maps), do: Coordinator.start_link(maps)
  def delete_cache(cache), do: Coordinator.stop(cache)

  def verify(module_map, binary, cache) do
    warnings = warnings(module_map, cache)
    binary = chunk(binary, module_map)
    {binary, warnings}
  end

  defp chunk(binary, module_map) do
    checker_chunk = build_chunk(module_map)
    {:ok, _module, all_chunks} = :beam_lib.all_chunks(binary)
    {:ok, binary} = :beam_lib.build_module([checker_chunk | all_chunks])
    binary
  end

  defp build_chunk(module_map) do
    map = %{
      deprecated: :maps.from_list(module_map.deprecated),
      exports: :maps.from_list(Coordinator.definitions_to_exports(module_map.definitions))
    }

    {'ExCk', :erlang.term_to_binary({:elixir_checker_v1, map})}
  end

  defp warnings(module_map, cache) do
    state = %{
      cache: cache,
      file: module_map.file,
      module: module_map.module,
      compile_opts: module_map.compile_opts,
      function: nil,
      warnings: []
    }

    state =
      module_map.definitions
      |> check_definitions(state)

    state.warnings
    |> merge_warnings()
    |> sort_warnings()
    |> emit_warnings()
  end

  defp check_definitions(definitions, state) do
    Enum.reduce(definitions, state, &check_definition/2)
  end

  defp check_definition({function, _kind, meta, clauses}, state) do
    with_file_meta(%{state | function: function}, meta, fn state ->
      Enum.reduce(clauses, state, &check_clause/2)
    end)
  end

  defp with_file_meta(%{file: original_file} = state, meta, fun) do
    case Keyword.fetch(meta, :file) do
      {:ok, {meta_file, _}} ->
        state = fun.(%{state | file: meta_file})
        %{state | file: original_file}

      :error ->
        fun.(state)
    end
  end

  defp check_clause({_meta, args, _guards, body}, state) do
    state = check_expr(args, state)
    check_expr(body, state)
  end

  # &Mod.fun/arity
  defp check_expr({:&, meta, [{:/, _, [{{:., _, [module, fun]}, _, []}, arity]}]}, state)
       when is_atom(module) and is_atom(fun) do
    check_remote(module, fun, arity, meta, state)
  end

  # Mod.fun(...)
  defp check_expr({{:., meta, [module, fun]}, _, args}, state)
       when is_atom(module) and is_atom(fun) do
    check_remote(module, fun, length(args), meta, state)
  end

  # %Module{...}
  defp check_expr({:%, meta, [module, {:%{}, _meta, args}]}, state)
       when is_atom(module) and is_list(args) do
    state = check_remote(module, :__struct__, 0, meta, state)
    check_expr(args, state)
  end

  # Function call
  defp check_expr({left, _meta, right}, state) when is_list(right) do
    state = check_expr(right, state)
    check_expr(left, state)
  end

  # {x, y}
  defp check_expr({left, right}, state) do
    state = check_expr(right, state)
    check_expr(left, state)
  end

  # [...]
  defp check_expr(list, state) when is_list(list) do
    Enum.reduce(list, state, &check_expr/2)
  end

  defp check_expr(_other, state) do
    state
  end

  defp check_remote(module, fun, arity, meta, state) do
    # TODO: In the future we may want to warn for modules defined
    # in the local context
    if Keyword.get(meta, :context_module, false) and state.module != module do
      state
    else
      Coordinator.preload_module(state.cache, module)
      check_export(module, fun, arity, meta, state)
    end
  end

  defp check_export(module, fun, arity, meta, state) do
    case Coordinator.fetch_export(state.cache, module, fun, arity) do
      {:ok, _} ->
        check_deprecated(module, fun, arity, meta, state)

      {:error, :module} ->
        if warn_undefined?(module, fun, arity, state) do
          warn(meta, state, {:undefined_module, module, fun, arity})
        else
          state
        end

      {:error, :function} ->
        if warn_undefined?(module, fun, arity, state) do
          exports = Coordinator.all_exports(state.cache, module)
          warn(meta, state, {:undefined_function, module, fun, arity, exports})
        else
          state
        end
    end
  end

  defp check_deprecated(module, fun, arity, meta, state) do
    case Coordinator.fetch_deprecated(state.cache, module, fun, arity) do
      {:ok, reason} ->
        warn(meta, state, {:deprecated, module, fun, arity, reason})

      :error ->
        state
    end
  end

  # TODO: Do not warn inside guards
  # TODO: Properly handle protocols
  # TODO: Properly handle __info__
  defp warn_undefined?(_module, :__impl__, 1, _state), do: false
  defp warn_undefined?(_module, :__info__, 1, _state), do: false
  defp warn_undefined?(:erlang, :orelse, 2, _state), do: false
  defp warn_undefined?(:erlang, :andalso, 2, _state), do: false

  defp warn_undefined?(module, fun, arity, state) do
    # TODO: Code.compiler_options[:no_warn_undefined]
    for(
      {:no_warn_undefined, values} <- state.compile_opts,
      value <- List.wrap(values),
      value == module or value == {module, fun, arity},
      do: :skip
    ) == []
  end

  defp warn(meta, state, warning) do
    {fun, arity} = state.function
    location = {state.file, meta[:line], {state.module, fun, arity}}
    %{state | warnings: [{warning, location} | state.warnings]}
  end

  defp merge_warnings(warnings) do
    Enum.reduce(warnings, %{}, fn {warning, location}, acc ->
      locations = MapSet.new([location])
      Map.update(acc, warning, locations, &MapSet.put(&1, location))
    end)
  end

  defp sort_warnings(warnings) do
    warnings
    |> Enum.map(fn {warning, locations} -> {warning, Enum.sort(locations)} end)
    |> Enum.sort()
  end

  defp emit_warnings(warnings) do
    Enum.flat_map(warnings, fn {warning, locations} ->
      message = format_warning(warning)
      print_warning([message, ?\n, format_locations(locations)])

      Enum.map(locations, fn {file, line, _mfa} ->
        {file, line, message}
      end)
    end)
  end

  defp format_warning({:undefined_module, module, fun, arity}) do
    [
      Exception.format_mfa(module, fun, arity),
      " is undefined (module ",
      inspect(module),
      " is not available or is yet to be defined)"
    ]
  end

  defp format_warning({:undefined_function, module, fun, arity, exports}) do
    [
      Exception.format_mfa(module, fun, arity),
      " is undefined or private",
      UndefinedFunctionError.hint_for_loaded_module(module, fun, arity, exports)
    ]
  end

  defp format_warning({:deprecated, module, function, arity, reason}) do
    [
      Exception.format_mfa(module, function, arity),
      " is deprecated. ",
      reason
    ]
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
    file = Path.relative_to_cwd(file)
    line = if line, do: [Integer.to_string(line), ": "], else: []
    mfa = Exception.format_mfa(module, fun, arity)
    ["  ", file, ?:, line, mfa, ?\n]
  end

  defp print_warning(message) do
    IO.puts(:stderr, [:elixir_errors.warning_prefix(), message])
  end
end
