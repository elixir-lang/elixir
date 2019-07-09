defmodule Module.Checker do
  alias Module.ParallelChecker

  @moduledoc false

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
    exports = ParallelChecker.definitions_to_exports(module_map.definitions)
    deprecated = :maps.from_list(module_map.deprecated)

    map =
      Enum.map(exports, fn {function, kind} ->
        reason = :maps.get(function, deprecated, nil)
        {function, {kind, reason}}
      end)

    {'ExCk', :erlang.term_to_binary({:elixir_checker_v1, :maps.from_list(map)})}
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
      ParallelChecker.preload_module(state.cache, module)
      check_export(module, fun, arity, meta, state)
    end
  end

  defp check_export(module, fun, arity, meta, state) do
    case ParallelChecker.fetch_export(state.cache, module, fun, arity) do
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
          exports = ParallelChecker.all_exports(state.cache, module)
          warn(meta, state, {:undefined_function, module, fun, arity, exports})
        else
          state
        end
    end
  end

  defp check_deprecated(module, fun, arity, reason, meta, state) do
    if reason do
      warn(meta, state, {:deprecated, module, fun, arity, reason})
    else
      state
    end
  end

  # TODO: Do not warn inside guards
  # TODO: Properly handle protocols
  defp warn_undefined?(_module, :__impl__, 1, _state), do: false
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
