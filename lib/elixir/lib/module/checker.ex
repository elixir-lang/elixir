defmodule Module.Checker do
  alias Module.ParallelChecker

  @moduledoc false

  def verify(module, cache) do
    case prepare_module(module) do
      {:ok, map} ->
        undefined_and_deprecation_warnings = undefined_and_deprecation_warnings(map, cache)
        infer_warnings = Module.Types.infer_definitions(map.file, map.module, map.definitions)
        warnings = infer_warnings ++ undefined_and_deprecation_warnings
        emit_warnings(warnings)

      :error ->
        []
    end
  end

  defp prepare_module({module, module_map}) when is_map(module_map) do
    {:ok,
     %{
       module: module,
       file: module_map.file,
       definitions: module_map.definitions,
       deprecated: module_map.deprecated,
       no_warn_undefined: no_warn_undefined(module_map.compile_opts)
     }}
  end

  defp prepare_module({module, binary}) when is_binary(binary) do
    with {:ok, debug_info} <- debug_info(module, binary),
         {:ok, checker_info} <- checker_chunk(binary) do
      {:ok,
       %{
         module: module,
         file: debug_info.file,
         definitions: debug_info.definitions,
         deprecated: checker_info.deprecated,
         no_warn_undefined: checker_info.no_warn_undefined
       }}
    end
  end

  defp no_warn_undefined(compile_opts) do
    for(
      {:no_warn_undefined, values} <- compile_opts,
      value <- List.wrap(values),
      do: value
    )
  end

  defp debug_info(module, binary) do
    with {:ok, {_, [debug_info: chunk]}} <- :beam_lib.chunks(binary, [:debug_info]),
         {:debug_info_v1, backend, data} <- chunk,
         {:ok, info} <- backend.debug_info(:elixir_v1, module, data, []) do
      {:ok, %{definitions: info.definitions, file: info.relative_file}}
    else
      _ -> :error
    end
  end

  defp checker_chunk(binary) do
    with {:ok, {_, [{'ExCk', chunk}]}} <- :beam_lib.chunks(binary, ['ExCk']),
         {:elixir_checker_v1, contents} <- :erlang.binary_to_term(chunk) do
      deprecated = Enum.map(contents.exports, fn {fun, map} -> {fun, map.deprecated_reason} end)
      {:ok, %{deprecated: deprecated, no_warn_undefined: contents.no_warn_undefined}}
    else
      _ -> :error
    end
  end

  defp undefined_and_deprecation_warnings(map, cache) do
    state = %{
      cache: cache,
      file: map.file,
      module: map.module,
      no_warn_undefined: merge_no_warn_undefined(map),
      function: nil,
      warnings: []
    }

    state = check_definitions(map.definitions, state)

    state.warnings
    |> merge_warnings()
    |> sort_warnings()
  end

  defp merge_no_warn_undefined(map) do
    case Code.get_compiler_option(:no_warn_undefined) do
      :all ->
        :all

      list when is_list(list) ->
        map.no_warn_undefined ++ list
    end
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
    state = check_remote(module, fun, length(args), meta, state)
    check_expr(args, state)
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
      {:ok, :def, reason} ->
        check_deprecated(module, fun, arity, reason, meta, state)

      {:ok, :defmacro, reason} ->
        state = warn(meta, state, {:unrequired_module, module, fun, arity})
        check_deprecated(module, fun, arity, reason, meta, state)

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
  defp warn_undefined?(_module, :module_info, 0, _state), do: false
  defp warn_undefined?(_module, :module_info, 1, _state), do: false
  defp warn_undefined?(:erlang, :orelse, 2, _state), do: false
  defp warn_undefined?(:erlang, :andalso, 2, _state), do: false

  defp warn_undefined?(_, _, _, %{no_warn_undefined: :all}) do
    false
  end

  defp warn_undefined?(module, fun, arity, state) do
    not Enum.any?(state.no_warn_undefined, &(&1 == module or &1 == {module, fun, arity}))
  end

  defp warn(meta, state, warning) do
    {fun, arity} = state.function
    location = {state.file, meta[:line], {state.module, fun, arity}}
    %{state | warnings: [{__MODULE__, warning, location} | state.warnings]}
  end

  defp merge_warnings(warnings) do
    Enum.reduce(warnings, %{}, fn {module, warning, location}, acc ->
      locations = MapSet.new([location])
      Map.update(acc, {module, warning}, locations, &MapSet.put(&1, location))
    end)
  end

  defp sort_warnings(warnings) do
    warnings
    |> Enum.map(fn {{module, warning}, locations} -> {module, warning, Enum.sort(locations)} end)
    |> Enum.sort()
  end

  defp emit_warnings(warnings) do
    Enum.flat_map(warnings, fn {module, warning, locations} ->
      message = module.format_warning(warning)
      print_warning([message, ?\n, format_locations(locations)])

      Enum.map(locations, fn {file, line, _mfa} ->
        {file, line, message}
      end)
    end)
  end

  def format_warning({:undefined_module, module, fun, arity}) do
    [
      Exception.format_mfa(module, fun, arity),
      " is undefined (module ",
      inspect(module),
      " is not available or is yet to be defined)"
    ]
  end

  def format_warning({:undefined_function, module, fun, arity, exports}) do
    [
      Exception.format_mfa(module, fun, arity),
      " is undefined or private",
      UndefinedFunctionError.hint_for_loaded_module(module, fun, arity, exports)
    ]
  end

  def format_warning({:deprecated, module, fun, arity, reason}) do
    [
      Exception.format_mfa(module, fun, arity),
      " is deprecated. ",
      reason
    ]
  end

  def format_warning({:unrequired_module, module, fun, arity}) do
    [
      "you must require ",
      inspect(module),
      " before invoking the macro ",
      Exception.format_mfa(module, fun, arity)
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
