defmodule Module.Checker do
  def verify(module_map, _binary) do
    state = %{
      file: module_map.file,
      module: module_map.module,
      compile_opts: module_map.compile_opts,
      function: nil
    }

    module_map.definitions
    |> check_definitions(state)
    |> List.flatten()
    |> merge_warnings()
    |> sort_warnings()
    |> emit_warnings()
  end

  defp check_definitions(definitions, state) do
    Enum.map(definitions, &check_definition(&1, state))
  end

  defp check_definition({function, _kind, meta, clauses}, state) do
    state = put_file_meta(%{state | function: function}, meta)
    Enum.map(clauses, &check_clause(&1, state))
  end

  defp put_file_meta(state, meta) do
    case Keyword.fetch(meta, :file) do
      {:ok, {file, _}} -> %{state | file: file}
      :error -> state
    end
  end

  defp check_clause({_meta, _args, _guards, body}, state) do
    check_body(body, state)
  end

  # &Mod.fun/arity
  defp check_body({:&, meta, [{:/, _, [{{:., dot_meta, [module, fun]}, _, []}, arity]}]}, state)
       when is_atom(module) and is_atom(fun) do
    check_remote(module, fun, arity, meta ++ dot_meta, state)
  end

  # Mod.fun(...)
  defp check_body({{:., meta, [module, fun]}, _, args}, state)
       when is_atom(module) and is_atom(fun) do
    check_remote(module, fun, length(args), meta, state)
  end

  # Function call
  defp check_body({left, _meta, right}, state) when is_list(right) do
    [check_body(right, state), check_body(left, state)]
  end

  # {x, y}
  defp check_body({left, right}, state) do
    [check_body(right, state), check_body(left, state)]
  end

  # [...]
  defp check_body(list, state) when is_list(list) do
    Enum.map(list, &check_body(&1, state))
  end

  defp check_body(_other, _state) do
    []
  end

  defp check_remote(module, fun, arity, meta, state) do
    cond do
      not warn_if_undefined?(module, fun, arity, state) ->
        []

      # TODO: In the future we may want to warn for modules defined
      # in the local context
      Keyword.get(meta, :context_module, false) and state.module != module ->
        []

      # TODO: Add no_autoload
      not Code.ensure_loaded?(module) ->
        warn(meta, state, {:undefined_module, module, fun, arity})

      not function_exported?(module, fun, arity) ->
        exports = exports_for(module)
        warn(meta, state, {:undefined_function, module, fun, arity, exports})

      true ->
        []
    end
  end

  # TODO: Do not warn inside guards
  # TODO: Properly handle protocols
  defp warn_if_undefined?(_module, :__impl__, 1, _state), do: false
  defp warn_if_undefined?(:erlang, :orelse, 2, _state), do: false
  defp warn_if_undefined?(:erlang, :andalso, 2, _state), do: false

  defp warn_if_undefined?(module, fun, arity, state) do
    for(
      {:no_warn_undefined, values} <- state.compile_opts,
      value <- List.wrap(values),
      value == module or value == {module, fun, arity},
      do: :skip
    ) == []
  end

  defp exports_for(module) do
    try do
      module.__info__(:macros) ++ module.__info__(:functions)
    rescue
      _ -> module.module_info(:exports)
    end
  end

  defp warn(meta, %{file: file, module: module, function: {fun, arity}}, warning) do
    {warning, {file, meta[:line], {module, fun, arity}}}
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
    line = Integer.to_string(line)
    mfa = Exception.format_mfa(module, fun, arity)

    ["  ", file, ?:, line, ": ", mfa, ?\n]
  end

  defp print_warning(message) do
    IO.puts(:stderr, [:elixir_errors.warning_prefix(), message])
  end
end
