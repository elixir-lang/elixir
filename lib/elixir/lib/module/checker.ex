defmodule Module.Checker do
  @moduledoc false

  def verify(module, cache) do
    case prepare_module(module) do
      {:ok, map} ->
        Module.Types.infer_definitions(
          map.file,
          map.module,
          map.definitions,
          merge_no_warn_undefined(map),
          cache
        )
        |> group_warnings()
        |> emit_warnings()

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
    with {:ok, {_, [debug_info: chunk]}} <- :beam_lib.chunks(binary, [:debug_info]),
         {:debug_info_v1, backend, data} <- chunk,
         {:ok, module_map} <- backend.debug_info(:elixir_v1, module, data, []) do
      prepare_module({module, module_map})
    else
      _ -> :error
    end
  end

  defp no_warn_undefined(compile_opts) do
    for(
      {:no_warn_undefined, values} <- compile_opts,
      value <- List.wrap(values),
      do: value
    )
  end

  defp merge_no_warn_undefined(map) do
    case Code.get_compiler_option(:no_warn_undefined) do
      :all ->
        :all

      list when is_list(list) ->
        map.no_warn_undefined ++ list
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
end
