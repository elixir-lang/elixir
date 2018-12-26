defmodule Logger.ErlangHandler do
  @moduledoc false

  @doc """
  Hook required by `:logger`.
  """
  def log(%{meta: %{domain: [:otp, :sasl | _]}}, %{sasl_reports?: false}) do
    :ok
  end

  def log(%{meta: %{domain: [:supervisor_report]}}, %{sasl_reports?: false}) do
    :ok
  end

  def log(%{level: level, msg: msg, meta: erl_meta}, _config) do
    level = erlang_level_to_elixir_level(level)

    Logger.bare_log(level, fn ->
      try do
        meta = extract_metadata(erl_meta)

        case msg do
          {:string, string} ->
            {string, meta}

          {:report, %{label: label, report: report} = complete} when map_size(complete) == 2 ->
            translate(level, :report, {label, report}, meta, erl_meta)

          {:report, %{label: {:error_logger, _}, format: format, args: args}} ->
            translate(level, :format, {format, args}, meta, erl_meta)

          {:report, report} ->
            translate(level, :report, {:logger, report}, meta, erl_meta)

          {format, args} ->
            translate(level, :format, {format, args}, meta, erl_meta)
        end
      rescue
        e ->
          [
            "Failure while translating Erlang's logger event\n",
            Exception.format(:error, e, __STACKTRACE__)
          ]
      end
    end)
  end

  defp erlang_level_to_elixir_level(:emergency), do: :error
  defp erlang_level_to_elixir_level(:alert), do: :error
  defp erlang_level_to_elixir_level(:critical), do: :error
  defp erlang_level_to_elixir_level(:error), do: :error
  defp erlang_level_to_elixir_level(:warning), do: :warn
  defp erlang_level_to_elixir_level(:notice), do: :info
  defp erlang_level_to_elixir_level(:info), do: :info
  defp erlang_level_to_elixir_level(:debug), do: :debug

  defp extract_metadata(map) do
    metadata = []

    metadata =
      case map do
        %{mfa: {mod, fun, arity}} -> [module: mod, function: form_fa(fun, arity)] ++ metadata
        _ -> metadata
      end

    metadata =
      case map do
        %{file: file, line: line} -> [file: List.to_string(file), line: line] ++ metadata
        _ -> metadata
      end

    metadata =
      case map do
        %{pid: pid} -> [pid: pid] ++ metadata
        _ -> metadata
      end

    metadata
  rescue
    _ -> []
  end

  defp form_fa(fun, arity) do
    Atom.to_string(fun) <> "/" <> Integer.to_string(arity)
  end

  @doc """
  Shared translation convenience.
  """
  def translate(level, kind, data, meta, erl_meta) do
    %{
      level: min_level,
      truncate: truncate,
      translators: translators
    } = Logger.Config.translation_data()

    case translate(translators, min_level, level, kind, data, meta) do
      :none -> {translate_fallback(kind, data, erl_meta, truncate), meta}
      other -> other
    end
  end

  defp translate([{mod, fun} | t], min_level, level, kind, data, meta) do
    case apply(mod, fun, [min_level, level, kind, data]) do
      {:ok, chardata, transdata} -> {chardata, Keyword.merge(meta, transdata)}
      {:ok, chardata} -> {chardata, meta}
      :skip -> :skip
      :none -> translate(t, min_level, level, kind, data, meta)
    end
  end

  defp translate([], _min_level, _level, _kind, _data, _meta) do
    :none
  end

  defp translate_fallback(:report, {:logger, data}, %{report_cb: callback} = meta, truncate) do
    translate_fallback(:format, callback.(data), meta, truncate)
  end

  defp translate_fallback(:format, {format, args}, _meta, truncate) do
    format
    |> Logger.Utils.scan_inspect(args, truncate)
    |> :io_lib.build_text()
  end

  defp translate_fallback(:report, {_type, %{} = data}, _meta, _truncate) do
    Kernel.inspect(Map.to_list(data))
  end

  defp translate_fallback(:report, {_type, data}, _meta, _truncate) do
    Kernel.inspect(data)
  end
end
