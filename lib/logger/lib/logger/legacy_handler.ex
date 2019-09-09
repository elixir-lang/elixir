defmodule Logger.LegacyHandler do
  @moduledoc false

  @doc false
  def child_spec(_) do
    %{
      id: :gen_event,
      start: {:gen_event, :start_link, [{:local, Logger}]},
      modules: :dynamic
    }
  end

  @doc """
  Hook required by `:logger`.
  """
  def log(%{level: erl_level, msg: msg, meta: erl_meta}, config) do
    level = erlang_level_to_elixir_level(erl_level)

    case Logger.Config.log_data(level) do
      {:discard, _config} ->
        :ok

      {mode, _config} ->
        %{utc_log: utc_log?, truncate: truncate} = config.config

        {erl_meta, rest} = Map.split(erl_meta, ~w[pid gl time mfa file line domain report_cb]a)
        meta = extract_metadata(erl_meta, Map.to_list(rest))

        translated =
          try do
            case msg do
              {:string, string} ->
                {string, meta}

              {:report, %{label: label, report: report} = complete}
              when map_size(complete) == 2 ->
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
              {[
                 "Failure while translating Erlang's logger event\n",
                 Exception.format(:error, e, __STACKTRACE__)
               ], meta}
          end

        case translated do
          :skip ->
            :ok

          {message, metadata} ->
            # TODO: Use `time` field of `erl_metadata` for timestamp
            tuple =
              {Logger, truncate(message, truncate), Logger.Utils.timestamp(utc_log?), metadata}

            try do
              notify(mode, {level, erl_meta.gl, tuple})
              :ok
            rescue
              ArgumentError -> {:error, :noproc}
            catch
              :exit, reason -> {:error, reason}
            end
        end
    end
  end

  defp erlang_level_to_elixir_level(:emergency), do: :error
  defp erlang_level_to_elixir_level(:alert), do: :error
  defp erlang_level_to_elixir_level(:critical), do: :error
  defp erlang_level_to_elixir_level(:error), do: :error
  defp erlang_level_to_elixir_level(:warning), do: :warn
  defp erlang_level_to_elixir_level(:notice), do: :info
  defp erlang_level_to_elixir_level(:info), do: :info
  defp erlang_level_to_elixir_level(:debug), do: :debug

  defp extract_metadata(map, metadata) do
    metadata = for {_k, v} = elem <- metadata, v != nil, do: elem

    metadata =
      case map do
        %{mfa: {mod, fun, arity}} ->
          metadata
          |> Keyword.put_new(:module, mod)
          |> Keyword.put_new(:function, form_fa(fun, arity))

        _ ->
          metadata
      end

    metadata =
      case map do
        %{file: file, line: line} -> [file: to_string(file), line: line] ++ metadata
        _ -> metadata
      end

    metadata =
      case map do
        %{pid: pid} -> [pid: pid] ++ metadata
        _ -> metadata
      end

    metadata
  rescue
    _ -> metadata
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

  defp notify(:sync, msg), do: :gen_event.sync_notify(Logger, msg)
  defp notify(:async, msg), do: :gen_event.notify(Logger, msg)

  defp truncate(data, n) when is_list(data) do
    Logger.Utils.truncate(data, n)
  rescue
    msg in ArgumentError ->
      Exception.message(msg)
  end

  defp truncate(data, n) when is_binary(data), do: Logger.Utils.truncate(data, n)
  defp truncate(data, n), do: Logger.Utils.truncate(to_string(data), n)
end
