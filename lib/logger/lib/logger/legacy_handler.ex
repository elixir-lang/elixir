defmodule Logger.LegacyHandler do
  @moduledoc false
  alias Logger.Counter

  @internal_keys [:counter]

  ## API used by other Logger modules

  def load_log_level do
    # TODO: Deprecate if level is warning.
    # PENDING: Fallback to kernel.logger_level if logger level is not set.
    level =
      case Application.fetch_env!(:logger, :level) do
        :warn -> :warning
        level -> level
      end

    :ok = :logger.set_primary_config(:level, level)
  end

  ## Config management

  def adding_handler(config) do
    {:ok, update_in(config.config, &Map.merge(default_config(), &1))}
  end

  # TODO: Remove when we will support OTP 22+
  def changing_config(current, new), do: changing_config(:set, current, new)

  def changing_config(
        op,
        %{config: %{counter: counter} = old_data} = old_config,
        %{config: new_data} = new_config
      ) do
    old_data =
      case op do
        :set -> default_config()
        :update -> old_data
      end

    data =
      Enum.reduce(new_data, old_data, fn {k, v}, acc ->
        case acc do
          %{^k => _} -> %{acc | k => v}
          %{} -> acc
        end
      end)

    config = Map.merge(old_config, new_config)
    {:ok, Map.put(config, :config, Map.put(data, :counter, counter))}
  end

  def filter_config(%{config: data} = config) do
    %{config | config: Map.drop(data, @internal_keys)}
  end

  defp default_config do
    sync_threshold = Application.fetch_env!(:logger, :sync_threshold)
    discard_threshold = Application.fetch_env!(:logger, :discard_threshold)
    level = Application.fetch_env!(:logger, :level)
    sasl_reports? = Application.fetch_env!(:logger, :handle_sasl_reports)

    %{
      level: level,
      utc_log: Application.fetch_env!(:logger, :utc_log),
      truncate: Application.fetch_env!(:logger, :truncate),
      translators: Application.fetch_env!(:logger, :translators),
      thresholds: {sync_threshold, discard_threshold},
      sasl: sasl_reports?
    }
  end

  ## Main logging API

  def log(%{meta: %{domain: [:otp, :sasl | _]}}, %{config: %{sasl: false}}) do
    :ok
  end

  def log(%{meta: %{domain: [:supervisor_report | _]}}, %{config: %{sasl: false}}) do
    :ok
  end

  def log(%{level: erl_level, msg: msg, meta: erl_meta}, %{id: _id, config: config}) do
    level = erlang_level_to_elixir_level(erl_level)
    %{utc_log: utc_log?, truncate: truncate, level: erl_min_level} = config
    min_level = erlang_level_to_elixir_level(erl_min_level)

    case threshold(config) do
      :discard ->
        :ok

      mode ->
        {erl_meta, rest} = Map.split(erl_meta, ~w[pid gl time mfa file line domain report_cb]a)
        meta = extract_metadata(erl_meta, Map.to_list(rest))

        translated =
          try do
            case msg do
              {:string, string} ->
                {string, meta}

              {:report, %{label: label, report: report} = complete}
              when map_size(complete) == 2 ->
                translate(level, :report, {label, report}, meta, erl_meta, config, min_level)

              {:report, %{label: {:error_logger, _}, format: format, args: args}} ->
                translate(level, :format, {format, args}, meta, erl_meta, config, min_level)

              {:report, report} ->
                translate(level, :report, {:logger, report}, meta, erl_meta, config, min_level)

              {format, args} ->
                translate(level, :format, {format, args}, meta, erl_meta, config, min_level)
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
  rescue
    e ->
      :logger.error(
        fn ->
          "Unhandled error in Elixir's Logger handler.\n#{inspect(e)}"
        end,
        []
      )
  end

  defp erlang_level_to_elixir_level(:emergency), do: :error
  defp erlang_level_to_elixir_level(:alert), do: :error
  defp erlang_level_to_elixir_level(:critical), do: :error
  defp erlang_level_to_elixir_level(:error), do: :error
  defp erlang_level_to_elixir_level(:warning), do: :warn
  defp erlang_level_to_elixir_level(:notice), do: :info
  defp erlang_level_to_elixir_level(:info), do: :info
  defp erlang_level_to_elixir_level(:debug), do: :debug
  defp erlang_level_to_elixir_level(:all), do: :debug

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
  def translate(level, kind, data, meta, erl_meta, config, min_level) do
    %{
      truncate: truncate,
      translators: translators
    } = config

    case do_translate(translators, min_level, level, kind, data, meta) do
      :none -> {translate_fallback(kind, data, erl_meta, truncate), meta}
      other -> other
    end
  end

  defp do_translate([{mod, fun} | t], min_level, level, kind, data, meta) do
    case apply(mod, fun, [min_level, level, kind, data]) do
      {:ok, chardata, transdata} -> {chardata, Keyword.merge(meta, transdata)}
      {:ok, chardata} -> {chardata, meta}
      :skip -> :skip
      :none -> do_translate(t, min_level, level, kind, data, meta)
    end
  end

  defp do_translate([], _min_level, _level, _kind, _data, _meta) do
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

  def threshold(config) do
    %{
      counter: counter,
      thresholds: {sync, discard}
    } = config

    value = Counter.bump(counter)

    cond do
      value >= discard -> :discard
      value >= sync -> :sync
      true -> :async
    end
  end
end
