defmodule Logger.Handler do
  @moduledoc false

  @internal_keys [:counter]

  ## Conversions

  # TODO: Remove this mapping once we remove old Logger Backends (v2.0)
  defp erlang_level_to_elixir_level(:none), do: :error
  defp erlang_level_to_elixir_level(:emergency), do: :error
  defp erlang_level_to_elixir_level(:alert), do: :error
  defp erlang_level_to_elixir_level(:critical), do: :error
  defp erlang_level_to_elixir_level(:error), do: :error
  defp erlang_level_to_elixir_level(:warning), do: :warn
  defp erlang_level_to_elixir_level(:notice), do: :info
  defp erlang_level_to_elixir_level(:info), do: :info
  defp erlang_level_to_elixir_level(:debug), do: :debug
  defp erlang_level_to_elixir_level(:all), do: :debug

  # TODO: Warn on deprecated level on v1.15
  def elixir_level_to_erlang_level(:warn), do: :warning
  def elixir_level_to_erlang_level(other), do: other

  ## Config management

  def adding_handler(config) do
    {:ok, update_in(config.config, &Map.merge(default_config(), &1))}
  end

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
    sasl_reports? = Application.fetch_env!(:logger, :handle_sasl_reports)

    %{
      utc_log: Application.fetch_env!(:logger, :utc_log),
      truncate: Application.fetch_env!(:logger, :truncate),
      translators: Application.fetch_env!(:logger, :translators),
      thresholds: {sync_threshold, discard_threshold},
      sasl: sasl_reports?
    }
  end

  ## Main logging API

  def log(%{meta: %{domain: [:otp, :sasl | _]}}, %{config: %{sasl: false}}), do: :ok
  def log(%{meta: %{domain: [:supervisor_report | _]}}, %{config: %{sasl: false}}), do: :ok

  def log(%{level: erl_level, msg: msg, meta: metadata}, %{config: config}) do
    case threshold(config) do
      :discard ->
        :ok

      mode ->
        level = erlang_level_to_elixir_level(erl_level)

        case do_log(level, msg, metadata, config) do
          :skip ->
            :ok

          {message, %{gl: gl} = metadata} ->
            timestamp = Map.get_lazy(metadata, :time, fn -> :os.system_time(:microsecond) end)
            metadata = [erl_level: erl_level] ++ erlang_metadata_to_elixir_metadata(metadata)

            %{truncate: truncate, utc_log: utc_log?} = config

            event = {
              level,
              gl,
              {Logger, truncate(message, truncate), Logger.Utils.timestamp(timestamp, utc_log?),
               metadata}
            }

            notify(mode, event)
        end
    end
  rescue
    ArgumentError -> {:error, :noproc}
  catch
    :exit, reason -> {:error, reason}
  end

  defp do_log(_level, {:string, message}, metadata, _config) do
    {message, metadata}
  end

  defp do_log(level, msg, metadata, config) do
    %{level: erl_min_level} = :logger.get_primary_config()
    min_level = erlang_level_to_elixir_level(erl_min_level)
    %{truncate: truncate} = config

    try do
      case msg do
        {:report, %{label: label, report: report} = complete}
        when map_size(complete) == 2 ->
          translate(level, :report, {label, report}, metadata, config, min_level)

        {:report, %{label: {:error_logger, _}, format: format, args: args}} ->
          translate(level, :format, {format, args}, metadata, config, min_level)

        {:report, report} ->
          translate(level, :report, {:logger, report}, metadata, config, min_level)

        {format, args} ->
          translate(level, :format, {format, args}, metadata, config, min_level)
      end
    rescue
      e ->
        {[
           "Failure while translating Erlang's logger event\n",
           Exception.format(:error, e, __STACKTRACE__)
         ], metadata}
    else
      :none -> {translate_fallback(msg, metadata, truncate), metadata}
      other -> other
    end
  end

  defp notify(:sync, msg) do
    pid = Process.whereis(Logger)

    # If we are within the logger process itself,
    #  we cannot use sync notify as that will deadlock.
    if pid == self(), do: :gen_event.notify(pid, msg), else: :gen_event.sync_notify(pid, msg)
  end

  defp notify(:async, msg) do
    :gen_event.notify(Logger, msg)
  end

  defp truncate(data, n) when is_list(data) do
    Logger.Utils.truncate(data, n)
  rescue
    msg in ArgumentError ->
      Exception.message(msg)
  end

  defp truncate(data, n) when is_binary(data), do: Logger.Utils.truncate(data, n)
  defp truncate(data, n), do: Logger.Utils.truncate(to_string(data), n)

  @counter_pos 1

  defp threshold(config) do
    %{
      counter: counter,
      thresholds: {sync, discard}
    } = config

    :counters.add(counter, @counter_pos, 1)
    value = :counters.get(counter, @counter_pos)

    cond do
      value >= discard -> :discard
      value >= sync -> :sync
      true -> :async
    end
  end

  ## Metadata helpers

  # TODO: We should only do this for legacy handlers.
  # The new handlers should accept all metadata as is
  # and receive the system time unit rather than tuples.
  # The new handlers should also receive structured
  # logging events as is.
  defp erlang_metadata_to_elixir_metadata(metadata) do
    metadata =
      case metadata do
        %{mfa: {mod, fun, arity}} ->
          Map.merge(%{module: mod, function: form_fa(fun, arity)}, metadata)

        %{} ->
          metadata
      end

    metadata =
      case metadata do
        %{file: file} -> %{metadata | file: List.to_string(file)}
        %{} -> metadata
      end

    Map.to_list(metadata)
  rescue
    _ -> Map.to_list(metadata)
  end

  defp form_fa(fun, arity) do
    Atom.to_string(fun) <> "/" <> Integer.to_string(arity)
  end

  ## Translating helpers

  defp translate(level, kind, data, meta, config, min_level) do
    %{translators: translators} = config

    do_translate(translators, min_level, level, kind, data, meta)
  end

  defp do_translate([{mod, fun} | t], min_level, level, kind, data, meta) do
    case apply(mod, fun, [min_level, level, kind, data]) do
      {:ok, chardata, transdata} -> {chardata, Enum.into(transdata, meta)}
      {:ok, chardata} -> {chardata, meta}
      :skip -> :skip
      :none -> do_translate(t, min_level, level, kind, data, meta)
    end
  end

  defp do_translate([], _min_level, _level, _kind, _data, _meta) do
    :none
  end

  defp translate_fallback({:report, data}, %{report_cb: callback} = meta, truncate)
       when is_function(callback, 1) do
    translate_fallback(callback.(data), meta, truncate)
  end

  defp translate_fallback({:report, data}, %{report_cb: callback}, _truncate)
       when is_function(callback, 2) do
    translator_opts =
      struct(Inspect.Opts, Application.fetch_env!(:logger, :translator_inspect_opts))

    opts = %{
      depth: translator_opts.limit,
      chars_limit: translator_opts.printable_limit,
      single_line: false
    }

    callback.(data, opts)
  end

  defp translate_fallback({:report, %{} = data}, _meta, _truncate) do
    Kernel.inspect(Map.to_list(data))
  end

  defp translate_fallback({:report, data}, _meta, _truncate) do
    Kernel.inspect(data)
  end

  defp translate_fallback({format, args}, _meta, truncate) do
    format
    |> Logger.Utils.scan_inspect(args, truncate)
    |> :io_lib.build_text()
  end
end
