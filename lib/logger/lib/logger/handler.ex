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

  def elixir_level_to_erlang_level(:warn) do
    IO.warn("the log level :warn is deprecated, use :warning instead")
    :warning
  end

  def elixir_level_to_erlang_level(other), do: other

  ## Config management

  def adding_handler(config) do
    {:ok, update_in(config.config, &Map.merge(default_config(), &1))}
  end

  def changing_config(:update, config, %{config: :refresh}) do
    {:ok, update_in(config.config, &Map.merge(&1, default_config()))}
  end

  def filter_config(%{config: data} = config) do
    %{config | config: Map.drop(data, @internal_keys)}
  end

  defp default_config do
    sync_threshold = Application.fetch_env!(:logger, :sync_threshold)
    discard_threshold = Application.fetch_env!(:logger, :discard_threshold)

    %{
      utc_log: Application.fetch_env!(:logger, :utc_log),
      truncate: Application.fetch_env!(:logger, :truncate),
      thresholds: {sync_threshold, discard_threshold}
    }
  end

  ## Main logging API

  def log(%{level: erl_level, msg: msg, meta: metadata}, %{config: config}) do
    case threshold(config) do
      :discard ->
        :ok

      mode ->
        level = erlang_level_to_elixir_level(erl_level)
        message = format_message(msg, metadata, config)

        %{gl: gl} = metadata
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
  rescue
    ArgumentError -> {:error, :noproc}
  catch
    :exit, reason -> {:error, reason}
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

  ## Message formatting

  defp format_message({:string, message}, _metadata, _config) do
    message
  end

  defp format_message({:report, data}, %{report_cb: callback} = meta, config)
       when is_function(callback, 1) do
    format_message(callback.(data), meta, config)
  end

  defp format_message({:report, data}, %{report_cb: callback}, _config)
       when is_function(callback, 2) do
    translator_opts = Inspect.Opts.new(translator_inspect_opts())

    opts = %{
      depth: translator_opts.limit,
      chars_limit: translator_opts.printable_limit,
      single_line: false
    }

    callback.(data, opts)
  end

  defp format_message({:report, %{} = data}, _meta, _config) do
    Kernel.inspect(Map.to_list(data), translator_inspect_opts())
  end

  defp format_message({:report, data}, _meta, _config) do
    Kernel.inspect(data, translator_inspect_opts())
  end

  defp format_message({format, args}, _meta, %{truncate: truncate}) do
    format
    |> Logger.Utils.scan_inspect(args, truncate)
    |> :io_lib.build_text()
  end

  defp translator_inspect_opts() do
    Application.fetch_env!(:logger, :translator_inspect_opts)
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
end
