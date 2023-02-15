defmodule Logger.Backends.Handler do
  @moduledoc false
  @internal_keys [:counter]

  def filesync(id) do
    :gen_event.sync_notify(id, :flush)
  end

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

  def log(%{level: erl_level, meta: metadata} = event, %{config: config}) do
    case threshold(config) do
      :discard ->
        :ok

      mode ->
        %{gl: gl} = metadata
        %{truncate: truncate, utc_log: utc_log?} = config
        level = erlang_level_to_elixir_level(erl_level)
        message = Logger.Formatter.format_event(event, truncate)
        timestamp = Map.get_lazy(metadata, :time, fn -> :os.system_time(:microsecond) end)
        date_time_ms = Logger.Formatter.system_time_to_date_time_ms(timestamp, utc_log?)
        metadata = [erl_level: erl_level] ++ erlang_metadata_to_elixir_metadata(metadata)
        event = {level, gl, {Logger, message, date_time_ms, metadata}}
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
end
