defmodule Logger.Backends.Capture do
  @moduledoc false

  use GenEvent

  def init({device, opts}) do
    {:ok, configure([], device, opts)}
  end

  def handle_call({:configure, options}, state) do
    {:ok, :ok, configure(state.events, state.device, options)}
  end

  def handle_event({_level, gl, _event}, %{device: device} = state)
  when gl != device and device != nil do
    {:ok, state}
  end

  def handle_event({level, _gl, {Logger, msg, ts, md}}, state) do
    if match_level?(level, state.level) do
      log_event(level, msg, ts, md, state)
    else
      {:ok, state}
    end
  end

  def terminate(:get, state) do
    {:ok, state.events}
  end

  def terminate(_reason, _state),
    do: :ok

  defp match_level?(_lvl, nil),
    do: true

  defp match_level?(lvl, min) do
    Logger.compare_levels(lvl, min) != :lt
  end

  ## Helpers

  defp configure(events, device, options) do
    config =
      Application.get_env(:logger, :capture, [])
      |> configure_merge(options)

    format =
      Keyword.get(config, :format)
      |> Logger.Formatter.compile
    level    = Keyword.get(config, :level)
    metadata = Keyword.get(config, :metadata, [])
    colors   = configure_colors(config)
    %{format: format, metadata: metadata,
      level: level, colors: colors,
      events: events, device: device}
  end

  defp configure_merge(env, options) do
    Keyword.merge(env, options, fn
      :colors, v1, v2 -> Keyword.merge(v1, v2)
      _key, _v1, v2 -> v2
    end)
  end

  defp configure_colors(config) do
    colors = Keyword.get(config, :colors, [])
    %{debug: Keyword.get(colors, :debug, :cyan),
      info: Keyword.get(colors, :info, :normal),
      warn: Keyword.get(colors, :warn, :yellow),
      error: Keyword.get(colors, :error, :red),
      enabled: Keyword.get(colors, :enabled, IO.ANSI.enabled?)}
  end

  defp log_event(level, msg, ts, md, %{colors: colors, events: acc} = state) do
    message =
      format_event(level, msg, ts, md, state)
      |> color_event(level, colors)
    {:ok, %{state | events: [acc | message]}}
  end

  defp format_event(level, msg, ts, md, %{format: format, metadata: metadata}) do
    Logger.Formatter.format(format, level, msg, ts, Dict.take(md, metadata))
  end

  defp color_event(data, _level, %{enabled: false}), do: data

  defp color_event(data, level, %{enabled: true} = colors) do
    [IO.ANSI.format_fragment(Map.fetch!(colors, level), true), data | IO.ANSI.reset]
  end
end
