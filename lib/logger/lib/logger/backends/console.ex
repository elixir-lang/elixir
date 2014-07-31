defmodule Logger.Backends.Console do
  @moduledoc false

  use GenEvent

  def init(_) do
    if user = Process.whereis(:user) do
      Process.group_leader(self(), user)
      {:ok, configure([])}
    else
      {:error, :ignore}
    end
  end

  def handle_call({:configure, options}, _state) do
    {:ok, :ok, configure(options)}
  end

  def handle_event({_level, gl, _event}, state) when node(gl) != node() do
    {:ok, state}
  end

  def handle_event({level, _gl, {Logger, msg, ts, md}}, %{level: min_level} = state) do
    if nil?(min_level) or Logger.compare_levels(level, min_level) != :lt do
      log_event(level, msg, ts, md, state)
    end
    {:ok, state}
  end

  ## Helpers

  defp configure(options) do
    env = Application.get_env(:logger, :console, [])
    console = configure_merge(env, options)
    Application.put_env(:logger, :console, console)

    format = console
      |> Keyword.get(:format)
      |> Logger.Formatter.compile

    level    = Keyword.get(console, :level)
    metadata = Keyword.get(console, :metadata, [])
    colors   = configure_colors(console)
    %{format: format, metadata: metadata, level: level, colors: colors}
  end

  defp configure_merge(env, options) do
    env_colors = Keyword.get(env, :colors, [])
    opts_colors = Keyword.get(options, :colors, [])
    colors = Keyword.merge(env_colors, opts_colors)

    Keyword.merge(env, options)
    |> Keyword.put(:colors, colors)
  end

  defp configure_colors(console) do
    colors  = Keyword.get(console, :colors, [])
    debug   = Keyword.get(colors, :debug, :magenta)
    info    = Keyword.get(colors, :info, :normal)
    warn    = Keyword.get(colors, :warn, :yellow)
    error   = Keyword.get(colors, :error, :red)
    enabled = Keyword.get(colors, :enabled, false)
    %{debug: debug, info: info, warn: warn, error: error, enabled: enabled}
  end

  defp log_event(level, msg, ts, md, %{colors: colors} = state) do
    ansidata = format_event(level, msg, ts, md, state)
    chardata = color_event(level, ansidata, colors)
    :io.put_chars(:user, chardata)
  end

  defp format_event(level, msg, ts, md, %{format: format, metadata: metadata}) do
    Logger.Formatter.format(format, level, msg, ts, Dict.take(md, metadata))
  end

  defp color_event(level, data, %{enabled: enabled} = colors) do
    IO.ANSI.format([Map.fetch!(colors, level) | data], enabled)
  end

end
