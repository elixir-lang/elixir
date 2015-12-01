defmodule Logger.Backends.Console do
  @moduledoc false

  use GenEvent

  def init(:console) do
    if Process.whereis(:user) do
      init({:user, []})
    else
      {:error, :ignore}
    end
  end

  def init({device, opts}) do
    {:ok, configure(device, opts)}
  end

  def handle_call({:configure, options}, state) do
    {:ok, :ok, configure(state.device, options)}
  end

  def handle_event({_level, gl, _event}, state)
  when node(gl) != node() do
    {:ok, state}
  end

  def handle_event({level, _gl, {Logger, msg, ts, md}}, state) do
    if meet_level?(level, state.level) do
      log_event(level, msg, ts, md, state)
    end
    {:ok, state}
  end

  ## Helpers

  defp meet_level?(_lvl, nil), do: true

  defp meet_level?(lvl, min) do
    Logger.compare_levels(lvl, min) != :lt
  end

  defp configure(device, options) do
    config =
      Application.get_env(:logger, :console, [])
      |> configure_merge(options)

    if device === :user do
      Application.put_env(:logger, :console, config)
    end

    format =
      Keyword.get(config, :format)
      |> Logger.Formatter.compile
    level    = Keyword.get(config, :level)
    metadata = Keyword.get(config, :metadata, [])
    colors   = configure_colors(config)
    %{format: format, metadata: metadata,
      level: level, colors: colors, device: device}
  end

  defp configure_merge(env, options) do
    Keyword.merge(env, options, fn
      :colors, v1, v2 -> Keyword.merge(v1, v2)
      _, _v1, v2 -> v2
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

  defp log_event(level, msg, ts, md, %{colors: colors} = state) do
    output =
      format_event(level, msg, ts, md, state)
      |> color_event(level, colors)
    IO.write(state.device, output)
  end

  defp format_event(level, msg, ts, md, %{format: format, metadata: keys}) do
    Logger.Formatter.format(format, level, msg, ts, take_metadata(md, keys))
  end

  defp take_metadata(metadata, keys) do
    List.foldr keys, [], fn key, acc ->
      case Keyword.fetch(metadata, key) do
        {:ok, val} -> [{key, val} | acc]
        :error     -> acc
      end
    end
  end

  defp color_event(data, _level, %{enabled: false}), do: data

  defp color_event(data, level, %{enabled: true} = colors) do
    [IO.ANSI.format_fragment(Map.fetch!(colors, level), true), data | IO.ANSI.reset]
  end
end
