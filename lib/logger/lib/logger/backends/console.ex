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
    {:ok, {configure(device, opts), %{}}}
  end

  def handle_call({:configure, options}, {state, refs}) do
    {:ok, :ok, {configure(state.device, options), refs}}
  end

  def handle_event({_level, gl, _event}, {_, _} = pair) when node(gl) != node() do
    {:ok, pair}
  end

  def handle_event({level, _gl, {Logger, msg, ts, md}}, {state, refs} = pair) do
    if meet_level?(level, state.level) do
      ref = log_event(level, msg, ts, md, state)
      {:ok, {state, Map.put(refs, ref, {ts, msg})}}
    else
      {:ok, pair}
    end
  end

  def handle_info({:io_reply, ref, status}, {state, refs} = pair) do
    case refs do
      %{^ref => _} when status == :ok ->
        {:ok, {state, Map.delete(refs, ref)}}
      %{^ref => {ts, msg}} ->
        # We use width infinity because the message has already been truncated
        log_event(:error, ["Failure while logging console message: ",
                           inspect(msg, width: :infinity)], ts, [], state)
        {:ok, {state, Map.delete(refs, ref)}}
      %{} ->
        {:ok, pair}
    end
  end

  def handle_info(_, {_, _} = pair) do
    {:ok, pair}
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

    format   = Logger.Formatter.compile Keyword.get(config, :format)
    level    = Keyword.get(config, :level)
    metadata = Keyword.get(config, :metadata, [])
    colors   = configure_colors(config)
    %{format: format, metadata: Enum.reverse(metadata),
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

  defp log_event(level, msg, ts, md, %{colors: colors, device: device} = state) do
    ref = make_ref()
    output =
      format_event(level, msg, ts, md, state)
      |> color_event(level, colors)
    send(device, {:io_request, self(), ref, {:put_chars, :unicode, output}})
    ref
  end

  defp format_event(level, msg, ts, md, %{format: format, metadata: keys}) do
    Logger.Formatter.format(format, level, msg, ts, take_metadata(md, keys))
  end

  defp take_metadata(metadata, keys) do
    Enum.reduce keys, [], fn key, acc ->
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
