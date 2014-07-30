defmodule Logger.Backends.Console do
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
    console = Keyword.merge(Application.get_env(:logger, :console, []), options)
    Application.put_env(:logger, :console, console)

    format = console
      |> Keyword.get(:format)
      |> Logger.Formatter.compile

    level    = Keyword.get(console, :level)
    metadata = Keyword.get(console, :metadata, [])
    %{format: format, metadata: metadata, level: level}
  end

  defp log_event(level, msg, ts, md, %{format: format, metadata: metadata}) do
    :io.put_chars :user,
      Logger.Formatter.format(format, level, msg, ts, Dict.take(md, metadata))
  end
end
