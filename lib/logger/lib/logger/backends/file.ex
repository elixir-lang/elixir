defmodule Logger.Backends.File do
  @moduledoc false
  @default_log_file "log/#{Mix.env}.log"
  use GenEvent

  def init(_) do
    {:ok, configure([])}
  end

  def handle_call({:configure, options}, %{fd: fd} = _state) do
    File.close(fd)
    new_state = configure(options)
    {:ok, :ok, new_state}
  end

  def handle_event({level, _gl, {Logger, msg, ts, md}}, %{fd: fd, level: min_level} = state) do
    if (is_nil(min_level) or Logger.compare_levels(level, min_level) != :lt) and !is_nil(fd) do
      log_event(fd, level, msg, ts, md, state)
    end
    {:ok, state}
  end

  def terminate(_, %{fd: fd} = _state) do
    File.close(fd)
  end

  defp configure(options) do
    env = Application.get_env(:logger, :file, [])
    file = Keyword.merge(env, options)
    Application.put_env(:logger, :file, file)

    format = file
      |> Keyword.get(:format)
      |> Logger.Formatter.compile

    metadata = Keyword.get(file, :metadata, [])
    level    = Keyword.get(file, :level)
    fd       = Keyword.get(file, :log_file, @default_log_file) |> open_file
    %{format: format, metadata: metadata, level: level, fd: fd}
  end

  defp open_file(nil), do: nil
  defp open_file(location) do
    case :filelib.ensure_dir(location) do
      :ok ->
        {:ok, fd} = File.open(location, [:write, :append])
        fd
      _ -> nil
    end
  end

  defp log_event(fd, level, msg, ts, md, state) do
    format = Map.get(state, :format)
    metadata = Map.get(state, :metadata)
    event = Logger.Formatter.format(format, level, msg, ts, Dict.take(md, metadata))
    IO.write(fd, event)
  end
end
