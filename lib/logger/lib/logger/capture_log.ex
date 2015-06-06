defmodule Logger.CaptureLog do
  def capture_log(_opts \\ [], fun) do
    old_gl = Process.group_leader()
    {:ok, gl} = ProxyIO.open()
    handler = {Logger.Backends.Capture, make_ref()}
    try do
      Process.group_leader(self(), gl)
      :ok = GenEvent.add_handler(Logger, handler, gl)
      _ = fun.()
      :ok
    catch
      kind, reason ->
        stack = System.stacktrace()
        _ = GenEvent.remove_handler(Logger, handler, nil)
        :erlang.raise(kind, reason, stack)
    else
      :ok ->
        {:ok, output} = GenEvent.remove_handler(Logger, handler, :get)
        IO.chardata_to_string(output)
    after
      Process.group_leader(self(), old_gl)
      ProxyIO.close(gl)
    end
  end
end
