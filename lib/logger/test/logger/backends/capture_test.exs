defmodule Logger.Backends.CaptureTest do
  use Logger.Case, async: true

  require Logger

  setup_all do
    :ok = Logger.remove_backend(:console)
    on_exit(fn -> Logger.add_backend(:console, flush: true) end)
  end

  setup do
    handler = {Logger.Backends.Capture, make_ref()}
    :ok = GenEvent.add_handler(Logger, handler, {Process.group_leader(), []})
    {:ok, capture: handler}
  end

  defp get_log(handler) do
    {:ok, output} = GenEvent.remove_handler(Logger, handler, :get)
    IO.chardata_to_string(output)
  end

  test "format configuration", context do
    Logger.configure_backend(context[:capture], format: "$message [$level]")
    Logger.debug "hello"

    assert get_log(context[:capture]) =~ "hello [debug]"
  end

  test "level configuration", context do
    Logger.configure_backend(context[:capture], level: :info)
    Logger.debug "hello"

    assert get_log(context[:capture]) == ""
  end

  test "metadata configuration", context do
    Logger.configure_backend(context[:capture], format: "$metadata$message", metadata: [:id])
    Logger.metadata(id: 11)
    Logger.metadata(id: 13)
    Logger.debug("hello")

    assert get_log(context[:capture]) =~ "id=13 hello"
  end

  test "colors configuration", context do
    Logger.configure_backend(context[:capture], format: "$message", colors: [enabled: false])
    Logger.debug("hello")

    assert get_log(context[:capture]) == "hello"
  end
end
