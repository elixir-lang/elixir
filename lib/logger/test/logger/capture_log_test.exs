Code.require_file "../test_helper.exs", __DIR__

defmodule Logger.CaptureLogTest do
  use ExUnit.Case

  require Logger

  import Logger.CaptureLog

  setup_all do
    :ok = Logger.remove_backend(:console)
    on_exit(fn -> Logger.add_backend(:console, flush: true) end)
  end

  test "no output" do
    assert capture_log(fn -> end) == ""
  end

  test "assert inside" do
    group_leader = Process.group_leader()

    try do
      capture_log(fn ->
        assert false
      end)
    rescue
      error in [ExUnit.AssertionError] ->
        assert error.message == "Expected truthy, got false"
    end

    # Ensure no leakage on failures
    assert group_leader == Process.group_leader()
  end

  test "log tracking" do
    captured =
      assert capture_log(fn ->
        Logger.info "one"

        captured = capture_log(fn -> Logger.error "one" end)
        send(test = self(), {:nested, captured})

        Logger.warn "two"

        spawn(fn ->
          Logger.debug "three"
          send(test, :done)
        end)
        receive do: (:done -> :ok)
      end)

    assert captured =~ "[info]  one\n\e[0m"
    assert captured =~ "[warn]  two\n\e[0m"
    assert captured =~ "[debug] three\n\e[0m"
    refute captured =~ "[error] one\n\e[0m"

    receive do
      {:nested, captured} ->
        assert captured =~ "[error] one\n\e[0m"
    end
  end
end
