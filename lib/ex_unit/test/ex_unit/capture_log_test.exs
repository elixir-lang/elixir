Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.CaptureLogTest do
  use ExUnit.Case

  require Logger

  import ExUnit.CaptureLog

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

  test "level aware" do
    assert capture_log([level: :warn], fn ->
      Logger.info "here"
    end) == ""
  end

  test "log tracking" do
    logged =
      assert capture_log(fn ->
        Logger.info "one"

        logged = capture_log(fn -> Logger.error "one" end)
        send(test = self(), {:nested, logged})

        Logger.warn "two"

        spawn(fn ->
          Logger.debug "three"
          send(test, :done)
        end)
        receive do: (:done -> :ok)
      end)

    assert logged =~ "[info]  one\n\e[0m"
    assert logged =~ "[warn]  two\n\e[0m"
    assert logged =~ "[debug] three\n\e[0m"
    assert logged =~ "[error] one\n\e[0m"

    receive do
      {:nested, logged} ->
        assert logged =~ "[error] one\n\e[0m"
        refute logged =~ "[warn]  two\n\e[0m"
    end
  end
end
