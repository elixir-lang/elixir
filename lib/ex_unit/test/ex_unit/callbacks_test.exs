Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.CallbacksTest do
  use ExUnit.Case, async: true

  setup_all do
    { :ok, [context: :setup_all] }
  end

  setup do
    { :ok, [initial_setup: true] }
  end

  setup context do
    assert context[:initial_setup]
    assert context[:context] == :setup_all
    { :ok, [context: :setup] }
  end

  setup context do
    if Process.get(:ex_unit_callback) do
      raise "ex_unit_callback was not cleaned"
    else
      Process.put(:ex_unit_callback, context[:test])
    end
    :ok
  end

  teardown context do
    assert context[:context] == :setup
    { :ok, context: :teardown }
  end

  teardown context do
    assert Process.get(:ex_unit_callback) == context[:test]
    Process.delete(:ex_unit_callback)
    assert context[:context] == :teardown
    :ok
  end

  teardown_all context do
    assert context[:context] == :setup_all
    :ok
  end

  test "callbacks can run custom code" do
    assert Process.get(:ex_unit_callback) == :"test callbacks can run custom code"
  end

  test "receives context from callback", context do
    assert context[:context] == :setup
  end

  test "assertions in setup_all are reported" do
    ExUnit.configure(formatter: ExUnit.TestsLoggingFormatter)

    defmodule SetupAllFailureTest do
      use ExUnit.Case
      setup_all do
        assert 1 == 2
      end
      test "foo" do
      end
    end

    { async, sync, load_us } = ExUnit.Server.start_run
    logs = ExUnit.Runner.run(async, sync, ExUnit.configuration, load_us)
           |> Enum.map(fn {reason, tc} -> {reason, tc.state} end)
    assert match?([
      case_started: nil,
      test_started: { :invalid, _ },
      test_finished: { :invalid, _ },
      case_finished: { :failed, _ }
    ], logs)
  end

  test "assertions in setup are reported" do
    ExUnit.configure(formatter: ExUnit.TestsLoggingFormatter)

    defmodule SetupFailureTest do
      use ExUnit.Case
      setup do
        assert 1 == 2
      end
      test "foo" do
      end
    end

    { async, sync, load_us } = ExUnit.Server.start_run
    logs = ExUnit.Runner.run(async, sync, ExUnit.configuration, load_us)
           |> Enum.map(fn {reason, tc} -> {reason, tc.state} end)
    assert match?([
      case_started: nil,
      test_started: nil,
      test_finished: { :failed, _ },
      case_finished: :passed
    ], logs)
  end

  test "assertions in teardown are reported" do
    ExUnit.configure(formatter: ExUnit.TestsLoggingFormatter)

    defmodule TeardownFailureTest do
      use ExUnit.Case
      teardown do
        assert 1 == 2
      end
      test "foo" do
      end
    end

    { async, sync, load_us } = ExUnit.Server.start_run
    logs = ExUnit.Runner.run(async, sync, ExUnit.configuration, load_us)
           |> Enum.map(fn {reason, tc} -> {reason, tc.state} end)
    IO.inspect(logs)
    assert match?([
      case_started: nil,
      test_started: nil,
      test_finished: { :failed, _ },
      case_finished: :passed
    ], logs)
  end

  test "assertions in teardown_all are reported" do
    ExUnit.configure(formatter: ExUnit.TestsLoggingFormatter)

    defmodule TeardownAllFailureTest do
      use ExUnit.Case
      teardown_all do
        assert 1 == 2
      end
      test "foo" do
      end
    end

    { async, sync, load_us } = ExUnit.Server.start_run
    logs = ExUnit.Runner.run(async, sync, ExUnit.configuration, load_us)
           |> Enum.map(fn {reason, tc} -> {reason, tc.state} end)
    assert match?([
      case_started: nil,
      test_started: nil,
      test_finished: :passed,
      case_finished: { :failed, _ }
    ], logs)
  end
end

defmodule ExUnit.CallbacksNoTests do
  use ExUnit.Case, async: true

  setup_all do
    raise "Never run"
  end

  setup do
    raise "Never run"
  end

  teardown do
    raise "Never run"
  end

  teardown_all do
    raise "Never run"
  end
end
