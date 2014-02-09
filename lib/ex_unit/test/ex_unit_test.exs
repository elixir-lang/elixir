Code.require_file "test_helper.exs", __DIR__

defmodule ExUnit.CounterFormatter do
  use GenEvent.Behaviour

  @timeout 30_000

  def start(opts) do
    :gen_event.start(ExUnit.Formatter.Manager, __MODULE__, opts)
  end

  def stop() do
    :gen_event.call(ExUnit.Formatter.Manager, __MODULE__, :stop, @timeout)
  end

  def test_finished(id, test) do
    :gen_server.cast(id, { :test_finished, test })
  end

  def init(_opts) do
    { :ok, 0 }
  end

  def handle_event({ :suite_finished }, _from, tests_counter) do
    { :stop, :normal, tests_counter, tests_counter }
  end

  def handle_event({ :test_finished, ExUnit.Test[state: { :skip, _ }] }, tests_counter) do
    { :ok, tests_counter }
  end

  def handle_event({ :test_finished, _ }, tests_counter) do
    { :ok, tests_counter + 1 }
  end

  def handle_event(_, tests_counter) do
    { :ok, tests_counter }
  end

  def handle_call(:stop, tests_counter) do
    { :remove_handler, tests_counter }
  end
end

defmodule ExUnitTest do
  use ExUnit.Case, async: false

  setup do
    # For this ExUnit tested by ExUnit trick to work we'll need to temporarily
    # replace the formatter manager. This is rather ugly and likely to cause
    # problems if formatter events are generated after a setup or before a
    # teardown. Luckily this is not the case at the moment.
    # ExUnit.configure(formatters: [ExUnit.CounterFormatter])
    real_manager = Process.whereis(ExUnit.Formatter.Manager)
    Process.unregister(ExUnit.Formatter.Manager)
    ExUnit.Formatter.Manager.start_link()
    ExUnit.Formatter.Manager.add_handler(ExUnit.CounterFormatter, [])
    { :ok, [real_manager: real_manager] }
  end

  teardown ctxt do
    Process.unregister(ExUnit.Formatter.Manager)
    Process.register(ctxt[:real_manager], ExUnit.Formatter.Manager)
    :ok
  end

  test "it supports many runs" do
    defmodule SampleTest do
      use ExUnit.Case, async: false

      test "true" do
        assert false
      end

      test "false" do
        assert false
      end
    end

    assert ExUnit.run == 2
  end

  test "it doesn't hang on exists" do
    defmodule EventServerTest do
      use ExUnit.Case, async: false

      test "spawn and crash" do
        Process.spawn_link(fn ->
          exit :foo
        end)
        receive after: (1000 -> :ok)
      end
    end

    assert ExUnit.run == 1
  end

  test "filtering cases with tags" do
    defmodule ParityTest do
      use ExUnit.Case

      test "zero", do: :ok

      @tag even: false
      test "one", do: :ok

      @tag even: true
      test "two", do: assert 1 == 2

      @tag even: false
      test "three", do: :ok
    end

    test_cases = ExUnit.Server.start_run

    assert run_with_filter([], test_cases) == { 4, 1 }
    assert run_with_filter([exclude: [even: true]], test_cases) == { 3, 0 }
    assert run_with_filter([exclude: :even], test_cases) == { 1, 0 }
    assert run_with_filter([exclude: :even, include: [even: true]], test_cases) == { 2, 1 }
    assert run_with_filter([exclude: :test, include: [even: true]], test_cases) == { 1, 1 }
  end

  defp run_with_filter(filters, { async, sync, load_us }) do
    opts = Keyword.merge(ExUnit.configuration, filters)
    ExUnit.Runner.run(async, sync, opts, load_us, { :total, :failures })
  end
end
