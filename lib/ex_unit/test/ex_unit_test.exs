Code.require_file "test_helper.exs", __DIR__

defmodule ExUnit.CounterFormatter do
  @timeout 30_000
  @behaviour ExUnit.Formatter

  use GenServer.Behaviour

  def suite_started(opts) do
    { :ok, pid } = :gen_server.start_link(__MODULE__, opts, [])
    pid
  end

  def suite_finished(id, _run_us, _load_us) do
    :gen_server.call(id, { :suite_finished }, @timeout)
  end

  def case_started(_id, _test_case) do
    :ok
  end

  def case_finished(_id, _test_case) do
    :ok
  end

  def test_started(_id, _test) do
    :ok
  end

  def test_finished(id, test) do
    :gen_server.cast(id, { :test_finished, test })
  end

  def init(_opts) do
    { :ok, 0 }
  end

  def handle_call({ :suite_finished }, _from, tests_counter) do
    { :stop, :normal, tests_counter, tests_counter }
  end

  def handle_cast({ :test_finished, ExUnit.Test[state: { :skip, _ }] }, tests_counter) do
    { :noreply, tests_counter }
  end

  def handle_cast({ :test_finished, _ }, tests_counter) do
    { :noreply, tests_counter + 1 }
  end
end

defmodule ExUnitTest do
  use ExUnit.Case, async: false

  setup do
    ExUnit.configure(formatter: ExUnit.CounterFormatter)
    :ok
  end

  teardown do
    ExUnit.configure(formatter: ExUnit.CLIFormatter)
    :ok
  end

  test "it supports many runs" do
    defmodule SampleTest do
      use ExUnit.Case, async: false

      test "true" do
        assert true
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

      test "zero", do: assert true

      @tag even: false
      test "one", do: assert true

      @tag even: true
      test "two", do: assert true

      @tag even: false
      test "three", do: assert true
    end

    test_cases = ExUnit.Server.start_run

    assert run_with_filter([], test_cases) == 4
    assert run_with_filter([exclude: [even: true]], test_cases) == 3
    assert run_with_filter([exclude: :even], test_cases) == 1
    assert run_with_filter([exclude: :even, include: [even: true]], test_cases) == 2
    assert run_with_filter([exclude: :test, include: [even: true]], test_cases) == 1
  end

  defp run_with_filter(filters, { async, sync, load_us }) do
    opts = Keyword.merge(ExUnit.configuration, filters)
    ExUnit.Runner.run(async, sync, opts, load_us)
  end
end
