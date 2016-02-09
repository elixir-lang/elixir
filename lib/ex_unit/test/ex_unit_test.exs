Code.require_file "test_helper.exs", __DIR__

defmodule ExUnitTest do
  use ExUnit.Case

  import ExUnit.CaptureIO

  test "it supports many runs" do
    defmodule SampleTest do
      use ExUnit.Case

      test "true" do
        assert false
      end

      test "false" do
        assert false
      end
    end

    assert capture_io(fn ->
      assert ExUnit.run == %{failures: 2, skipped: 0, total: 2}
    end) =~ "2 tests, 2 failures"
  end

  test "it doesn't hang on exits" do
    defmodule EventServerTest do
      use ExUnit.Case

      test "spawn and crash" do
        spawn_link(fn ->
          exit :foo
        end)
        receive after: (1000 -> :ok)
      end
    end

    assert capture_io(fn ->
      assert ExUnit.run == %{failures: 1, skipped: 0, total: 1}
    end) =~ "1 test, 1 failure"
  end

  test "it supports timeouts" do
    defmodule TimeoutTest do
      use ExUnit.Case

      @tag timeout: 10
      test "ok" do
        :timer.sleep(:infinity)
      end
    end

    output = capture_io(fn -> ExUnit.run end)
    assert output =~ "** (ExUnit.TimeoutError) test timed out after 10ms"
    assert output =~ ~r"\(stdlib\) timer\.erl:\d+: :timer\.sleep/1"
  end

  test "it supports configured timeout" do
    defmodule ConfiguredTimeoutTest do
      use ExUnit.Case

      test "ok" do
        :timer.sleep(:infinity)
      end
    end

    ExUnit.configure(timeout: 5)
    output = capture_io(fn -> ExUnit.run end)
    assert output =~ "** (ExUnit.TimeoutError) test timed out after 5ms"
  after
    ExUnit.configure(timeout: 60_000)
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

    {result, output} = run_with_filter([], test_cases)
    assert result == %{failures: 1, skipped: 0, total: 4}
    assert output =~ "4 tests, 1 failure"

    {result, output} = run_with_filter([exclude: [even: true]], test_cases)
    assert result == %{failures: 0, skipped: 1, total: 4}
    assert output =~ "4 tests, 0 failures, 1 skipped"

    {result, output} = run_with_filter([exclude: :even], test_cases)
    assert result == %{failures: 0, skipped: 3, total: 4}
    assert output =~ "4 tests, 0 failures, 3 skipped"

    {result, output} = run_with_filter([exclude: :even, include: [even: true]], test_cases)
    assert result == %{failures: 1, skipped: 2, total: 4}
    assert output =~ "4 tests, 1 failure, 2 skipped"

    {result, output} = run_with_filter([exclude: :test, include: [even: true]], test_cases)
    assert result == %{failures: 1, skipped: 3, total: 4}
    assert output =~ "4 tests, 1 failure, 3 skipped"
  end

  test "log capturing" do
    defmodule LogCapturingTest do
      use ExUnit.Case

      require Logger

      setup_all do
        :ok = Logger.remove_backend(:console)
        on_exit(fn -> Logger.add_backend(:console, flush: true) end)
      end

      @tag :capture_log
      test "one" do
        Logger.debug("one")
        assert 1 == 1
      end

      @tag :capture_log
      test "two" do
        Logger.debug("two")
        assert 1 == 2
      end

      @tag capture_log: []
      test "three" do
        Logger.debug("three")
        assert 1 == 2
      end

      test "four" do
        Logger.debug("four")
        assert 1 == 2
      end
    end

    output = capture_io(&ExUnit.run/0)
    assert output =~ "[debug] two"
    refute output =~ "[debug] one"
    assert output =~ "[debug] three"
    refute output =~ "[debug] four"
  end

  test "supports multi errors" do
    capture_io :stderr, fn ->
      defmodule MultiTest do
        use ExUnit.Case

        test "multi" do
          error1 =
            try do
              assert 1 = 2
            rescue e in ExUnit.AssertionError ->
              {:error, e, System.stacktrace}
            end

          error2 =
            try do
              assert 3 > 4
            rescue e in ExUnit.AssertionError ->
              {:error, e, System.stacktrace}
            end

          raise ExUnit.MultiError, errors: [error1, error2]
        end
      end
    end

    output = capture_io(fn ->
      assert ExUnit.run == %{failures: 1, skipped: 0, total: 1}
    end)

    assert output =~ "1 test, 1 failure"
    assert output =~ "1) test multi (ExUnitTest.MultiTest)"
    assert output =~ "Failure #1"
    assert output =~ "Failure #2"
  end

  test "registers only the first test with any given name" do
    capture_io :stderr, fn ->
      defmodule TestWithSameNames do
        use ExUnit.Case

        test "same name, different outcome" do
          assert 1 == 1
        end

        test "same name, different outcome" do
          assert 1 == 2
        end
      end
    end

    assert capture_io(fn ->
      assert ExUnit.run == %{failures: 0, skipped: 0, total: 1}
    end) =~ "1 test, 0 failure"
  end

  test "produces error on not implemented tests" do
    defmodule TestNotImplemented do
      use ExUnit.Case

      setup context do
        assert context[:not_implemented]
        :ok
      end

      test "this is not implemented yet"
    end

    output = capture_io(fn ->
      assert ExUnit.run == %{failures: 1, skipped: 0, total: 1}
    end)

    assert output =~ "Not yet implemented"
    assert output =~ "1 test, 1 failure"
  end

  test "skips tagged test with skip" do
    defmodule TestSkipped do
      use ExUnit.Case

      setup context do
        assert context[:not_implemented]
        :ok
      end

      @tag :skip
      test "this will raise", do: raise "oops"

      @tag skip: "won't work"
      test "this will also raise", do: raise "oops"
    end

    output = capture_io(fn ->
      assert ExUnit.run == %{failures: 0, skipped: 2, total: 2}
    end)

    assert output =~ "2 tests, 0 failures, 2 skipped"
  end

  test "filtering cases with :case tag" do
    defmodule FirstTestCase do
      use ExUnit.Case
      test "ok", do: :ok
    end

    defmodule SecondTestCase do
      use ExUnit.Case
      test "false", do: assert false
    end

    test_cases = ExUnit.Server.start_run

    {result, output} = run_with_filter([exclude: :case], test_cases)
    assert result == %{failures: 0, skipped: 2, total: 2}
    assert output =~ "2 tests, 0 failures, 2 skipped"

    {result, output} =
      [exclude: :test, include: [case: "ExUnitTest.SecondTestCase"]]
      |> run_with_filter(test_cases)
    assert result == %{failures: 1, skipped: 1, total: 2}
    assert output =~ "1) test false (ExUnitTest.SecondTestCase)"
    assert output =~ "2 tests, 1 failure, 1 skipped"
  end

  test "raises on reserved tag in module" do
    assert_raise RuntimeError, "cannot set tag :file because it is reserved by ExUnit", fn ->
      defmodule ReservedTag do
        use ExUnit.Case

        setup do
          {:ok, file: :foo}
        end

        @tag file: "oops"
        test "sample", do: :ok
      end
    end
  end

  test "raises on reserved tag in setup" do
    defmodule ReservedSetupTag do
      use ExUnit.Case

      setup do
        {:ok, file: :foo}
      end

      test "sample", do: :ok
    end

    output = capture_io(fn ->
      assert ExUnit.run == %{failures: 1, skipped: 0, total: 1}
    end)

    assert output =~ "trying to set reserved field :file"
  end

  test "does not raise on reserved tag in setup_all (lower priority)" do
    defmodule ReservedSetupAllTag do
      use ExUnit.Case

      setup_all do
        {:ok, file: :foo}
      end

      test "sample", do: :ok
    end

    capture_io(fn ->
      assert ExUnit.run == %{failures: 0, skipped: 0, total: 1}
    end)
  end

  defp run_with_filter(filters, {async, sync, load_us}) do
    opts = Keyword.merge(ExUnit.configuration, filters)
    output = capture_io fn ->
      Process.put(:capture_result, ExUnit.Runner.run(async, sync, opts, load_us))
    end
    {Process.get(:capture_result), output}
  end
end
