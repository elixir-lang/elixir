Code.require_file("test_helper.exs", __DIR__)

defmodule ExUnitTest do
  use ExUnit.Case

  import ExUnit.CaptureIO

  test "supports many runs" do
    defmodule SampleTest do
      use ExUnit.Case

      test "true" do
        assert false
      end

      test "false" do
        assert false
      end
    end

    ExUnit.Server.modules_loaded()

    assert capture_io(fn ->
             assert ExUnit.run() == %{failures: 2, skipped: 0, total: 2}
           end) =~ "2 tests, 2 failures"
  end

  test "doesn't hang on exits" do
    defmodule EventServerTest do
      use ExUnit.Case

      test "spawn and crash" do
        spawn_link(fn ->
          exit(:foo)
        end)

        receive after: (1000 -> :ok)
      end
    end

    ExUnit.Server.modules_loaded()

    assert capture_io(fn ->
             assert ExUnit.run() == %{failures: 1, skipped: 0, total: 1}
           end) =~ "1 test, 1 failure"
  end

  test "supports timeouts" do
    defmodule TimeoutTest do
      use ExUnit.Case

      @tag timeout: 10
      test "ok" do
        Process.sleep(:infinity)
      end
    end

    ExUnit.Server.modules_loaded()

    output = capture_io(fn -> ExUnit.run() end)
    assert output =~ "** (ExUnit.TimeoutError) test timed out after 10ms"
    assert output =~ ~r"\(elixir\) lib/process\.ex:\d+: Process\.sleep/1"
  end

  test "supports configured timeout" do
    defmodule ConfiguredTimeoutTest do
      use ExUnit.Case

      test "ok" do
        Process.sleep(:infinity)
      end
    end

    ExUnit.configure(timeout: 5)
    ExUnit.Server.modules_loaded()
    output = capture_io(fn -> ExUnit.run() end)
    assert output =~ "** (ExUnit.TimeoutError) test timed out after 5ms"
  after
    ExUnit.configure(timeout: 60000)
  end

  test "reports slow tests" do
    defmodule SlowestTest do
      use ExUnit.Case

      test "tardy" do
        refute false
      end

      test "delayed" do
        Process.sleep(5)
        assert false
      end

      test "slowest" do
        Process.sleep(10)
        refute false
      end
    end

    on_exit_reload_config()
    ExUnit.start(slowest: 2)
    ExUnit.Server.modules_loaded()

    output = capture_io(fn -> ExUnit.run() end)

    assert output =~ ~r"Top 2 slowest \(\d+\.\d+s\), \d+.\d% of total time:"
    assert output =~ ~r"\* test slowest \(.+ms\)"
    assert output =~ ~r"\* test delayed \(.+ms\)"
  end

  test "sets max cases to one with trace enabled" do
    on_exit_reload_config()
    ExUnit.start(trace: true, max_cases: 10, autorun: false)
    config = ExUnit.configuration()
    assert config[:trace]
    assert config[:max_cases] == 1
    assert config[:timeout] == 60000
  end

  test "does not set timeout to infinity and the max cases to 1 with trace disabled" do
    on_exit_reload_config()
    ExUnit.start(trace: false, autorun: false)
    config = ExUnit.configuration()
    refute config[:trace]
    assert config[:max_cases] == System.schedulers_online() * 2
    assert config[:timeout] == 60000
  end

  test "sets trace when slowest is enabled" do
    on_exit_reload_config()
    ExUnit.start(slowest: 10, max_cases: 10, autorun: false)
    config = ExUnit.configuration()
    assert config[:trace]
    assert config[:slowest] == 10
    assert config[:max_cases] == 1
  end

  test "filtering cases with tags" do
    defmodule ParityTest do
      use ExUnit.Case

      test "zero", do: :ok

      @tag even: false
      test "one", do: :ok

      @tag even: true
      test "two", do: assert(1 == 2)

      @tag even: false
      test "three", do: :ok
    end

    # Empty because it is already loaded
    {result, output} = run_with_filter([], [])
    assert result == %{failures: 1, skipped: 0, total: 4}
    assert output =~ "4 tests, 1 failure"

    {result, output} = run_with_filter([exclude: [even: true]], [ParityTest])
    assert result == %{failures: 0, skipped: 1, total: 4}
    assert output =~ "4 tests,"
    assert output =~ "0 failures,"
    assert output =~ "1 skipped"

    {result, output} = run_with_filter([exclude: :even], [ParityTest])
    assert result == %{failures: 0, skipped: 3, total: 4}
    assert output =~ "4 tests,"
    assert output =~ "0 failures,"
    assert output =~ "3 skipped"

    {result, output} = run_with_filter([exclude: :even, include: [even: true]], [ParityTest])
    assert result == %{failures: 1, skipped: 2, total: 4}
    assert output =~ "4 tests,"
    assert output =~ "1 failure,"
    assert output =~ "2 skipped"

    {result, output} = run_with_filter([exclude: :test, include: [even: true]], [ParityTest])
    assert result == %{failures: 1, skipped: 3, total: 4}
    assert output =~ "4 tests,"
    assert output =~ "1 failure,"
    assert output =~ "3 skipped"
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

    ExUnit.Server.modules_loaded()
    output = capture_io(&ExUnit.run/0)
    assert output =~ "[debug] two"
    refute output =~ "[debug] one"
    assert output =~ "[debug] three"
    refute output =~ "[debug] four"
  end

  test "supports multi errors" do
    capture_io(:stderr, fn ->
      defmodule MultiTest do
        use ExUnit.Case

        test "multi" do
          error1 =
            try do
              assert 1 = 2
            rescue
              e in ExUnit.AssertionError ->
                {:error, e, System.stacktrace()}
            end

          error2 =
            try do
              assert 3 > 4
            rescue
              e in ExUnit.AssertionError ->
                {:error, e, System.stacktrace()}
            end

          raise ExUnit.MultiError, errors: [error1, error2]
        end
      end
    end)

    ExUnit.Server.modules_loaded()

    output =
      capture_io(fn ->
        assert ExUnit.run() == %{failures: 1, skipped: 0, total: 1}
      end)

    assert output =~ "1 test, 1 failure"
    assert output =~ "1) test multi (ExUnitTest.MultiTest)"
    assert output =~ "Failure #1"
    assert output =~ "Failure #2"

    assert_raise ExUnit.MultiError, ~r/oops/, fn ->
      error = {:error, RuntimeError.exception("oops"), System.stacktrace()}
      raise ExUnit.MultiError, errors: [error]
    end
  end

  test "raises friendly error for duplicate test names" do
    message = ~S("test duplicate" is already defined in ExUnitTest.TestWithSameNames)

    assert_raise ExUnit.DuplicateTestError, message, fn ->
      defmodule TestWithSameNames do
        use ExUnit.Case

        test "duplicate" do
          assert true
        end

        test "duplicate" do
          assert true
        end
      end
    end
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

    ExUnit.Server.modules_loaded()

    output =
      capture_io(fn ->
        assert ExUnit.run() == %{failures: 1, skipped: 0, total: 1}
      end)

    assert output =~ "Not implemented"
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
      test "this will raise", do: raise("oops")

      @tag skip: "won't work"
      test "this will also raise", do: raise("oops")
    end

    ExUnit.Server.modules_loaded()

    output =
      capture_io(fn ->
        assert ExUnit.run() == %{failures: 0, skipped: 2, total: 2}
      end)

    assert output =~ "2 tests,"
    assert output =~ "0 failures,"
    assert output =~ "2 skipped"
  end

  test "filtering cases with :module tag" do
    defmodule FirstTestModule do
      use ExUnit.Case
      test "ok", do: :ok
    end

    defmodule SecondTestModule do
      use ExUnit.Case
      test "false", do: assert(false)
    end

    # Empty because it is already loaded
    {result, output} = run_with_filter([exclude: :module], [])
    assert result == %{failures: 0, skipped: 2, total: 2}

    assert output =~ "2 tests,"
    assert output =~ "0 failures,"
    assert output =~ "2 skipped"

    {result, output} =
      [exclude: :test, include: [module: "ExUnitTest.SecondTestModule"]]
      |> run_with_filter([FirstTestModule, SecondTestModule])

    assert result == %{failures: 1, skipped: 1, total: 2}
    assert output =~ "1) test false (ExUnitTest.SecondTestModule)"
    assert output =~ "2 tests,"
    assert output =~ "1 failure,"
    assert output =~ "1 skipped"
  end

  test "raises on reserved tag :file in module" do
    assert_raise RuntimeError, "cannot set tag :file because it is reserved by ExUnit", fn ->
      defmodule ReservedTagFile do
        use ExUnit.Case

        @tag file: "oops"
        test "sample", do: :ok
      end
    end
  end

  test "raises on reserved tag :async in module" do
    assert_raise RuntimeError, "cannot set tag :async because it is reserved by ExUnit", fn ->
      defmodule ReservedTagAsync do
        use ExUnit.Case

        @tag async: true
        test "sample", do: :ok
      end
    end
  end

  test "raises on reserved tag :file in setup" do
    defmodule ReservedSetupTagFile do
      use ExUnit.Case

      setup do
        {:ok, file: :foo}
      end

      test "sample", do: :ok
    end

    ExUnit.Server.modules_loaded()

    output =
      capture_io(fn ->
        assert ExUnit.run() == %{failures: 1, skipped: 0, total: 1}
      end)

    assert output =~ "trying to set reserved field :file"
  end

  test "raises on reserved tag :async in setup" do
    defmodule ReservedSetupTagAsync do
      use ExUnit.Case

      setup do
        {:ok, async: true}
      end

      test "sample", do: :ok
    end

    ExUnit.Server.modules_loaded()

    output =
      capture_io(fn ->
        assert ExUnit.run() == %{failures: 1, skipped: 0, total: 1}
      end)

    assert output =~ "trying to set reserved field :async"
  end

  test "does not raise on reserved tag in setup_all (lower priority)" do
    defmodule ReservedSetupAllTag do
      use ExUnit.Case

      setup_all do
        {:ok, file: :foo}
      end

      test "sample", do: :ok
    end

    ExUnit.Server.modules_loaded()

    capture_io(fn ->
      assert ExUnit.run() == %{failures: 0, skipped: 0, total: 1}
    end)
  end

  test "seed is predictable and different for each test" do
    global_seed = ExUnit.configuration()[:seed]
    ExUnit.configure(seed: 1)

    defmodule PredictableSeedTest do
      use ExUnit.Case, async: true

      test "generated seed is always the same in the same module and test" do
        assert :rand.uniform(1_000_000) == 622_983
      end

      test "generated seed is different for other test" do
        assert :rand.uniform(1_000_000) == 783_982
      end
    end

    defmodule DifferentModuleWithDifferentSeedTest do
      use ExUnit.Case, async: true

      test "generated seed is always the same in the same module and test" do
        assert :rand.uniform(1_000_000) == 358_099
      end
    end

    ExUnit.Server.modules_loaded()

    assert capture_io(fn ->
             assert ExUnit.run() == %{failures: 0, skipped: 0, total: 3}
           end) =~ "3 tests, 0 failures"

    ExUnit.configure(seed: global_seed)
  end

  defp on_exit_reload_config(extra \\ []) do
    old_config = ExUnit.configuration()
    on_exit(fn -> ExUnit.configure(extra ++ old_config) end)
  end

  defp run_with_filter(filters, cases) do
    Enum.each(cases, &ExUnit.Server.add_sync_module/1)
    ExUnit.Server.modules_loaded()
    opts = Keyword.merge(ExUnit.configuration(), filters)
    output = capture_io(fn -> Process.put(:capture_result, ExUnit.Runner.run(opts, nil)) end)
    {Process.get(:capture_result), output}
  end
end
