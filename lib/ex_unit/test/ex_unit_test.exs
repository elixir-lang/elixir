Code.require_file("test_helper.exs", __DIR__)

defmodule ExUnitTest do
  use ExUnit.Case

  import ExUnit.CaptureIO

  test "supports many runs and loads" do
    defmodule SampleTest do
      use ExUnit.Case

      test "true" do
        assert false
      end

      test "false" do
        assert false
      end
    end

    configure_and_reload_on_exit([])

    assert capture_io(fn ->
             assert ExUnit.run() == %{failures: 2, skipped: 0, total: 2, excluded: 0}
           end) =~ "\n2 tests, 2 failures\n"

    ExUnit.Server.modules_loaded(false)

    assert capture_io(fn ->
             assert ExUnit.async_run() |> ExUnit.await_run() ==
                      %{failures: 0, skipped: 0, total: 0, excluded: 0}
           end) =~ "\n0 failures\n"
  end

  test "supports rerunning given modules" do
    defmodule SampleAsyncTest do
      use ExUnit.Case, async: true, register: false

      test "true" do
        assert false
      end
    end

    defmodule SampleSyncTest do
      use ExUnit.Case, register: false

      test "true" do
        assert false
      end
    end

    defmodule IgnoreTest do
      use ExUnit.Case

      test "true" do
        assert false
      end
    end

    configure_and_reload_on_exit([])

    assert capture_io(fn ->
             assert ExUnit.run() == %{
                      failures: 1,
                      skipped: 0,
                      total: 1,
                      excluded: 0
                    }
           end) =~ "\n1 test, 1 failure\n"

    sample = [SampleSyncTest, SampleAsyncTest]

    assert capture_io(fn ->
             assert ExUnit.run(sample) == %{
                      failures: 2,
                      skipped: 0,
                      total: 2,
                      excluded: 0
                    }
           end) =~ "\n2 tests, 2 failures\n"

    assert capture_io(fn ->
             assert ExUnit.run(sample ++ sample) == %{
                      failures: 2,
                      skipped: 0,
                      total: 2,
                      excluded: 0
                    }
           end) =~ "\n2 tests, 2 failures\n"
  end

  test "prints aborted runs on sigquit", config do
    Process.register(self(), :aborted_on_sigquit)
    line = __ENV__.line + 5

    defmodule SleepOnTest do
      use ExUnit.Case, async: true

      test "true" do
        send(:aborted_on_sigquit, :sleeping)
        Process.sleep(:infinity)
      end
    end

    defmodule SleepOnSetupAll do
      use ExUnit.Case, async: true

      setup_all do
        send(:aborted_on_sigquit, :sleeping)
        Process.sleep(:infinity)
      end

      test "true", do: :ok
    end

    configure_and_reload_on_exit(max_cases: 8)

    result =
      capture_io(fn ->
        {pid, ref} = spawn_monitor(fn -> ExUnit.run() end)
        assert_receive :sleeping
        assert_receive :sleeping

        # We are testing implementation details but since this involves
        # the signal handler, it is truly the only way to test it.
        send(pid, {ref, self(), :sigquit})

        receive do
          ^ref -> :ok
        end
      end)

    assert result =~ "Aborting test suite, the following have not completed:"
    assert result =~ ~r"\* ExUnitTest.SleepOnSetupAll \[.*test/ex_unit_test.exs\]"
    assert result =~ ~r"\* test true \[.*test/ex_unit_test.exs:#{line}\]"

    assert result =~ """
           Showing results so far...

           0 failures
           """
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

    configure_and_reload_on_exit([])

    assert capture_io(fn ->
             assert ExUnit.run() == %{failures: 1, skipped: 0, total: 1, excluded: 0}
           end) =~ "\n1 test, 1 failure\n"
  end

  test "reports capture log crashes" do
    defmodule CaptureLogTest do
      use ExUnit.Case

      test "capture log crash because logger stopped" do
        Logger.App.stop()
        Logger.App.start()
        flunk("this should fail")
      end
    end

    configure_and_reload_on_exit([])

    assert capture_io(fn ->
             assert ExUnit.run() == %{failures: 1, skipped: 0, total: 1, excluded: 0}
           end) =~ "\n1 test, 1 failure\n"
  end

  test "supports timeouts" do
    defmodule TimeoutTest do
      use ExUnit.Case

      @tag timeout: 10
      test "ok" do
        Process.sleep(:infinity)
      end
    end

    output = capture_io(fn -> ExUnit.run() end)
    assert output =~ "** (ExUnit.TimeoutError) test timed out after 10ms"
    assert output =~ ~r"\(elixir #{System.version()}\) lib/process\.ex:\d+: Process\.sleep/1"
  end

  test "supports configured timeout" do
    defmodule ConfiguredTimeoutTest do
      use ExUnit.Case

      test "ok" do
        Process.sleep(:infinity)
      end
    end

    ExUnit.configure(timeout: 5)
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

    configure_and_reload_on_exit(slowest: 2)

    output = capture_io(fn -> ExUnit.run() end)
    assert output =~ ~r"Top 2 slowest \(\d+\.\d+s\), \d+.\d% of total time:"
    assert output =~ ~r"\* test slowest \(.+ms\)"
    assert output =~ ~r"\* test delayed \(.+ms\)"
  end

  test "reports slow test modules" do
    defmodule SlowTestModule do
      use ExUnit.Case

      test "slow" do
        refute false
      end
    end

    defmodule SlowerTestModule do
      use ExUnit.Case

      test "slower" do
        Process.sleep(5)
        refute false
      end
    end

    defmodule SlowestTestModule do
      use ExUnit.Case

      test "slowest" do
        Process.sleep(10)
        refute false
      end
    end

    configure_and_reload_on_exit(slowest_modules: 2)

    output = capture_io(fn -> ExUnit.run() end)
    assert output =~ ~r"Top 2 slowest \(\d+\.\d+s\), \d+.\d% of total time:"
    assert output =~ ~r"SlowestTestModule \(.+ms\)"
    assert output =~ ~r"SlowerTestModule \(.+ms\)"
  end

  test "sets max cases to one with trace enabled" do
    configure_and_reload_on_exit(trace: true, max_cases: 10)
    config = ExUnit.configuration()
    assert config[:trace]
    assert config[:max_cases] == 1
    assert config[:timeout] == 60000
  end

  test "sets trace when slowest is enabled" do
    configure_and_reload_on_exit(slowest: 10, max_cases: 10)
    config = ExUnit.configuration()
    assert config[:trace]
    assert config[:slowest] == 10
    assert config[:max_cases] == 1
  end

  test "filters to the given test IDs when the :only_test_ids option is provided" do
    defmodule TestIdTestModule do
      use ExUnit.Case

      test "passing", do: :ok
      test "failing", do: assert(1 == 2)
    end

    test_ids =
      MapSet.new([
        {TestIdTestModule, :"test failing"},
        {TestIdTestModule, :"test missing"},
        {MissingModule, :"test passing"}
      ])

    {result, output} = run_with_filter([only_test_ids: test_ids], [])
    assert result == %{failures: 1, skipped: 0, excluded: 0, total: 1}
    assert output =~ "\n1 test, 1 failure\n"
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
    assert result == %{failures: 1, skipped: 0, total: 4, excluded: 0}
    assert output =~ "\n4 tests, 1 failure\n"

    {result, output} = run_with_filter([exclude: [even: true]], [ParityTest])
    assert result == %{failures: 0, skipped: 0, excluded: 1, total: 4}
    assert output =~ "\n4 tests, 0 failures, 1 excluded\n"

    {result, output} = run_with_filter([exclude: :even], [ParityTest])
    assert result == %{failures: 0, skipped: 0, excluded: 3, total: 4}
    assert output =~ "\n4 tests, 0 failures, 3 excluded\n"

    {result, output} = run_with_filter([exclude: :even, include: [even: true]], [ParityTest])
    assert result == %{failures: 1, skipped: 0, excluded: 2, total: 4}
    assert output =~ "\n4 tests, 1 failure, 2 excluded\n"

    {result, output} = run_with_filter([exclude: :test, include: [even: true]], [ParityTest])
    assert result == %{failures: 1, skipped: 0, excluded: 3, total: 4}
    assert output =~ "\n4 tests, 1 failure, 3 excluded\n"
  end

  test "log capturing" do
    defmodule LogCapturingTest do
      use ExUnit.Case
      require Logger

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
    end

    output = capture_io(&ExUnit.run/0)
    refute output =~ "[debug] one\n"
    assert output =~ "[debug] two\n"
    assert output =~ "[debug] three\n"
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
                {:error, e, __STACKTRACE__}
            end

          error2 =
            try do
              assert 3 > 4
            rescue
              e in ExUnit.AssertionError ->
                {:error, e, __STACKTRACE__}
            end

          raise ExUnit.MultiError, errors: [error1, error2]
        end
      end
    end)

    configure_and_reload_on_exit([])

    output =
      capture_io(fn ->
        assert ExUnit.run() == %{failures: 1, skipped: 0, total: 1, excluded: 0}
      end)

    assert output =~ "\n1 test, 1 failure\n"
    assert output =~ "\n  1) test multi (ExUnitTest.MultiTest)\n"
    assert output =~ "Failure #1\n"
    assert output =~ "Failure #2\n"

    assert_raise ExUnit.MultiError, ~r/oops/, fn ->
      stack =
        try do
          raise("oops")
        rescue
          _ -> __STACKTRACE__
        end

      error = {:error, RuntimeError.exception("oops"), stack}
      raise ExUnit.MultiError, errors: [error]
    end
  end

  test "raises friendly error for duplicate test names" do
    message = ~S("test duplicate" is already defined in ExUnitTest.TestWithSameNames)

    assert_raise ArgumentError, message, fn ->
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

    configure_and_reload_on_exit([])

    output =
      capture_io(fn ->
        assert ExUnit.run() == %{failures: 1, skipped: 0, total: 1, excluded: 0}
      end)

    assert output =~ "Not implemented\n"
    assert output =~ "\n1 test, 1 failure\n"
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

    configure_and_reload_on_exit([])

    output =
      capture_io(fn ->
        assert ExUnit.run() == %{failures: 0, skipped: 2, total: 2, excluded: 0}
      end)

    assert output =~ "\n2 tests, 0 failures, 2 skipped\n"
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
    assert result == %{failures: 0, skipped: 0, excluded: 2, total: 2}
    assert output =~ "\n2 tests, 0 failures, 2 excluded\n"

    {result, output} =
      [exclude: :test, include: [module: "ExUnitTest.SecondTestModule"]]
      |> run_with_filter([FirstTestModule, SecondTestModule])

    assert result == %{failures: 1, skipped: 0, excluded: 1, total: 2}
    assert output =~ "\n  1) test false (ExUnitTest.SecondTestModule)\n"
    assert output =~ "\n2 tests, 1 failure, 1 excluded\n"
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

    configure_and_reload_on_exit([])

    output =
      capture_io(fn ->
        assert ExUnit.run() == %{failures: 1, skipped: 0, total: 1, excluded: 0}
      end)

    assert output =~ "trying to set reserved field :file"
  end

  test "removes new lines from multiline test name (with --trace option)" do
    defmodule Multiline do
      use ExUnit.Case

      test """
      - line 1
      - line 2
      """ do
        :ok
      end
    end

    configure_and_reload_on_exit(trace: true)

    output = capture_io(fn -> ExUnit.run() end)

    assert output =~ "test - line 1 - line 2"
  end

  test "raises on reserved tag :async in setup" do
    defmodule ReservedSetupTagAsync do
      use ExUnit.Case

      setup do
        {:ok, async: true}
      end

      test "sample", do: :ok
    end

    output =
      capture_io(fn ->
        assert ExUnit.run() == %{failures: 1, skipped: 0, total: 1, excluded: 0}
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

    capture_io(fn ->
      assert ExUnit.run() == %{failures: 0, skipped: 0, total: 1, excluded: 0}
    end)
  end

  test "seed is predictable and different for each test" do
    defmodule PredictableSeedTest do
      use ExUnit.Case, async: true

      test "generated seed is always the same in the same module and test" do
        assert :rand.uniform(1_000_000) == 253_740
      end

      test "generated seed is different for other test" do
        assert :rand.uniform(1_000_000) == 462_665
      end
    end

    defmodule DifferentModuleWithDifferentSeedTest do
      use ExUnit.Case, async: true

      test "generated seed is always the same in the same module and test" do
        assert :rand.uniform(1_000_000) == 563_517
      end
    end

    configure_and_reload_on_exit(seed: 1)

    assert capture_io(fn ->
             assert ExUnit.run() == %{failures: 0, skipped: 0, total: 3, excluded: 0}
           end) =~ "\n3 tests, 0 failures\n"
  end

  # Skipped and excluded tests should be included in the stats
  # as well as printed to stdout. On the other hand, invalid tests
  # should be marked as failures in the stats, but still be printed
  # as "invalid" to stdout.
  #
  # If setup_all fails, the skipped and excluded tests should not be
  # counted as invalid or failures.
  test "setup_all fails and module has skipped and excluded tests" do
    defmodule SetupAllFailsModuleHasSkippedExcludedTest do
      use ExUnit.Case

      setup_all do
        raise "oops"
      end

      @tag :skip
      test "skipped #{__ENV__.line}", do: assert(false)

      test "pass #{__ENV__.line}", do: assert(true)
      test "pass #{__ENV__.line}", do: assert(true)
      test "fail #{__ENV__.line}", do: assert(false)
      test "fail #{__ENV__.line}", do: assert(false)

      @tag :exclude
      test "excluded #{__ENV__.line}", do: assert(false)
    end

    configure_and_reload_on_exit([])

    output =
      capture_io(fn ->
        assert ExUnit.run() == %{total: 6, failures: 4, excluded: 1, skipped: 1}
      end)

    refute output =~ max_failures_reached_msg()
    assert output =~ "\n6 tests, 0 failures, 1 excluded, 4 invalid, 1 skipped\n"
  end

  test "parameterized tests" do
    Process.register(self(), :parameterized_tests)

    defmodule ParameterizedTests do
      use ExUnit.Case, async: true, parameterize: [%{value: true}, %{value: false}]

      @tag :tmp_dir
      test "hello world", %{value: value, tmp_dir: tmp_dir} do
        send(:parameterized_tests, {:tmp_dir, tmp_dir})
        assert value
      end
    end

    configure_and_reload_on_exit(trace: true)

    output = capture_io(fn -> ExUnit.run() end)

    assert output =~ ~r"""
           ExUnitTest.ParameterizedTests \[.*test/ex_unit_test.exs\]
           Parameters: %\{value: false\}
           """

    assert output =~ """
             1) test hello world (ExUnitTest.ParameterizedTests)
                Parameters: %{value: false}
           """

    # Check that the temporary paths are different
    assert_receive {:tmp_dir, tmp_dir1}
    assert_receive {:tmp_dir, tmp_dir2} when tmp_dir1 != tmp_dir2
  end

  describe "after_suite/1" do
    test "executes all callbacks set in reverse order" do
      Process.register(self(), :after_suite_test_process)

      defmodule MultipleAfterSuiteTest do
        use ExUnit.Case

        test "true" do
          send(:after_suite_test_process, :in_first_test)
        end
      end

      ExUnit.after_suite(fn _ -> send(:after_suite_test_process, :first_after_suite) end)
      ExUnit.after_suite(fn result -> send(:after_suite_test_process, result) end)
      ExUnit.after_suite(fn _ -> send(:after_suite_test_process, :third_after_suite) end)

      capture_io(fn -> ExUnit.run() end)

      # Because `after_suite` is global, we need to be sure to clear out the
      # test callbacks here, otherwise it will attempt to execute them after
      # every subsequent call to `ExUnit.run()` in any tests run after these.
      Application.put_env(:ex_unit, :after_suite, [])

      assert next_message_in_mailbox() == :in_first_test
      assert next_message_in_mailbox() == :third_after_suite
      assert next_message_in_mailbox() == %{excluded: 0, failures: 0, skipped: 0, total: 1}
      assert next_message_in_mailbox() == :first_after_suite
      # Check to make sure the mailbox is empty after these four messages
      refute_received _
    end
  end

  describe ":max_failures" do
    test "default value to :infinity" do
      configure_and_reload_on_exit([])
      ExUnit.start(autorun: false)
      config = ExUnit.configuration()
      assert config[:max_failures] == :infinity
    end

    test "sets value of :max_failures" do
      configure_and_reload_on_exit([])
      ExUnit.start(max_failures: 5, autorun: false)
      config = ExUnit.configuration()
      assert config[:max_failures] == 5
    end

    test ":max_failures are reached" do
      defmodule TestMaxFailuresReached do
        use ExUnit.Case

        @tag :skip
        test "skipped #{__ENV__.line}", do: assert(false)

        test __ENV__.line, do: assert(true)
        test __ENV__.line, do: assert(true)
        test __ENV__.line, do: assert(false)
        test __ENV__.line, do: assert(false)
        test __ENV__.line, do: assert(true)

        @tag :exclude
        test "excluded #{__ENV__.line}", do: assert(false)
      end

      configure_and_reload_on_exit(max_failures: 2)

      output =
        capture_io(fn ->
          assert ExUnit.run() == %{total: 6, failures: 2, skipped: 1, excluded: 1}
        end)

      assert output =~ max_failures_reached_msg()
      assert output =~ "\n6 tests, 2 failures, 1 excluded, 1 skipped\n"
    end

    test ":max_failures is not reached" do
      defmodule TestMaxFailuresNotReached do
        use ExUnit.Case

        @tag :skip
        test "skipped #{__ENV__.line}", do: assert(true)

        test "pass #{__ENV__.line}", do: assert(true)
        test "fail #{__ENV__.line}", do: assert(false)
        test "pass #{__ENV__.line}", do: assert(true)
        test "fail #{__ENV__.line}", do: assert(false)
        test "pass #{__ENV__.line}", do: assert(true)

        @tag :exclude
        test "excluded #{__ENV__.line}", do: assert(true)

        @tag :exclude
        test "excluded #{__ENV__.line}", do: assert(false)
      end

      configure_and_reload_on_exit(max_failures: 3)

      output =
        capture_io(fn ->
          assert ExUnit.run() == %{total: 8, excluded: 2, failures: 2, skipped: 1}
        end)

      refute output =~ max_failures_reached_msg()
      assert output =~ "\n8 tests, 2 failures, 2 excluded, 1 skipped\n"
    end

    test ":max_failures has been reached" do
      defmodule TestMaxFailuresAlreadyReached do
        use ExUnit.Case

        @tag :skip
        test "skipped #{__ENV__.line}", do: assert(true)

        @tag :skip
        test "skipped #{__ENV__.line}", do: assert(false)

        test "pass #{__ENV__.line}", do: assert(true)
        test "fail #{__ENV__.line}", do: assert(false)
        test "fail #{__ENV__.line}", do: assert(false)
        test "fail #{__ENV__.line}", do: assert(false)
        test "pass #{__ENV__.line}", do: assert(true)

        @tag :exclude
        test "excluded #{__ENV__.line}", do: assert(true)

        @tag :exclude
        test "excluded #{__ENV__.line}", do: assert(true)
      end

      configure_and_reload_on_exit(max_failures: 2)

      output =
        capture_io(fn ->
          assert ExUnit.run() == %{total: 7, failures: 2, excluded: 2, skipped: 2}
        end)

      assert output =~ max_failures_reached_msg()
      assert output =~ "\n7 tests, 2 failures, 2 excluded, 2 skipped\n"
    end

    # Excluded and skipped tests are detected before setup_all
    # callback is executed, therefore they are always included
    # as part of the total number of tests in the stats.
    test ":max_failures on setup_all errors" do
      defmodule TestMaxFailuresSetupAll do
        use ExUnit.Case

        setup_all do
          raise "oops"
        end

        @tag :skip
        test "skipped #{__ENV__.line}", do: assert(true)

        test "pass #{__ENV__.line}", do: assert(true)
        test "pass #{__ENV__.line}", do: assert(true)
        test "fail #{__ENV__.line}", do: assert(false)
        test "fail #{__ENV__.line}", do: assert(false)

        @tag :exclude
        test "excluded #{__ENV__.line}", do: assert(false)
      end

      configure_and_reload_on_exit(max_failures: 2)

      output =
        capture_io(fn ->
          assert ExUnit.run() == %{total: 4, failures: 2, excluded: 1, skipped: 1}
        end)

      assert output =~ max_failures_reached_msg()
      assert output =~ "\n4 tests, 0 failures, 1 excluded, 2 invalid, 1 skipped\n"
    end

    test ":max_failures flushes all async/sync cases" do
      defmodule TestMaxFailuresAsync1 do
        use ExUnit.Case, async: true
        test "error", do: assert(false)
      end

      defmodule TestMaxFailuresAsync2 do
        use ExUnit.Case, async: true
        test "error", do: assert(false)
      end

      defmodule TestMaxFailuresSync do
        use ExUnit.Case
        test "error", do: assert(false)
      end

      configure_and_reload_on_exit(max_failures: 1)

      output =
        capture_io(fn ->
          assert ExUnit.run() == %{total: 1, failures: 1, excluded: 0, skipped: 0}
        end)

      assert output =~ max_failures_reached_msg()
      assert output =~ "\n1 test, 1 failure\n"

      capture_io(fn ->
        assert ExUnit.run() == %{total: 0, failures: 0, excluded: 0, skipped: 0}
      end)
    end

    test "warns and errors when context is not a map" do
      assert capture_io(:stderr, fn ->
               defmodule ContextTest do
                 use ExUnit.Case, async: true

                 test "I made a typo", conn: conn do
                   assert true
                 end
               end
             end) =~ "test context is always a map. The pattern \"[conn: conn]\" will never match"

      configure_and_reload_on_exit([])

      assert capture_io(fn ->
               assert ExUnit.run() == %{failures: 1, skipped: 0, total: 1, excluded: 0}
             end) =~ "** (FunctionClauseError) no function clause matching"
    end
  end

  describe ":exit_status" do
    test "defaults value to 2" do
      configure_and_reload_on_exit([])
      ExUnit.start(autorun: false)
      config = ExUnit.configuration()
      assert config[:exit_status] == 2
    end

    test "sets value of :exit_status" do
      configure_and_reload_on_exit([])
      ExUnit.start(exit_status: 5, autorun: false)
      config = ExUnit.configuration()
      assert config[:exit_status] == 5
    end
  end

  describe ":repeat_until_failure" do
    test "defaults to 0" do
      configure_and_reload_on_exit([])
      ExUnit.start(autorun: false)
      config = ExUnit.configuration()
      assert config[:repeat_until_failure] == 0
    end

    test "sets value of :repeat_until_failure" do
      configure_and_reload_on_exit([])
      ExUnit.start(repeat_until_failure: 5, autorun: false)
      config = ExUnit.configuration()
      assert config[:repeat_until_failure] == 5
    end

    test "repeats tests up to the configured number of times" do
      defmodule TestRepeatUntilFailureReached do
        use ExUnit.Case

        @tag :skip
        test "skipped #{__ENV__.line}", do: assert(false)

        test __ENV__.line, do: assert(true)
        test __ENV__.line, do: assert(true)
        test __ENV__.line, do: assert(true)

        @tag :exclude
        test "excluded #{__ENV__.line}", do: assert(false)
      end

      configure_and_reload_on_exit(repeat_until_failure: 5)

      output =
        capture_io(fn ->
          assert ExUnit.run() == %{total: 5, failures: 0, skipped: 1, excluded: 1}
        end)

      runs = String.split(output, "Running ExUnit", trim: true)
      # 6 runs in total, 5 repeats
      assert length(runs) == 6
    end

    test "stops on failure" do
      {:ok, pid} = Agent.start_link(fn -> 0 end)
      Process.register(pid, :ex_unit_repeat_until_failure_count)

      defmodule TestRepeatUntilFailureFailure do
        use ExUnit.Case

        @tag :skip
        test "skipped #{__ENV__.line}", do: assert(true)

        test "maybe pass #{__ENV__.line}" do
          count = Agent.get(:ex_unit_repeat_until_failure_count, & &1)

          if count < 3 do
            Agent.update(:ex_unit_repeat_until_failure_count, &(&1 + 1))
            assert(true)
          else
            assert(false)
          end
        end

        @tag :exclude
        test "excluded #{__ENV__.line}", do: assert(true)

        @tag :exclude
        test "excluded #{__ENV__.line}", do: assert(false)
      end

      configure_and_reload_on_exit(repeat_until_failure: 5)

      output =
        capture_io(fn ->
          assert ExUnit.run() == %{total: 4, excluded: 2, failures: 1, skipped: 1}
        end)

      runs = String.split(output, "Running ExUnit", trim: true)
      # four runs in total, the first two repeats work fine, the third repeat (4th run)
      # fails, therefore we stop
      assert length(runs) == 4
      assert List.last(runs) =~ "Expected truthy, got false"
    end
  end

  test "prints warning when all tests are excluded" do
    defmodule OnlyExcludedTests do
      use ExUnit.Case

      @tag :exclude
      test "excluded test", do: assert(false)

      @tag :exclude
      test "one more excluded test", do: assert(false)
    end

    configure_and_reload_on_exit([])

    output =
      capture_io(fn ->
        assert ExUnit.run() == %{total: 2, failures: 0, excluded: 2, skipped: 0}
      end)

    assert output =~ "All tests have been excluded.\n"
    assert output =~ "2 tests, 0 failures, 2 excluded\n"
  end

  test "tests are run in compile order (FIFO)" do
    defmodule FirstTestFIFO do
      use ExUnit.Case

      test "first test" do
        assert true
      end
    end

    defmodule SecondTestFIFO do
      use ExUnit.Case

      test "second test" do
        assert true
      end
    end

    defmodule ThirdTestFIFO do
      use ExUnit.Case

      test "third test" do
        assert true
      end
    end

    configure_and_reload_on_exit(trace: true)

    output =
      capture_io(fn ->
        assert ExUnit.run() == %{total: 3, failures: 0, excluded: 0, skipped: 0}
      end)

    [_, first, second, third | _] = String.split(output, "\n\n")

    assert first =~ "FirstTestFIFO"
    assert second =~ "SecondTestFIFO"
    assert third =~ "ThirdTestFIFO"
  end

  test "groups are run in compile order (FIFO)" do
    defmodule RedOneFIFO do
      use ExUnit.Case, async: true, group: :red

      test "red one test" do
        Process.sleep(5)
        assert true
      end
    end

    defmodule BlueOneFIFO do
      use ExUnit.Case, async: true, group: :blue

      test "blue one test" do
        Process.sleep(5)
        assert true
      end
    end

    defmodule RedTwoFIFO do
      use ExUnit.Case, async: true, group: :red

      test "red two test" do
        assert true
      end
    end

    defmodule BlueTwoFIFO do
      use ExUnit.Case, async: true, group: :blue

      test "blue two test" do
        assert true
      end
    end

    configure_and_reload_on_exit(trace: true, max_cases: 2)

    output =
      capture_io(fn ->
        assert ExUnit.run() == %{total: 4, failures: 0, excluded: 0, skipped: 0}
      end)

    [_, first, second, third, fourth | _] = String.split(output, "\n\n")

    assert first =~ "RedOneFIFO"
    assert second =~ "RedTwoFIFO"
    assert third =~ "BlueOneFIFO"
    assert fourth =~ "BlueTwoFIFO"
  end

  test "filters async tests" do
    defmodule FirstTestAsyncTrue do
      use ExUnit.Case, async: true

      test "first test" do
        assert true
      end
    end

    defmodule SecondTestAsyncTrue do
      use ExUnit.Case, async: true

      test "second test" do
        assert true
      end
    end

    defmodule FirstTestAsyncFalse do
      use ExUnit.Case, async: false

      test "first test" do
        assert true
      end
    end

    assert {%{failures: 0, skipped: 0, total: 3, excluded: 1}, _} =
             run_with_filter([include: [async: true], exclude: [:test]], [])

    assert {%{failures: 0, skipped: 0, total: 3, excluded: 2}, _} =
             run_with_filter(
               [include: [async: false], exclude: [:test]],
               [FirstTestAsyncTrue, SecondTestAsyncTrue, FirstTestAsyncFalse]
             )
  end

  ##  Helpers

  defp run_with_filter(filters, cases) do
    Enum.each(cases, fn mod ->
      ExUnit.Server.add_module(mod, mod.__ex_unit__(:config))
    end)

    ExUnit.Server.modules_loaded(false)

    opts =
      ExUnit.configuration()
      |> Keyword.merge(filters)
      |> Keyword.merge(colors: [enabled: false])

    with_io(fn -> ExUnit.Runner.run(opts, nil) |> elem(0) end)
  end

  defp next_message_in_mailbox() do
    receive do
      msg -> msg
    after
      0 -> nil
    end
  end

  defp configure_and_reload_on_exit(opts) do
    old_opts = ExUnit.configuration()

    ExUnit.configure(
      Keyword.merge(
        [autorun: false, seed: 0, colors: [enabled: false], exclude: [:exclude]],
        opts
      )
    )

    on_exit(fn -> ExUnit.configure(old_opts) end)
  end

  defp max_failures_reached_msg() do
    "--max-failures reached, aborting test suite"
  end
end
