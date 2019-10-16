Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.TestTest do
  use MixTest.Case

  describe "ex_unit_opts/1" do
    test "returns ex unit options" do
      assert ex_unit_opts_from_given(unknown: "ok", seed: 13) == [seed: 13]
    end

    test "returns includes and excludes" do
      included = [include: [:focus, key: "val"]]
      assert ex_unit_opts_from_given(include: "focus", include: "key:val") == included

      excluded = [exclude: [:focus, key: "val"]]
      assert ex_unit_opts_from_given(exclude: "focus", exclude: "key:val") == excluded
    end

    test "translates :only into includes and excludes" do
      assert ex_unit_opts_from_given(only: "focus") == [include: [:focus], exclude: [:test]]

      only = [include: [:focus, :special], exclude: [:test]]
      assert ex_unit_opts_from_given(only: "focus", include: "special") == only
    end

    test "translates :color into list containing an enabled key/value pair" do
      assert ex_unit_opts_from_given(color: false) == [colors: [enabled: false]]
      assert ex_unit_opts_from_given(color: true) == [colors: [enabled: true]]
    end

    test "translates :formatter into list of modules" do
      assert ex_unit_opts_from_given(formatter: "A.B") == [formatters: [A.B]]
    end

    test "includes some default options" do
      assert ex_unit_opts([]) == [
               autorun: false,
               failures_manifest_file:
                 Path.join(Mix.Project.manifest_path(), ".mix_test_failures")
             ]
    end

    defp ex_unit_opts(opts) do
      {ex_unit_opts, _allowed_files} = Mix.Tasks.Test.process_ex_unit_opts(opts)
      ex_unit_opts
    end

    defp ex_unit_opts_from_given(passed) do
      passed
      |> ex_unit_opts()
      |> Keyword.drop([:failures_manifest_file, :autorun])
    end
  end

  describe "--stale" do
    test "runs all tests for first run, then none on second" do
      in_fixture("test_stale", fn ->
        assert_stale_run_output("2 tests, 0 failures")

        assert_stale_run_output("""
        No stale tests
        """)
      end)
    end

    test "runs tests that depend on modified modules" do
      in_fixture("test_stale", fn ->
        assert_stale_run_output("2 tests, 0 failures")

        set_all_mtimes()
        File.touch!("lib/b.ex")

        assert_stale_run_output("1 test, 0 failures")

        set_all_mtimes()
        File.touch!("lib/a.ex")

        assert_stale_run_output("2 tests, 0 failures")
      end)
    end

    test "doesn't write manifest when there are failures" do
      in_fixture("test_stale", fn ->
        assert_stale_run_output("2 tests, 0 failures")

        set_all_mtimes()

        File.write!("lib/b.ex", """
        defmodule B do
          def f, do: :error
        end
        """)

        assert_stale_run_output("1 test, 1 failure")

        assert_stale_run_output("1 test, 1 failure")
      end)
    end

    test "runs tests that have changed" do
      in_fixture("test_stale", fn ->
        assert_stale_run_output("2 tests, 0 failures")

        set_all_mtimes()
        File.touch!("test/a_test_stale.exs")

        assert_stale_run_output("1 test, 0 failures")
      end)
    end

    test "runs tests that have changed test_helpers" do
      in_fixture("test_stale", fn ->
        assert_stale_run_output("2 tests, 0 failures")

        set_all_mtimes()
        File.touch!("test/test_helper.exs")

        assert_stale_run_output("2 tests, 0 failures")
      end)
    end

    test "runs all tests no matter what with --force" do
      in_fixture("test_stale", fn ->
        assert_stale_run_output("2 tests, 0 failures")

        assert_stale_run_output(~w[--force], "2 tests, 0 failures")
      end)
    end
  end

  describe "--cover" do
    test "reports the coverage of each app's modules in an umbrella" do
      in_fixture("umbrella_test", fn ->
        output = mix(["test", "--cover"])

        # For bar, we do regular --cover and also test protocols
        assert output =~ """
               Generating cover results ...

               Percentage | Module
               -----------|--------------------------
                  100.00% | Bar
                  100.00% | Bar.Protocol.BitString
               -----------|--------------------------
                  100.00% | Total
               """

        # For foo, we do regular --cover and test it does not include bar
        assert output =~ """
               Generating cover results ...

               Percentage | Module
               -----------|--------------------------
                  100.00% | Foo
               -----------|--------------------------
                  100.00% | Total
               """
      end)
    end
  end

  describe "--failed" do
    test "loads only files with failures and runs just the failures" do
      in_fixture("test_failed", fn ->
        loading_only_passing_test_msg = "loading OnlyPassingTest"

        # Run `mix test` once to record failures...
        output = mix(["test"])
        assert output =~ loading_only_passing_test_msg
        assert output =~ "4 tests, 2 failures"

        # `mix test --failed` runs only failed tests and avoids loading files with no failures
        output = mix(["test", "--failed"])
        refute output =~ loading_only_passing_test_msg
        assert output =~ "2 tests, 2 failures"

        # `mix test --failed` can be applied to a directory or file
        output = mix(["test", "test/passing_and_failing_test_failed.exs", "--failed"])
        assert output =~ "1 test, 1 failure"

        # `--failed` composes with an `--only` filter by running the intersection.
        # Of the failing tests, 1 is tagged with `@tag :foo`.
        # Of the passing tests, 1 is tagged with `@tag :foo`.
        # But only the failing test with that tag should run.
        output = mix(["test", "--failed", "--only", "foo"])
        assert output =~ "2 tests, 1 failure, 1 excluded"

        # Run again to give it a chance to record as passed
        System.put_env("PASS_FAILING_TESTS", "true")
        assert mix(["test", "--failed"]) =~ "2 tests, 0 failures"

        # Nothing should get run if we try it again since everything is passing.
        assert mix(["test", "--failed"]) =~ "There are no tests to run"

        # `--failed` and `--stale` cannot be combined
        output = mix(["test", "--failed", "--stale"])
        assert output =~ "Combining --failed and --stale is not supported"
      end)
    after
      System.delete_env("PASS_FAILING_TESTS")
    end
  end

  describe "--listen-on-stdin" do
    test "runs tests after input" do
      in_fixture("test_stale", fn ->
        port = mix_port(~w[test --stale --listen-on-stdin])

        assert receive_until_match(port, "seed", "") =~ "2 tests"

        Port.command(port, "\n")

        assert receive_until_match(port, "No stale tests", "") =~ "Restarting..."
      end)
    end

    test "does not exit on compilation failure" do
      in_fixture("test_stale", fn ->
        File.write!("lib/b.ex", """
        defmodule B do
          def f, do: error_not_a_var
        end
        """)

        port = mix_port(~w[test --listen-on-stdin])

        assert receive_until_match(port, "error", "") =~ "lib/b.ex"

        File.write!("lib/b.ex", """
        defmodule B do
          def f, do: A.f
        end
        """)

        Port.command(port, "\n")

        assert receive_until_match(port, "seed", "") =~ "2 tests"

        File.write!("test/b_test_stale.exs", """
        defmodule BTest do
          use ExUnit.Case

          test "f" do
            assert B.f() == error_not_a_var
          end
        end
        """)

        Port.command(port, "\n")

        message = "undefined function error_not_a_var"
        assert receive_until_match(port, message, "") =~ "test/b_test_stale.exs"

        File.write!("test/b_test_stale.exs", """
        defmodule BTest do
          use ExUnit.Case

          test "f" do
            assert B.f() == :ok
          end
        end
        """)

        Port.command(port, "\n")

        assert receive_until_match(port, "seed", "") =~ "2 tests"
      end)
    end
  end

  describe "--partitions" do
    test "splits tests into partitions" do
      in_fixture("test_stale", fn ->
        assert mix(["test", "--partitions", "3"], [{"MIX_TEST_PARTITION", "1"}]) =~
                 "1 test, 0 failures"

        assert mix(["test", "--partitions", "3"], [{"MIX_TEST_PARTITION", "2"}]) =~
                 "1 test, 0 failures"

        assert mix(["test", "--partitions", "3"], [{"MIX_TEST_PARTITION", "3"}]) =~
                 "There are no tests to run"
      end)
    end
  end

  describe "logs and errors" do
    test "logs test absence for a project with no test paths" do
      in_fixture("test_stale", fn ->
        File.rm_rf!("test")

        assert_run_output("There are no tests to run")
      end)
    end

    test "raises when no test runs even with Mix.shell() change" do
      in_fixture("test_stale", fn ->
        File.write!("test/test_helper.exs", """
        Mix.shell(Mix.Shell.Process)
        ExUnit.start()
        """)

        assert_run_output(
          ["--only", "unknown"],
          "The --only option was given to \"mix test\" but no test was executed"
        )
      end)
    end

    test "raises an exception if line numbers are given with multiple files" do
      in_fixture("test_stale", fn ->
        assert_run_output(
          [
            "test/a_test_stale.exs",
            "test/b_test_stale.exs:4"
          ],
          "Line numbers can only be used when running a single test file"
        )
      end)
    end

    test "umbrella with file path" do
      in_fixture("umbrella_test", fn ->
        # Run false positive test first so at least the code is compiled
        # and we can perform more aggressive assertions later
        assert mix(["test", "apps/unknown_app/test"]) =~ """
               ==> bar
               Paths given to "mix test" did not match any directory/file: apps/unknown_app/test
               ==> foo
               Paths given to "mix test" did not match any directory/file: apps/unknown_app/test
               """

        output = mix(["test", "apps/bar/test/bar_tests.exs"])

        assert output =~ """
               ==> bar
               .
               """

        refute output =~ "==> foo"
        refute output =~ "Paths given to \"mix test\" did not match any directory/file"

        output = mix(["test", "apps/bar/test/bar_tests.exs:10"])

        assert output =~ """
               ==> bar
               Excluding tags: [:test]
               Including tags: [line: \"10\"]

               .
               """

        refute output =~ "==> foo"
        refute output =~ "Paths given to \"mix test\" did not match any directory/file"
      end)
    end
  end

  defp receive_until_match(port, expected, acc) do
    receive do
      {^port, {:data, output}} ->
        acc = acc <> output

        if output =~ expected do
          acc
        else
          receive_until_match(port, expected, acc)
        end
    end
  end

  defp set_all_mtimes(time \\ {{2010, 1, 1}, {0, 0, 0}}) do
    Enum.each(Path.wildcard("**", match_dot: true), &File.touch!(&1, time))
  end

  defp assert_stale_run_output(opts \\ [], expected) do
    assert_run_output(["--stale" | opts], expected)
  end

  defp assert_run_output(opts \\ [], expected) do
    assert mix(["test" | opts]) =~ expected
  end
end
