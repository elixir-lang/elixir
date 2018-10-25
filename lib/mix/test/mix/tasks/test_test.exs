Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.TestTest do
  use MixTest.Case

  test "ex_unit_opts/1 returns ex unit options" do
    assert ex_unit_opts_from_given(unknown: "ok", seed: 13) == [seed: 13]
  end

  test "ex_unit_opts/1 returns includes and excludes" do
    included = [include: [:focus, key: "val"]]
    assert ex_unit_opts_from_given(include: "focus", include: "key:val") == included

    excluded = [exclude: [:focus, key: "val"]]
    assert ex_unit_opts_from_given(exclude: "focus", exclude: "key:val") == excluded
  end

  test "ex_unit_opts/1 translates :only into includes and excludes" do
    assert ex_unit_opts_from_given(only: "focus") == [include: [:focus], exclude: [:test]]

    only = [include: [:focus, :special], exclude: [:test]]
    assert ex_unit_opts_from_given(only: "focus", include: "special") == only
  end

  test "ex_unit_opts/1 translates :color into list containing an enabled key/value pair" do
    assert ex_unit_opts_from_given(color: false) == [colors: [enabled: false]]
    assert ex_unit_opts_from_given(color: true) == [colors: [enabled: true]]
  end

  test "ex_unit_opts/1 translates :formatter into list of modules" do
    assert ex_unit_opts_from_given(formatter: "A.B") == [formatters: [A.B]]
  end

  test "ex_unit_opts/1 includes some default options" do
    assert ex_unit_opts([]) == [
             autorun: false,
             failures_manifest_file: Path.join(Mix.Project.manifest_path(), ".mix_test_failures")
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

  test "--stale: runs all tests for first run, then none on second" do
    in_fixture("test_stale", fn ->
      assert_stale_run_output("2 tests, 0 failures")

      assert_stale_run_output("""
      No stale tests
      """)
    end)
  end

  test "--stale: runs tests that depend on modified modules" do
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

  test "--stale: doesn't write manifest when there are failures" do
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

  test "--stale: runs tests that have changed" do
    in_fixture("test_stale", fn ->
      assert_stale_run_output("2 tests, 0 failures")

      set_all_mtimes()
      File.touch!("test/a_test_stale.exs")

      assert_stale_run_output("1 test, 0 failures")
    end)
  end

  test "--stale: runs tests that have changed test_helpers" do
    in_fixture("test_stale", fn ->
      assert_stale_run_output("2 tests, 0 failures")

      set_all_mtimes()
      File.touch!("test/test_helper.exs")

      assert_stale_run_output("2 tests, 0 failures")
    end)
  end

  test "--stale: runs all tests no matter what with --force" do
    in_fixture("test_stale", fn ->
      assert_stale_run_output("2 tests, 0 failures")

      assert_stale_run_output(~w[--force], "2 tests, 0 failures")
    end)
  end

  test "--cover: in an umbrella application, reports on the coverage of each app's modules exactly once" do
    in_fixture("umbrella_cover", fn ->
      output = mix(["test", "--cover"])

      occurrences_of_bar_coverage =
        output
        |> String.split("100.00% | Bar")
        |> length()
        |> (&(&1 - 1)).()

      occurrences_of_foo_coverage =
        output
        |> String.split("100.00% | Foo")
        |> length()
        |> (&(&1 - 1)).()

      assert occurrences_of_bar_coverage == 1
      assert occurrences_of_foo_coverage == 1
    end)
  end

  test "--failed: loads only files with failures and runs just the failures" do
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

  test "logs test absence for a project with no test paths" do
    in_fixture("test_stale", fn ->
      File.rm_rf!("test")

      assert_run_output("There are no tests to run")
    end)
  end

  test "--listen-on-stdin: runs tests after input" do
    in_fixture("test_stale", fn ->
      port = mix_port(~w[test --stale --listen-on-stdin])

      assert receive_until_match(port, "seed", "") =~ "2 tests"

      Port.command(port, "\n")

      assert receive_until_match(port, "No stale tests", "") =~ "Restarting..."
    end)
  end

  test "--listen-on-stdin: does not exit on compilation failure" do
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
