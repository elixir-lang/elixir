Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.TestTest do
  use MixTest.Case

  import Mix.Tasks.Test, only: [ex_unit_opts: 1]

  test "ex_unit_opts/1 returns ex unit options" do
    assert ex_unit_opts([unknown: "ok", seed: 13]) ==
           [autorun: false, seed: 13]
  end

  test "ex_unit_opts/1 returns includes and excludes" do
    assert ex_unit_opts([include: "focus", include: "key:val"]) ==
           [autorun: false, include: [:focus, key: "val"]]

    assert ex_unit_opts([exclude: "focus", exclude: "key:val"]) ==
           [autorun: false, exclude: [:focus, key: "val"]]
  end

  test "ex_unit_opts/1 translates :only into includes and excludes" do
    assert ex_unit_opts([only: "focus"]) ==
           [autorun: false, include: [:focus], exclude: [:test]]

    assert ex_unit_opts([only: "focus", include: "special"]) ==
           [autorun: false, include: [:focus, :special], exclude: [:test]]
  end

  test "ex_unit_opts/1 translates :color into list containing an enabled key/value pair" do
    assert ex_unit_opts([color: false]) == [autorun: false, colors: [enabled: false]]
    assert ex_unit_opts([color: true]) == [autorun: false, colors: [enabled: true]]
  end

  test "ex_unit_opts/1 translates :formatter into list of modules" do
    assert ex_unit_opts([formatter: "A.B"]) == [autorun: false, formatters: [A.B]]
  end

  test "--stale: runs all tests for first run, then none on second" do
    in_fixture "test_stale", fn ->
      assert_stale_run_output "2 tests, 0 failures"

      assert_stale_run_output """
      No stale tests
      """
    end
  end

  test "--stale: runs tests that depend on modified modules" do
    in_fixture "test_stale", fn ->
      assert_stale_run_output "2 tests, 0 failures"

      set_all_mtimes()
      File.touch!("lib/b.ex")

      assert_stale_run_output "1 test, 0 failures"

      set_all_mtimes()
      File.touch!("lib/a.ex")

      assert_stale_run_output "2 tests, 0 failures"
    end
  end

  test "--stale: doesn't write manifest when there are failures" do
    in_fixture "test_stale", fn ->
      assert_stale_run_output "2 tests, 0 failures"

      set_all_mtimes()
      File.write!("lib/b.ex", """
      defmodule B do
        def f, do: :error
      end
      """)

      assert_stale_run_output "1 test, 1 failure"

      assert_stale_run_output "1 test, 1 failure"
    end
  end

  test "--stale: runs tests that have changed" do
    in_fixture "test_stale", fn ->
      assert_stale_run_output "2 tests, 0 failures"

      set_all_mtimes()
      File.touch!("test/a_test_stale.exs")

      assert_stale_run_output "1 test, 0 failures"
    end
  end

  test "--stale: runs tests that have changed test_helpers" do
    in_fixture "test_stale", fn ->
      assert_stale_run_output "2 tests, 0 failures"

      set_all_mtimes()
      File.touch!("test/test_helper.exs")

      assert_stale_run_output "2 tests, 0 failures"
    end
  end

  test "--stale: runs all tests no matter what with --force" do
    in_fixture "test_stale", fn ->
      assert_stale_run_output "2 tests, 0 failures"

      assert_stale_run_output ~w[--force], "2 tests, 0 failures"
    end
  end

  test "logs test absence for a project with no test paths" do
    in_fixture "test_stale", fn ->
      File.rm_rf! "test"

      assert_run_output "There are no tests to run"
    end
  end

  test "--listen-on-stdin: runs tests after input" do
    in_fixture "test_stale", fn ->
      port = mix_port(~w[test --stale --listen-on-stdin])

      assert receive_until_match(port, "seed", []) =~ "2 tests"

      Port.command(port, "\n")

      assert receive_until_match(port, "No stale tests", []) =~ "Restarting..."
    end
  end

  test "--listen-on-stdin: does not exit on compilation failure" do
    in_fixture "test_stale", fn ->
      File.write!("lib/b.ex", """
      defmodule B do
        def f, do: error_not_a_var
      end
      """)

      port = mix_port(~w[test --listen-on-stdin])

      assert receive_until_match(port, "error", []) =~ "lib/b.ex"

      File.write!("lib/b.ex", """
      defmodule B do
        def f, do: A.f
      end
      """)

      Port.command(port, "\n")

      assert receive_until_match(port, "seed", []) =~ "2 tests"

      File.write!("test/b_test_stale.exs", """
      defmodule BTest do
        use ExUnit.Case

        test "f" do
          assert B.f() == error_not_a_var
        end
      end
      """)

      Port.command(port, "\n")

      assert receive_until_match(port, "undefined function error_not_a_var", []) =~ "test/b_test_stale.exs"

      File.write!("test/b_test_stale.exs", """
      defmodule BTest do
        use ExUnit.Case

        test "f" do
          assert B.f() == :ok
        end
      end
      """)

      Port.command(port, "\n")

      assert receive_until_match(port, "seed", []) =~ "2 tests"
    end
  end

  defp receive_until_match(port, expected, acc) do
    receive do
      {^port, {:data, output}} ->
        acc = [acc | output]

        if output =~ expected do
          IO.iodata_to_binary(acc)
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
