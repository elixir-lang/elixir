Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.TestTest do
  use MixTest.Case

  import Mix.Tasks.Test, only: [ex_unit_opts: 1]

  test "ex_unit_opts returns ex unit options" do
    assert ex_unit_opts([unknown: "ok", seed: 13]) ==
           [autorun: false, seed: 13]
  end

  test "ex_unit_opts returns includes and excludes" do
    assert ex_unit_opts([include: "focus", include: "key:val"]) ==
           [autorun: false, include: [:focus, key: "val"]]

    assert ex_unit_opts([exclude: "focus", exclude: "key:val"]) ==
           [autorun: false, exclude: [:focus, key: "val"]]
  end

  test "ex_unit_opts translates only into includes and excludes" do
    assert ex_unit_opts([only: "focus"]) ==
           [autorun: false, exclude: [:test], include: [:focus]]

    assert ex_unit_opts([only: "focus", include: "special"]) ==
           [autorun: false, exclude: [:test], include: [:focus, :special]]
  end

  test "ex_unit_opts translates :color into list containing an enabled key/value pair" do
    assert ex_unit_opts([color: false]) == [autorun: false, colors: [enabled: false]]
    assert ex_unit_opts([color: true]) == [autorun: false, colors: [enabled: true]]
  end

  test "--stale: runs all tests for first run, then none on second" do
    in_fixture "test_stale", fn ->
      assert_stale_run_output "2 tests, 0 failures"

      assert_stale_run_output """
      No stale tests.
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

  defp set_all_mtimes(time \\ {{2010, 1, 1}, {0, 0, 0}}) do
    Enum.each(Path.wildcard("**", match_dot: true), &File.touch!(&1, time))
  end

  defp assert_stale_run_output(opts \\ [], expected) do
    assert mix(~w[test --stale] ++ opts) =~ expected
  end
end
