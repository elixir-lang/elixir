Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.XrefTest do
  use MixTest.Case

  import ExUnit.CaptureIO

  setup do
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  test "doesn't do anything if no Elixir manifest" do
    in_fixture "no_mixfile", fn ->
      write_no_func()

      [xref_manifest] = Mix.Tasks.Compile.Xref.manifests()

      assert_no_warn fn ->
        assert Mix.Tasks.Compile.Xref.run([]) == :noop
        refute File.exists? xref_manifest
      end
    end
  end

  test "doesn't xref if not stale, unless forced" do
    in_fixture "no_mixfile", fn ->
      write_no_func()

      assert_warn_no_func fn ->
        assert Mix.Tasks.Compile.Elixir.run([]) == {:ok, []}
        assert Mix.Tasks.Compile.Xref.run([]) == :noop
      end

      assert_no_warn fn ->
        assert Mix.Tasks.Compile.Xref.run([]) == :noop
      end

      Mix.Task.reenable("xref")

      assert_warn_no_func fn ->
        assert Mix.Tasks.Compile.Xref.run(["--force"]) == :noop
      end
    end
  end

  test "xrefs if stale" do
    in_fixture "no_mixfile", fn ->
      write_no_func()

      assert_warn_no_func fn ->
        assert Mix.Tasks.Compile.Elixir.run([]) == {:ok, []}
        assert Mix.Tasks.Compile.Xref.run([]) == :noop
      end

      [manifest] = Mix.Tasks.Compile.Elixir.manifests()
      future = {{2020, 1, 1}, {0, 0, 0}}
      File.touch!(manifest, future)

      Mix.Task.reenable("xref")

      assert_warn_no_func fn ->
        assert Mix.Tasks.Compile.Xref.run([]) == :noop
      end
    end
  end

  test "exits if warnings-as-errors" do
    in_fixture "no_mixfile", fn ->
      write_no_func()

      assert_warn_no_func fn ->
        assert Mix.Tasks.Compile.Elixir.run([]) == {:ok, []}
        assert catch_exit(Mix.Tasks.Compile.Xref.run(["--warnings-as-errors"])) == {:shutdown, 1}
      end
    end
  end

  test "does not exit if warnings-as-errors and no warnings" do
    in_fixture "no_mixfile", fn ->
      assert_no_warn fn ->
        assert Mix.Tasks.Compile.Elixir.run([]) == {:ok, []}
        assert Mix.Tasks.Compile.Xref.run(["--warnings-as-errors"]) == :noop
      end
    end
  end

  defp write_no_func do
    File.write!("lib/a.ex", """
    defmodule A do
      def a, do: B.no_func
    end
    """)
  end

  defp assert_warn_no_func(fun) do
    assert capture_io(:stderr, fun) =~ "no_func"
  end

  defp assert_no_warn(fun) do
    assert capture_io(:stderr, fun) == ""
  end
end
