Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.XrefTest do
  use MixTest.Case

  setup do
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  test "doesnt xref if not stale unless forced" do
    in_fixture "no_mixfile", fn ->
      assert Mix.Tasks.Compile.Xref.run([]) == :noop
      assert Mix.Tasks.Compile.Elixir.run([]) == :ok
    end
  end

  test "xrefs if stale" do
    in_fixture "no_mixfile", fn ->
      assert Mix.Tasks.Compile.Elixir.run([]) == :ok
      assert Mix.Tasks.Compile.Xref.run([]) == :noop

      purge [A, B, C]
      [manifest] = Mix.Tasks.Compile.Elixir.manifests()
      future = {{2020, 1, 1}, {0, 0, 0}}
      File.touch!(manifest, future)

      assert Mix.Tasks.Compile.Xref.run([]) == :noop
    end
  end

  test "warns on referencing missing modules" do
    in_fixture "missing_modules_and_functions", fn ->
      task = fn ->
        assert Mix.Tasks.Compile.Elixir.run([]) == :ok
        assert Mix.Tasks.Compile.Xref.run([]) == :noop
      end

      result = ExUnit.CaptureIO.capture_io(:stderr, task)

      assert result == """
      \e[33mwarning: \e[0mRemote function BadReferencer.no_func/0 cannot be found
        lib/missing_modules_and_functions.ex:17

      \e[33mwarning: \e[0mModule MissingModule2 cannot be found

      In remote call to MissingModule2.call/0 at:
        lib/missing_modules_and_functions.ex:24

      \e[33mwarning: \e[0mRemote function BadReferencer.reference/1 cannot be found
        lib/missing_modules_and_functions.ex:26

      \e[33mwarning: \e[0mRemote function BadReferencer.no_func2/0 cannot be found
        lib/missing_modules_and_functions.ex:27

      \e[33mwarning: \e[0mRemote function BadReferencer.no_func3/0 cannot be found
        lib/missing_modules_and_functions.ex:34

      \e[33mwarning: \e[0mRemote function List.old_flatten/1 cannot be found
        lib/missing_modules_and_functions.ex:52

      \e[33mwarning: \e[0mModule :missing_module cannot be found

      In remote call to :missing_module.no_func/0 at:
        lib/missing_modules_and_functions.ex:59

      \e[33mwarning: \e[0mRemote function :lists.not_a_real_func/0 cannot be found
        lib/missing_modules_and_functions.ex:60

      \e[33mwarning: \e[0mRemote function :lists.all/3 cannot be found
        lib/missing_modules_and_functions.ex:62

      """
    end
  end

  test "exits if warnings-as-errors" do
    in_fixture "missing_modules_and_functions", fn ->
      task = fn ->
        assert Mix.Tasks.Compile.Elixir.run([]) == :ok
        assert catch_exit(Mix.Tasks.Compile.Xref.run(["--warnings-as-errors"])) == {:shutdown, 1}
      end

      assert ExUnit.CaptureIO.capture_io(:stderr, task) =~ "warning"
    end
  end

  test "does not exit if warnings-as-errors and no warnings" do
    in_fixture "no_mixfile", fn ->
      task = fn ->
        assert Mix.Tasks.Compile.Elixir.run([]) == :ok
        assert Mix.Tasks.Compile.Xref.run(["--warnings-as-errors"]) == :noop
      end

      assert ExUnit.CaptureIO.capture_io(:stderr, task) =~ ""
    end
  end
end
