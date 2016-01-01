Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.MakeTest do
  use MixTest.Case
  import ExUnit.CaptureIO

  setup do
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  test "running without a makefile" do
    msg             = ~r/`make` exited with a non-zero status \(\d+\)/
    expected_output = "make: *** No targets specified and no makefile found.  Stop.\n"

    in_fixture "compile_make", fn ->
      File.rm_rf!("Makefile")

      output = capture_io fn ->
        assert_raise Mix.Error, msg, fn -> run() end
      end

      assert expected_output == output
    end
  end

  test "running with a makefile" do
    in_fixture "compile_make", fn ->
      File.write! "Makefile", """
      my_target:
      \t@echo "hello"
      """

      assert capture_io(fn -> run() end) =~ "hello\n"
    end
  end

  test "specifying targets" do
    in_fixture "compile_make", fn ->
      File.write! "Makefile", """
      useless_target:
      \t@echo "nope"
      my_target:
      \t@echo "target"
      my_other_target:
      \t@echo "other target"
      """

      with_project_config [make_targets: ~w(my_target my_other_target)], fn ->
        output = capture_io(fn -> run() end)
        assert output =~ "target\n"
        assert output =~ "other target\n"
        refute output =~ "nope"
      end
    end
  end

  test "specifying a cwd" do
    in_fixture "compile_make", fn ->
      File.mkdir_p!("subdir")
      File.write! "subdir/Makefile", """
      all:
      \t@echo "subdir"
      """

      with_project_config [make_cwd: "subdir"], fn ->
        assert capture_io(&run/0) == "subdir\n"
      end
    end
  end

  defp with_project_config(config, fun) do
    Mix.Project.in_project(:sample, ".", config, fn(_) -> fun.() end)
  end

  defp run(args \\ []) do
    Mix.Tasks.Compile.Make.run(args)
  end
end
