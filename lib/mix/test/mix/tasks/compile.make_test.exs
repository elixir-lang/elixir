Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.MakeTest do
  use MixTest.Case
  import ExUnit.CaptureIO

  setup do
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  test "running with a specific executable" do
    in_fixture "compile_make", fn ->
      with_project_config [make_executable: "nonexistentmake"], fn ->
        assert_raise Mix.Error, "`nonexistentmake` not found in the current path", fn ->
          run()
        end
      end
    end
  end

  test "running without a makefile" do
    msg = ~r/\ACould not compile with/

    in_fixture "compile_make", fn ->
      File.rm_rf!("Makefile")

      capture_io fn ->
        assert_raise Mix.Error, msg, fn -> run() end
      end
    end
  end

  test "running with a makefile" do
    in_fixture "compile_make", fn ->
      File.write! "Makefile", """
      target:
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
      target:
      \t@echo "target"
      other_target:
      \t@echo "other target"
      """

      with_project_config [make_targets: ~w(target other_target)], fn ->
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
        assert capture_io(fn -> run() end) == "subdir\n"
      end
    end
  end

  test "specifying a makefile" do
    in_fixture "compile_make", fn ->
      File.write "MyMakefile", """
      all:
      \t@echo "my makefile"
      """

      with_project_config [make_makefile: "MyMakefile"], fn ->
        assert capture_io(fn -> run() end) == "my makefile\n"
      end
    end
  end

  test "specifying a custom error message" do
    in_fixture "compile_make", fn ->
      with_project_config [make_error_message: "try harder"], fn ->
        capture_io fn ->
          assert_raise Mix.Error, ~r/try harder/, fn -> run() end
        end
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
