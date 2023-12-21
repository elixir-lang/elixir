Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Compile.LeexTest do
  use MixTest.Case
  import ExUnit.CaptureIO

  setup do
    Mix.ProjectStack.post_config(compilers: [:leex | Mix.compilers()])
    Mix.Project.push(MixTest.Case.Sample)
    :ok
  end

  test "compilation continues if one file fails to compile" do
    in_fixture("compile_leex", fn ->
      file = Path.absname("src/zzz.xrl")

      File.write!(file, """
      oops.
      """)

      capture_io(fn ->
        assert {:error, [diagnostic]} = Mix.Tasks.Compile.Leex.run(["--force"])

        assert %Mix.Task.Compiler.Diagnostic{
                 compiler_name: "leex",
                 file: ^file,
                 source: ^file,
                 message: "missing Definitions",
                 position: 1,
                 severity: :error
               } = diagnostic
      end)

      assert File.regular?("src/test_ok.erl")
    end)
  end

  test "compiles src/test_ok.xrl" do
    in_fixture("compile_leex", fn ->
      assert Mix.Tasks.Compile.Leex.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled src/test_ok.xrl"]}
      assert File.regular?("src/test_ok.erl")

      assert Mix.Tasks.Compile.Leex.run(["--verbose"]) == {:noop, []}
      refute_received {:mix_shell, :info, ["Compiled src/test_ok.xrl"]}

      assert Mix.Tasks.Compile.Leex.run(["--force", "--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled src/test_ok.xrl"]}
    end)
  end

  test "removes old artifact files" do
    in_fixture("compile_leex", fn ->
      assert Mix.Tasks.Compile.Leex.run([]) == {:ok, []}
      assert File.regular?("src/test_ok.erl")

      File.rm!("src/test_ok.xrl")
      assert Mix.Tasks.Compile.Leex.run([]) == {:ok, []}
      refute File.regular?("src/test_ok.erl")
    end)
  end
end
