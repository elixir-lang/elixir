Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Compile.YeccTest do
  use MixTest.Case
  import ExUnit.CaptureIO

  defmacro position(line, column), do: {line, column}

  setup do
    Mix.ProjectStack.post_config(compilers: [:yecc | Mix.compilers()])
    Mix.Project.push(MixTest.Case.Sample)
    :ok
  end

  test "compilation continues if one file fails to compile" do
    in_fixture("compile_yecc", fn ->
      file = Path.absname("src/zzz.yrl")

      File.write!(file, """
      oops.
      """)

      capture_io(fn ->
        assert {:error, [diagnostic]} = Mix.Tasks.Compile.Yecc.run(["--force"])

        assert %Mix.Task.Compiler.Diagnostic{
                 compiler_name: "yecc",
                 file: ^file,
                 source: ^file,
                 message: message,
                 position: position(1, 5),
                 severity: :error
               } = diagnostic

        assert message =~ "syntax error before: "
      end)

      assert File.regular?("src/test_ok.erl")
    end)
  end

  test "returns warning diagnostics on conflicts" do
    in_fixture("compile_yecc", fn ->
      file = Path.absname("src/conflict.yrl")

      File.write!(file, """
      Nonterminals exp.
      Terminals number '+'.
      Rootsymbol exp.
      exp -> exp '+' exp.
      exp -> number.
      """)

      capture_io(fn ->
        assert {:ok, [diagnostic]} = Mix.Tasks.Compile.Yecc.run(["--force"])

        assert %Mix.Task.Compiler.Diagnostic{
                 compiler_name: "yecc",
                 file: ^file,
                 source: ^file,
                 message: "conflicts: 1 shift/reduce, 0 reduce/reduce",
                 position: 0,
                 severity: :warning
               } = diagnostic
      end)

      # Removing parse tools re-add it later even if only to show warnings
      :code.del_path(:parsetools)
      purge([:yecc])
      refute Code.ensure_loaded?(:yecc)

      capture_io(fn ->
        assert {:noop, [diagnostic]} = Mix.Tasks.Compile.Yecc.run([])

        assert %Mix.Task.Compiler.Diagnostic{
                 compiler_name: "yecc",
                 file: ^file,
                 source: ^file,
                 message: "conflicts: 1 shift/reduce, 0 reduce/reduce",
                 position: 0,
                 severity: :warning
               } = diagnostic
      end)
    end)
  end

  test "compiles src/test_ok.yrl" do
    in_fixture("compile_yecc", fn ->
      assert Mix.Tasks.Compile.Yecc.run(["--verbose"]) == {:ok, []}

      assert_received {:mix_shell, :info, ["Compiled src/test_ok.yrl"]}
      assert File.regular?("src/test_ok.erl")

      assert Mix.Tasks.Compile.Yecc.run(["--verbose"]) == {:noop, []}
      refute_received {:mix_shell, :info, ["Compiled src/test_ok.yrl"]}

      assert Mix.Tasks.Compile.Yecc.run(["--force", "--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled src/test_ok.yrl"]}
    end)
  end

  test "removes old artifact files" do
    in_fixture("compile_yecc", fn ->
      assert Mix.Tasks.Compile.Yecc.run([]) == {:ok, []}
      assert File.regular?("src/test_ok.erl")

      File.rm!("src/test_ok.yrl")
      assert Mix.Tasks.Compile.Yecc.run([]) == {:ok, []}
      refute File.regular?("src/test_ok.erl")
    end)
  end
end
