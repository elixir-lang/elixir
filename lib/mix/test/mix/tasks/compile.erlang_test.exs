Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.ErlangTest do
  use MixTest.Case
  import ExUnit.CaptureIO

  setup config do
    erlc_options = Map.get(config, :erlc_options, [])
    Mix.ProjectStack.post_config erlc_options: erlc_options
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  @tag erlc_options: [{:d, 'foo', 'bar'}]
  test "raises on invalid erlc_options" do
    in_fixture "compile_erlang", fn ->
      assert_raise Mix.Error, ~r"failed with ArgumentError", fn ->
        capture_io fn ->
          Mix.Tasks.Compile.Erlang.run []
        end
      end
    end
  end

  test "compilation continues if one file fails to compile" do
    in_fixture "compile_erlang", fn ->
      file = Path.absname("src/zzz.erl")
      File.write! file, """
      -module(zzz).
      def zzz(), do: b
      """

      capture_io fn ->
        assert {:error, [%Mix.Task.Compiler.Diagnostic{
          compiler_name: "erl_parse",
          file: ^file,
          message: "syntax error before: zzz",
          position: 2,
          severity: :error
        }]} = Mix.Tasks.Compile.Erlang.run([])
      end

      assert File.regular?("_build/dev/lib/sample/ebin/b.beam")
      assert File.regular?("_build/dev/lib/sample/ebin/c.beam")
    end
  end

  test "compiles src/b.erl and src/c.erl" do
    in_fixture "compile_erlang", fn ->
      assert Mix.Tasks.Compile.Erlang.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled src/b.erl"]}
      assert_received {:mix_shell, :info, ["Compiled src/c.erl"]}

      assert File.regular?("_build/dev/lib/sample/ebin/b.beam")
      assert File.regular?("_build/dev/lib/sample/ebin/c.beam")

      assert Mix.Tasks.Compile.Erlang.run(["--verbose"]) == {:noop, []}
      refute_received {:mix_shell, :info, ["Compiled src/b.erl"]}

      assert Mix.Tasks.Compile.Erlang.run(["--force", "--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled src/b.erl"]}
      assert_received {:mix_shell, :info, ["Compiled src/c.erl"]}
    end
  end

  test "removes old artifact files" do
    in_fixture "compile_erlang", fn ->
      assert Mix.Tasks.Compile.Erlang.run([]) == {:ok, []}
      assert File.regular?("_build/dev/lib/sample/ebin/b.beam")

      File.rm!("src/b.erl")
      assert Mix.Tasks.Compile.Erlang.run([]) == {:ok, []}
      refute File.regular?("_build/dev/lib/sample/ebin/b.beam")
    end
  end

  test "compilation purges the module" do
    in_fixture "compile_erlang", fn ->
      # Create the first version of the module.
      defmodule :purge_test do
        def version, do: :v1
      end
      assert :v1 == :purge_test.version

      # Create the second version of the module (this time as Erlang source).
      File.write! "src/purge_test.erl", """
      -module(purge_test).
      -export([version/0]).
      version() -> v2.
      """
      assert Mix.Tasks.Compile.Erlang.run([]) == {:ok, []}

      # If the module was not purged on recompilation, this would fail.
      assert :v2 == :purge_test.version
    end
  end

  test "saves warnings between builds" do
    in_fixture "compile_erlang", fn ->
      file = Path.absname("src/has_warning.erl")
      File.write! file, """
      -module(has_warning).
      my_fn() -> ok.
      """

      capture_io fn ->
        assert {:ok, [%Mix.Task.Compiler.Diagnostic{
          file: ^file,
          compiler_name: "erl_lint",
          message: "function my_fn/0 is unused",
          position: 2,
          severity: :warning
        } = warning]} = Mix.Tasks.Compile.Erlang.run([])

        # Should return warning without recompiling file
        assert {:noop, [^warning]} = Mix.Tasks.Compile.Erlang.run(["--verbose"])
        refute_received {:mix_shell, :info, ["Compiled src/has_warning.erl"]}

        # Should not return warning after changing file
        File.write! file, """
        -module(has_warning).
        -export([my_fn/0]).
        my_fn() -> ok.
        """
        ensure_touched(file)
        assert {:ok, []} = Mix.Tasks.Compile.Erlang.run([])
      end
    end
  end
end
