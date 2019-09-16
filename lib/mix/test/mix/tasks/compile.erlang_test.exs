Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Compile.ErlangTest do
  use MixTest.Case
  import ExUnit.CaptureIO

  setup config do
    erlc_options = Map.get(config, :erlc_options, [])
    Mix.ProjectStack.post_config(erlc_options: erlc_options)
    Mix.Project.push(MixTest.Case.Sample)
    :ok
  end

  @tag erlc_options: [{:d, 'foo', 'bar'}]
  test "raises on invalid erlc_options" do
    in_fixture("compile_erlang", fn ->
      assert_raise Mix.Error, ~r"failed with ArgumentError", fn ->
        capture_io(fn ->
          Mix.Tasks.Compile.Erlang.run([])
        end)
      end
    end)
  end

  test "compiles and cleans src/b.erl and src/c.erl" do
    in_fixture("compile_erlang", fn ->
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

      assert Mix.Tasks.Compile.Erlang.clean()
      refute File.regular?("_build/dev/lib/sample/ebin/b.beam")
      refute File.regular?("_build/dev/lib/sample/ebin/c.beam")
    end)
  end

  test "removes old artifact files" do
    in_fixture("compile_erlang", fn ->
      assert Mix.Tasks.Compile.Erlang.run([]) == {:ok, []}
      assert File.regular?("_build/dev/lib/sample/ebin/b.beam")

      File.rm!("src/b.erl")
      assert Mix.Tasks.Compile.Erlang.run([]) == {:ok, []}
      refute File.regular?("_build/dev/lib/sample/ebin/b.beam")
    end)
  end

  test "compilation purges the module" do
    in_fixture("compile_erlang", fn ->
      # Create the first version of the module.
      defmodule :purge_test do
        def version, do: :v1
      end

      assert :v1 == :purge_test.version()

      # Create the second version of the module (this time as Erlang source).
      File.write!("src/purge_test.erl", """
      -module(purge_test).
      -export([version/0]).
      version() -> v2.
      """)

      assert Mix.Tasks.Compile.Erlang.run([]) == {:ok, []}

      # If the module was not purged on recompilation, this would fail.
      assert :v2 == :purge_test.version()
    end)
  end

  test "continues even if one file fails to compile" do
    in_fixture("compile_erlang", fn ->
      file = Path.absname("src/zzz.erl")

      File.write!(file, """
      -module(zzz).
      def zzz(), do: b
      """)

      capture_io(fn ->
        assert {:error, [diagnostic]} = Mix.Tasks.Compile.Erlang.run([])

        assert %Mix.Task.Compiler.Diagnostic{
                 compiler_name: "erl_parse",
                 file: ^file,
                 message: "syntax error before: zzz",
                 position: 2,
                 severity: :error
               } = diagnostic
      end)

      assert File.regular?("_build/dev/lib/sample/ebin/b.beam")
      assert File.regular?("_build/dev/lib/sample/ebin/c.beam")
    end)
  end

  test "saves warnings between builds" do
    in_fixture("compile_erlang", fn ->
      file = Path.absname("src/has_warning.erl")

      File.write!(file, """
      -module(has_warning).
      my_fn() -> ok.
      """)

      capture_io(fn ->
        assert {:ok, [diagnostic]} = Mix.Tasks.Compile.Erlang.run([])

        assert %Mix.Task.Compiler.Diagnostic{
                 file: ^file,
                 compiler_name: "erl_lint",
                 message: "function my_fn/0 is unused",
                 position: 2,
                 severity: :warning
               } = diagnostic

        # Should return warning without recompiling file
        assert {:noop, [^diagnostic]} = Mix.Tasks.Compile.Erlang.run(["--verbose"])
        refute_received {:mix_shell, :info, ["Compiled src/has_warning.erl"]}

        # Should not return warning after changing file
        File.write!(file, """
        -module(has_warning).
        -export([my_fn/0]).
        my_fn() -> ok.
        """)

        ensure_touched(file)
        assert {:ok, []} = Mix.Tasks.Compile.Erlang.run([])
      end)
    end)
  end

  test "prints warnings from stale files with --all-warnings" do
    in_fixture("compile_erlang", fn ->
      file = Path.absname("src/has_warning.erl")

      File.write!(file, """
      -module(has_warning).
      my_fn() -> ok.
      """)

      capture_io(fn -> Mix.Tasks.Compile.Erlang.run([]) end)

      output =
        capture_io(fn ->
          assert {:noop, _} = Mix.Tasks.Compile.Erlang.run(["--all-warnings"])
        end)

      assert output == "src/has_warning.erl:2: Warning: function my_fn/0 is unused\n"

      # Should not print old warnings after fixing
      File.write!(file, """
      -module(has_warning).
      """)

      ensure_touched(file)

      output =
        capture_io(fn ->
          Mix.Tasks.Compile.Erlang.run(["--all-warnings"])
        end)

      assert output == ""
    end)
  end

  @tag erlc_options: [{:warnings_as_errors, true}]
  test "adds :debug_info to erlc_options by default" do
    in_fixture("compile_erlang", fn ->
      Mix.Tasks.Compile.Erlang.run([])

      binary = File.read!("_build/dev/lib/sample/ebin/b.beam")

      {:ok, {_, [debug_info: {:debug_info_v1, _, {debug_info, _}}]}} =
        :beam_lib.chunks(binary, [:debug_info])

      assert debug_info != :none
    end)
  end
end
