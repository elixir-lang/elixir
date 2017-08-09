Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.CompileTest do
  use MixTest.Case

  defmodule CustomCompilers do
    def project do
      [compilers: [:elixir, :app, :custom]]
    end
  end

  setup do
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  test "compile --list with mixfile" do
    Mix.Tasks.Compile.run ["--list"]
    assert_received {:mix_shell, :info, ["\nEnabled compilers: yecc, leex, erlang, elixir, xref, app, protocols"]}
    assert_received {:mix_shell, :info, ["mix compile.elixir    # " <> _]}
  end

  test "compile --list with custom mixfile" do
    Mix.Project.push CustomCompilers
    Mix.Tasks.Compile.run ["--list"]
    assert_received {:mix_shell, :info, ["\nEnabled compilers: elixir, app, custom, protocols"]}
  end

  test "compile does not require all compilers available on manifest" do
    Mix.Project.push CustomCompilers
    assert Mix.Tasks.Compile.manifests |> Enum.map(&Path.basename/1) ==
           [".compile.elixir"]
  end

  test "compile a project with mixfile" do
    in_fixture "no_mixfile", fn ->
      assert Mix.Tasks.Compile.run(["--verbose"]) == {:ok, []}
      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      assert File.regular?("_build/dev/lib/sample/ebin/sample.app")
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Generated sample app"]}
      assert File.regular? "_build/dev/lib/sample/consolidated/Elixir.Enumerable.beam"

      # Noop
      Mix.Task.clear
      assert Mix.Tasks.Compile.run(["--verbose"]) == {:noop, []}
      refute_received {:mix_shell, :info, ["Compiled lib/a.ex"]}

      # Noop consolidates protocols if folder is missing
      File.rm_rf("_build/dev/lib/sample/consolidated")
      Mix.Task.clear
      assert Mix.Tasks.Compile.run(["--verbose"]) == {:ok, []}
      refute_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert File.regular? "_build/dev/lib/sample/consolidated/Elixir.Enumerable.beam"

      # Purge so consolidated is picked up
      purge [Enumerable]
      assert Mix.Tasks.App.Start.run(["--verbose"]) == :ok
      assert Protocol.consolidated?(Enumerable)
    end
  end

  test "compile a project with multiple compilers and a syntax error in an Erlang file" do
    in_fixture "no_mixfile", fn ->
      import ExUnit.CaptureIO

      File.mkdir! "src"
      File.write! "src/a.erl", """
      -module(b).
      def b(), do: b
      """
      assert File.regular?("src/a.erl")

      assert_raise Mix.Error, fn ->
        capture_io fn -> Mix.Tasks.Compile.run ["--force"] end
      end

      refute File.regular?("ebin/Elixir.A.beam")
      refute File.regular?("ebin/Elixir.B.beam")
    end
  end

  test "add Logger application metadata" do
    import ExUnit.CaptureLog
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
      require Logger
      def info, do: Logger.info("hello")
      end
      """)

      assert Mix.Tasks.Compile.run([]) == {:ok, []}
      try do
        assert capture_log([metadata: [:application]], &A.info/0) =~ "application=sample"
      after
        purge [A]
      end
    end
  end

  test "returns warning diagnostics" do
    alias Mix.Task.Compiler.Diagnostic
    import ExUnit.CaptureIO
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        def my_fn(unused), do: :ok
      end
      """)

      capture_io(:standard_error, fn ->
        assert Mix.Tasks.Compile.run([]) == {:ok, [%Diagnostic{
          file: Path.absname("lib/a.ex"),
          severity: :warning,
          position: 2,
          compiler_name: "Elixir",
          message: "variable \"unused\" is unused"
        }]}
      end)
    end
  end

  test "exits on compile errors without --return-errors" do
    import ExUnit.CaptureIO
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        $$$
      end
      """)

      capture_io(fn ->
        assert catch_exit(Mix.Tasks.Compile.run([])) == {:shutdown, 1}
      end)
    end
  end

  test "returns diagnostics on compile errors with --return-errors" do
    alias Mix.Task.Compiler.Diagnostic
    import ExUnit.CaptureIO
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        $$$
      end
      """)

      capture_io(fn ->
        file = Path.absname("lib/a.ex")
        assert {:error, [%Diagnostic{
          file: ^file,
          severity: :error,
          position: 2,
          message: "** (SyntaxError) lib/a.ex:2:" <> _,
          compiler_name: "Elixir"
        }]} = Mix.Tasks.Compile.run(["--return-errors"])
      end)
    end
  end

  test "returns diagnostics from errors in deps with --return-errors" do
    alias Mix.Task.Compiler.Diagnostic
    import ExUnit.CaptureIO

    defmodule DepsApp do
      def project do
        [app: :sample, version: "0.1.0",
        deps: [
          {:ok, "0.1.0", path: "deps/ok"}
        ]]
      end
    end

    in_fixture "deps_status", fn ->
      Mix.Project.push DepsApp

      File.mkdir_p("deps/ok/lib/")
      File.write!("deps/ok/lib/dep_a.ex", """
      defmodule DepA do
        $$$
      end
      """)

      capture_io(fn ->
        file = Path.absname("deps/ok/lib/dep_a.ex")
        assert {:error, [%Diagnostic{
          compiler_name: "Elixir",
          file: ^file,
          message: "** (SyntaxError) lib/dep_a.ex:2:" <> _,
          position: 2,
          severity: :error
        }]} = Mix.Tasks.Compile.run(["--return-errors"])
      end)
    end
  end
end
