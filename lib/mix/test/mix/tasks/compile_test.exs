Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.CompileTest do
  use MixTest.Case

  defmodule CustomCompilers do
    def project do
      [compilers: [:elixir, :app, :custom]]
    end
  end

  setup do
    Mix.Project.push(MixTest.Case.Sample)
    :ok
  end

  test "compile --list with mixfile" do
    Mix.Task.run("compile", ["--list"])

    msg = "\nEnabled compilers: yecc, leex, erlang, elixir, xref, app, protocols"
    assert_received {:mix_shell, :info, [^msg]}

    assert_received {:mix_shell, :info, ["mix compile.elixir    # " <> _]}
  end

  test "compile --list with custom mixfile" do
    Mix.Project.push(CustomCompilers)
    Mix.Task.run("compile", ["--list"])
    assert_received {:mix_shell, :info, ["\nEnabled compilers: elixir, app, custom, protocols"]}
  end

  test "compile does not require all compilers available on manifest" do
    Mix.Project.push(CustomCompilers)
    assert Mix.Tasks.Compile.manifests() |> Enum.map(&Path.basename/1) == ["compile.elixir"]
  end

  test "compile a project with mixfile" do
    in_fixture "no_mixfile", fn ->
      assert Mix.Task.run("compile", ["--verbose"]) == {:ok, []}
      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      assert File.regular?("_build/dev/lib/sample/ebin/sample.app")
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Generated sample app"]}
      assert File.regular?("_build/dev/lib/sample/consolidated/Elixir.Enumerable.beam")

      # Noop
      Mix.Task.clear()
      assert Mix.Task.run("compile", ["--verbose"]) == {:noop, []}
      refute_received {:mix_shell, :info, ["Compiled lib/a.ex"]}

      # Consolidates protocols if manifest is out of date
      File.rm("_build/dev/lib/sample/.mix/compile.protocols")
      Mix.Task.clear()
      assert Mix.Task.run("compile", ["--verbose"]) == {:ok, []}
      refute_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert File.regular?("_build/dev/lib/sample/consolidated/Elixir.Enumerable.beam")

      # Purge so consolidated is picked up
      purge([Enumerable])
      assert Mix.Tasks.App.Start.run(["--verbose"]) == :ok
      assert Protocol.consolidated?(Enumerable)
    end
  end

  test "compile a project with multiple compilers and a syntax error in an Erlang file" do
    in_fixture "no_mixfile", fn ->
      import ExUnit.CaptureIO

      file = Path.absname("src/a.erl")
      File.mkdir!("src")

      File.write!(file, """
      -module(b).
      def b(), do: b
      """)

      assert File.regular?(file)

      capture_io(fn ->
        assert {:error, [diagnostic]} = Mix.Task.run("compile", ["--force", "--return-errors"])

        assert %Mix.Task.Compiler.Diagnostic{
                 compiler_name: "erl_parse",
                 file: ^file,
                 message: "syntax error before: b",
                 position: 2,
                 severity: :error
               } = diagnostic
      end)

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

      assert Mix.Task.run("compile", []) == {:ok, []}

      try do
        assert capture_log([metadata: [:application]], &A.info/0) =~ "application=sample"
      after
        purge([A])
      end
    end
  end

  test "returns errors from compilers when --return-errors is set" do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        def my_fn(), do: $$$
      end
      """)

      file = Path.absname("lib/a.ex")

      ExUnit.CaptureIO.capture_io(fn ->
        assert {:error, [diagnostic]} = Mix.Task.run("compile", ["--return-errors"])

        assert %Mix.Task.Compiler.Diagnostic{
                 file: ^file,
                 severity: :error,
                 position: 2,
                 message: "** (SyntaxError) lib/a.ex:2:" <> _,
                 compiler_name: "Elixir"
               } = diagnostic
      end)
    end
  end
end
