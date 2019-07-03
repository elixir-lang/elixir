Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.CompileTest do
  use MixTest.Case

  defmodule CustomCompilers do
    def project do
      [compilers: [:elixir, :app, :custom]]
    end
  end

  defmodule DepsApp do
    def project do
      [app: :deps_app, version: "0.1.0", deps: [{:ok, "0.1.0", path: "deps/ok"}]]
    end
  end

  defmodule WrongPath do
    def project do
      [app: :apps_path_bug, apps_path: "this_path_does_not_exist"]
    end
  end

  setup do
    Mix.Project.push(MixTest.Case.Sample)
    :ok
  end

  test "compiles --list with mixfile" do
    Mix.Task.run("compile", ["--list"])

    msg = "\nEnabled compilers: yecc, leex, erlang, elixir, app, protocols"
    assert_received {:mix_shell, :info, [^msg]}

    assert_received {:mix_shell, :info, ["mix compile.elixir    # " <> _]}
  end

  test "compiles --list with custom mixfile" do
    Mix.Project.pop()
    Mix.Project.push(CustomCompilers)
    Mix.Task.run("compile", ["--list"])
    assert_received {:mix_shell, :info, ["\nEnabled compilers: elixir, app, custom, protocols"]}
  end

  test "compiles does not require all compilers available on manifest" do
    Mix.Project.pop()
    Mix.Project.push(CustomCompilers)
    assert Mix.Tasks.Compile.manifests() |> Enum.map(&Path.basename/1) == ["compile.elixir"]
  end

  test "compiles a project with cached deps information" do
    Mix.Project.pop()
    Mix.Project.push(DepsApp)

    in_fixture("deps_status", fn ->
      File.mkdir_p!("lib")

      File.write!("lib/a.ex", """
      root = File.cwd!
      File.cd!("lib", fn ->
        %{ok: path} = Mix.Project.deps_paths()

        if Path.relative_to(path, root) != "deps/ok" do
          raise "non cached path"
        end
      end)
      """)

      assert Mix.Task.run("compile", ["--force", "--no-deps"]) == {:ok, []}
    end)
  end

  test "compiles a project with mixfile" do
    in_fixture("no_mixfile", fn ->
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
    end)
  end

  test "compiles a project with multiple compilers and a syntax error in an Erlang file" do
    in_fixture("no_mixfile", fn ->
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
    end)
  end

  test "adds Logger application metadata" do
    import ExUnit.CaptureLog

    in_fixture("no_mixfile", fn ->
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
    end)
  end

  test "returns errors from compilers when --return-errors is set" do
    in_fixture("no_mixfile", fn ->
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
    end)
  end

  test "skip protocol consolidation when --no-protocol-consolidation" do
    in_fixture("no_mixfile", fn ->
      File.rm("_build/dev/lib/sample/.mix/compile.protocols")
      assert Mix.Task.run("compile", ["--no-protocol-consolidation"]) == {:ok, []}
      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      refute File.regular?("_build/dev/lib/sample/consolidated/Elixir.Enumerable.beam")
    end)
  end

  test "loads Mix config with --erl-config" do
    in_fixture("no_mixfile", fn ->
      File.write!("mix.config", "{erl_config_app, [{value, true}]}.")
      assert Mix.Task.run("compile", ["--erl-config", "mix.config"]) == {:ok, []}
      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      assert Application.get_env(:erl_config_app, :value)
    end)
  after
    Application.delete_env(:erl_config_app, :value)
  end

  test "compiles a project with wrong path" do
    Mix.Project.pop()
    Mix.Project.push(WrongPath)

    ExUnit.CaptureIO.capture_io(fn ->
      assert Mix.Task.run("compile", ["--no-protocol-consolidation"]) == {:noop, []}
    end)
  end
end
