Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.CompileTest do
  use MixTest.Case

  defmacro position(line, column), do: {line, column}

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

  setup tags do
    Mix.ProjectStack.post_config(Map.get(tags, :project, []))
    Mix.Project.push(MixTest.Case.Sample)
    :ok
  end

  test "compiles --list with mixfile" do
    Mix.Task.run("compile", ["--list"])

    msg = "\nEnabled compilers: yecc, leex, erlang, elixir, app, protocols"
    assert_received {:mix_shell, :info, [^msg]}

    assert_received {:mix_shell, :info, ["mix compile.elixir    # " <> _]}
  end

  @tag project: [compilers: [:elixir, :app, :custom]]
  test "compiles --list with custom mixfile" do
    Mix.Task.run("compile", ["--list"])

    assert_received {:mix_shell, :info,
                     ["\nEnabled compilers: yecc, leex, elixir, app, custom, protocols"]}
  end

  @tag project: [compilers: [:elixir, :app, :custom]]
  test "compiles does not require all compilers available on manifest" do
    assert Mix.Task.Compiler.manifests() |> Enum.map(&Path.basename/1) ==
             ["compile.yecc", "compile.leex", "compile.elixir"]
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

  test "compiles a project with cached deps information" do
    Mix.Project.pop()

    in_fixture("deps_status", fn ->
      Mix.Project.push(DepsApp)

      File.mkdir_p!("lib")

      File.write!("lib/a.ex", """
      root = File.cwd!()
      File.cd!("lib", fn ->
        %{ok: path} = Mix.Project.deps_paths()

        if Path.relative_to(path, root) != "deps/ok" do
          raise "non cached path"
        end
      end)
      """)

      assert Mix.Task.run("compile", ["--force", "--from-mix-deps-compile"]) == {:ok, []}
    end)
  end

  @tag project: [compilers: Mix.compilers() ++ [:my_custom_compiler]]
  test "compiles a project with custom in-project compiler" do
    in_fixture("no_mixfile", fn ->
      File.mkdir_p!("lib")

      File.write!("lib/a.ex", """
      defmodule Mix.Tasks.Compile.MyCustomCompiler do
        use Mix.Task.Compiler

        @impl true
        def run(_args) do
          Mix.shell().info("Compiling...")
          :ok
        end
      end
      """)

      assert Mix.Task.run("compile") == {:ok, []}
      assert_receive {:mix_shell, :info, ["Compiling..."]}
      Code.delete_path(Mix.Project.compile_path())
      purge([Mix.Tasks.Compile.MyCustomCompiler])
      false = Code.ensure_loaded?(Mix.Tasks.Compile.MyCustomCompiler)

      Mix.Task.clear()
      assert Mix.Task.rerun("compile")
    end)
  end

  test "recompiles app cache if manifest changes" do
    in_fixture("no_mixfile", fn ->
      Mix.Tasks.Compile.run(["--force"])
      purge([A, B])

      File.rm!("_build/dev/lib/sample/.mix/compile.app_cache")
      Mix.Task.clear()

      Mix.Tasks.Compile.run(["--force"])
      assert File.exists?("_build/dev/lib/sample/.mix/compile.app_cache")
    end)
  end

  # A.info/0 is loaded dynamically
  @compile {:no_warn_undefined, {A, :info, 0}}

  test "adds Logger application metadata" do
    import ExUnit.CaptureLog

    in_fixture("no_mixfile", fn ->
      Process.put({MixTest.Case.Sample, :application}, extra_applications: [:logger])

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

      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        assert {:error, [diagnostic]} = Mix.Task.run("compile", ["--return-errors"])

        assert %Mix.Task.Compiler.Diagnostic{
                 file: ^file,
                 source: ^file,
                 severity: :error,
                 position: {2, 20},
                 message: "** (SyntaxError) invalid syntax found on lib/a.ex:2:" <> _,
                 compiler_name: "Elixir",
                 details: {:error, %SyntaxError{}}
               } = diagnostic
      end)
    end)
  end

  test "calling raise inside a macro returns a diagnostic with a position" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        defmacro custom_macro do
          raise "error"
        end
      end
      """)

      File.write!("lib/b.ex", """
      defmodule B do
        require A
        A.custom_macro()
      end
      """)

      file = Path.absname("lib/b.ex")

      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        assert {:error, [diagnostic]} = Mix.Task.run("compile", ["--return-errors"])

        assert %Mix.Task.Compiler.Diagnostic{
                 file: ^file,
                 source: ^file,
                 severity: :error,
                 position: 3,
                 message: "** (RuntimeError) error\n    expanding macro: A.custom_macro/0" <> _,
                 compiler_name: "Elixir"
               } = diagnostic
      end)
    end)
  end

  test "returns syntax error from an Erlang file when --return-errors is set" do
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
                 source: ^file,
                 message: "syntax error before: b",
                 position: position(2, 5),
                 severity: :error
               } = diagnostic
      end)

      refute File.regular?("ebin/Elixir.A.beam")
      refute File.regular?("ebin/Elixir.B.beam")
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

  test "loads consolidated protocols even on --no-compile" do
    in_fixture("no_mixfile", fn ->
      File.rm("_build/dev/lib/sample/.mix/compile.protocols")
      consolidated = "_build/dev/lib/sample/consolidated" |> Path.expand() |> to_charlist()

      assert Mix.Task.run("compile") == {:ok, []}
      assert File.regular?("_build/dev/lib/sample/consolidated/Elixir.Enumerable.beam")
      assert consolidated in :code.get_path()

      Code.delete_path("_build/dev/lib/sample/consolidated")
      assert Mix.Task.rerun("compile", ["--no-compile"]) == {:noop, []}
      assert consolidated in :code.get_path()
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

  test "runs after_compiler callback once" do
    in_fixture("no_mixfile", fn ->
      callback = fn result -> send(self(), result) end

      assert Mix.Task.Compiler.after_compiler(:elixir, callback) == :ok
      assert Mix.Task.rerun("compile", []) == {:ok, []}
      assert_received {:ok, []}

      Mix.Task.clear()
      assert Mix.Task.rerun("compile", []) == {:noop, []}
      refute_received {:noop, []}

      Mix.Task.clear()
      assert Mix.Task.Compiler.after_compiler(:elixir, callback) == :ok
      assert Mix.Task.run("compile", []) == {:noop, []}
      assert_received {:noop, []}
    end)
  end

  test "does not crash on a project with bad path" do
    Mix.Project.pop()
    Mix.Project.push(WrongPath)

    ExUnit.CaptureIO.capture_io(fn ->
      assert Mix.Task.run("compile", ["--no-protocol-consolidation"]) == {:noop, []}
    end)
  end

  test "validates compile_env" do
    in_fixture("no_mixfile", fn ->
      File.mkdir_p!("config")

      File.write!("config/config.exs", """
      import Config
      config :sample, :hello, System.get_env("MIX_SAMPLE_HELLO")
      """)

      File.write!("lib/a.ex", """
      defmodule A do
        require Application
        _ = Application.compile_env(:sample, :hello)
      end
      """)

      System.put_env("MIX_SAMPLE_HELLO", "compile")
      Mix.Tasks.Loadconfig.run([])
      assert Mix.Tasks.Compile.All.run([]) == {:ok, []}

      System.put_env("MIX_SAMPLE_HELLO", "runtime")
      Mix.Tasks.Loadconfig.run([])
      Application.unload(:sample)

      assert_raise Mix.Error,
                   ~r/the application :sample has a different value set for key :hello during runtime compared to compile time/,
                   fn -> Mix.Tasks.Compile.All.run([]) end

      # Can start if compile env matches
      System.put_env("MIX_SAMPLE_HELLO", "compile")
      Mix.Tasks.Loadconfig.run([])
      assert Mix.Tasks.App.Start.run([]) == :ok
    end)
  after
    System.delete_env("MIX_SAMPLE_HELLO")
    Application.delete_env(:sample, :hello, persistent: true)
  end

  test "code path pruning" do
    Mix.ensure_application!(:parsetools)
    otp_docs? = match?({:docs_v1, _, _, _, _, _, _}, Code.fetch_docs(:zlib))

    in_fixture("no_mixfile", fn ->
      assert Mix.Task.run("compile", []) == {:ok, []}
      assert :code.where_is_file(~c"parsetools.app") == :non_existing

      # Make sure erts is also kept but not loaded
      assert Application.spec(:erts, :vsn) == nil

      if otp_docs? do
        assert {:docs_v1, _, _, _, _, _, _} = Code.fetch_docs(:zlib)
      else
        IO.warn("Erlang/OTP was not compiled with docs, skipping assertion")
      end
    end)
  end

  test "code path pruning disabled" do
    Mix.ensure_application!(:parsetools)

    in_fixture("no_mixfile", fn ->
      assert Mix.Task.run("compile", ["--no-prune-code-paths"]) == {:ok, []}
      assert is_list(:code.where_is_file(~c"parsetools.app"))
    end)
  end

  test "listening to concurrent compilations" do
    timeout = 2_000

    Mix.Project.pop()

    in_fixture("compile_listeners", fn ->
      File.write!("mix.exs", """
        defmodule WithReloader do
          use Mix.Project

          def project do
            [
              app: :with_reloader,
              version: "0.1.0",
              deps: [{:reloader, "0.1.0", path: "deps/reloader"}],
              # Register a listener from a dependency
              listeners: [Reloader]
            ]
          end
        end
      """)

      File.mkdir_p!("config")

      File.mkdir_p!("lib")

      File.write!("lib/a.ex", "defmodule A do end")
      File.write!("lib/b.ex", "defmodule B do end")
      File.write!("lib/c.ex", "defmodule C do end")

      File.mkdir_p!("src")

      File.write!("src/a.erl", "-module(a).")
      File.write!("src/b.erl", "-module(b).")
      File.write!("src/c.erl", "-module(c).")

      mix(["deps.compile"])

      parent = self()

      spawn_link(fn ->
        port =
          mix_port([
            "run",
            "--no-halt",
            "--no-compile",
            "--no-start",
            "--eval",
            ~s/IO.puts("ok"); IO.gets(""); System.halt()/
          ])

        assert_receive {^port, {:data, "ok\n"}}, timeout
        send(parent, :mix_started)

        send_port_outputs_to(port, parent)
      end)

      assert_receive :mix_started, timeout

      # Project compilation

      output = mix(["do", "compile", "+", "eval", "IO.write System.pid"])
      os_pid = output |> String.split("\n") |> List.last()

      assert_receive {:output, output}, timeout

      assert output == """
             Received :modules_compiled with
               added: [:a, :b, :c], changed: [], removed: []
               app: :with_reloader
               scm: Mix.SCM.Path
               os_pid: "#{os_pid}"
             """

      assert_receive {:output, output}, timeout

      assert output == """
             Received :modules_compiled with
               added: [A, B, C], changed: [], removed: []
               app: :with_reloader
               scm: Mix.SCM.Path
               os_pid: "#{os_pid}"
             """

      # Changed
      File.write!("lib/a.ex", "defmodule A do @moduledoc false end")
      File.write!("src/a.erl", "-module(a). -export([a/0]). a() -> ok.")
      File.touch!("src/a.erl", System.os_time(:second) + 10)
      # Removed
      File.rm!("lib/b.ex")
      File.rm!("src/b.erl")
      # New
      File.write!("lib/d.ex", "defmodule D do end")
      File.write!("src/d.erl", "-module(d).")

      # Project recompilation

      output = mix(["do", "compile", "+", "eval", "IO.write System.pid"])
      os_pid = output |> String.split("\n") |> List.last()

      assert_receive {:output, output}, timeout

      assert output == """
             Received :modules_compiled with
               added: [:d], changed: [:a], removed: [:b]
               app: :with_reloader
               scm: Mix.SCM.Path
               os_pid: "#{os_pid}"
             """

      assert_receive {:output, output}, timeout

      assert output == """
             Received :modules_compiled with
               added: [D], changed: [A], removed: [B]
               app: :with_reloader
               scm: Mix.SCM.Path
               os_pid: "#{os_pid}"
             """

      # Dependency recompilation

      output = mix(["do", "deps.compile", "--force", "+", "eval", "IO.write System.pid"])
      os_pid = output |> String.split("\n") |> List.last()

      assert_receive {:output, output}, timeout

      assert output == """
             Received :modules_compiled with
               added: [Reloader], changed: [], removed: []
               app: :reloader
               scm: Mix.SCM.Path
               os_pid: "#{os_pid}"
             """

      assert_receive {:output, output}, timeout

      assert output == """
             Received :dep_compiled with
               app: :reloader
               scm: Mix.SCM.Path
               manager: :mix
               os_pid: "#{os_pid}"
             """
    end)
  end

  defp send_port_outputs_to(port, pid) do
    receive do
      {^port, {:data, output}} ->
        send(pid, {:output, output})
        send_port_outputs_to(port, pid)
    end
  end
end
