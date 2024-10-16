Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Compile.ElixirTest do
  use MixTest.Case

  import ExUnit.CaptureIO
  alias Mix.Task.Compiler.Diagnostic

  def trace(event, env) do
    send(__MODULE__, {event, env})
    :ok
  end

  @old_time {{2010, 1, 1}, {0, 0, 0}}
  @elixir_otp_version {System.version(), :erlang.system_info(:otp_release)}

  test "compiles a project without per environment build" do
    Mix.ProjectStack.post_config(build_per_environment: false)

    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.write!("lib/a.ex", """
      defmodule A, do: :ok

      # Also make sure that we access the ebin directory during compilation
      true = to_charlist(Mix.Project.compile_path()) in :code.get_path()
      """)

      Mix.Tasks.Compile.Elixir.run(["--verbose"])

      assert File.regular?("_build/shared/lib/sample/ebin/Elixir.A.beam")
      assert File.regular?("_build/shared/lib/sample/ebin/Elixir.B.beam")

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  end

  test "compiles a project with per environment build" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.write!("lib/a.ex", """
      defmodule A, do: :ok

      # Also make sure that we access the ebin directory during compilation
      true = to_charlist(Mix.Project.compile_path()) in :code.get_path()
      """)

      Mix.Tasks.Compile.Elixir.run(["--verbose"])

      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.B.beam")

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  end

  test "compiles a project with custom tracer" do
    Process.register(self(), __MODULE__)

    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      Mix.Tasks.Compile.Elixir.run(["--tracer", "Mix.Tasks.Compile.ElixirTest"])
      assert_received {{:on_module, _, :none}, %{module: A}}
      assert_received {{:on_module, _, :none}, %{module: B}}
    end)
  after
    Code.put_compiler_option(:tracers, [])
  end

  test "compiles a project with a previously set custom tracer" do
    Process.register(self(), __MODULE__)
    Code.put_compiler_option(:tracers, [__MODULE__])

    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      Mix.Tasks.Compile.Elixir.run([])
      assert_received {{:on_module, _, :none}, %{module: A}}
      assert_received {{:on_module, _, :none}, %{module: B}}
    end)
  after
    Code.put_compiler_option(:tracers, [])
  end

  test "recompiles project if elixirc_options changed" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}

      Mix.Task.clear()
      Mix.ProjectStack.merge_config(xref: [exclude: [Foo]])
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
    end)
  end

  test "recompiles files using Mix.Project if mix.exs changes" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample, __ENV__.file)

      File.write!("lib/a.ex", """
      defmodule A do
        Mix.Project.config()
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      ensure_touched(__ENV__.file, "_build/dev/lib/sample/.mix/compile.elixir")
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Now remove the dependency
      File.write!("lib/a.ex", """
      defmodule A do
      end
      """)

      ensure_touched(__ENV__.file, "_build/dev/lib/sample/.mix/compile.elixir")
      File.touch!("_build/dev/lib/sample/.mix/compile.elixir", @old_time)
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      assert File.stat!("_build/dev/lib/sample/.mix/compile.elixir").mtime > @old_time

      ensure_touched(__ENV__.file, "_build/dev/lib/sample/.mix/compile.elixir")
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      refute_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  end

  test "recompiles files when config changes" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      Process.put({MixTest.Case.Sample, :application}, extra_applications: [:logger])
      File.mkdir_p!("config")

      File.write!("lib/a.ex", """
      defmodule A do
        _ = Logger.metadata()
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      recompile = fn ->
        Mix.ProjectStack.pop()
        Mix.Project.push(MixTest.Case.Sample)
        ensure_touched("config/config.exs", "_build/dev/lib/sample/.mix/compile.elixir")
        Mix.Tasks.Loadconfig.load_compile("config/config.exs")
        Mix.Tasks.Compile.Elixir.run(["--verbose"])
      end

      # Adding config recompiles
      File.write!("config/config.exs", """
      import Config
      config :logger, :level, :debug
      """)

      assert recompile.() == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Changing config recompiles
      File.write!("config/config.exs", """
      import Config
      config :logger, :level, :info
      """)

      assert recompile.() == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Removing config recompiles
      File.write!("config/config.exs", """
      import Config
      """)

      assert recompile.() == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # No-op does not recompile
      assert recompile.() == {:ok, []}
      refute_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Changing self fully recompiles
      File.write!("config/config.exs", """
      import Config
      config :sample, :foo, :bar
      """)

      assert recompile.() == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Changing an unknown dependency returns :ok but does not recompile
      File.write!("config/config.exs", """
      import Config
      config :sample, :foo, :bar
      config :unknown, :unknown, :unknown
      """)
    end)
  after
    Application.delete_env(:sample, :foo, persistent: true)
  end

  defdelegate dbg(code, options, env), to: Macro

  test "recompiles only config files when elixir config changes" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.write!("lib/a.ex", """
      defmodule A do
        def a, do: dbg(:ok)
      end
      """)

      File.write!("lib/b.ex", """
      defmodule B do
        def b, do: :ok
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Change the dbg_callback at runtime
      File.touch!("_build/dev/lib/sample/.mix/compile.elixir", @old_time)
      Application.put_env(:elixir, :dbg_callback, {__MODULE__, :dbg, []})

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      assert File.stat!("_build/dev/lib/sample/.mix/compile.elixir").mtime > @old_time
    end)
  after
    Application.put_env(:elixir, :dbg_callback, {Macro, :dbg, []})
  end

  test "recompiles files when config changes export dependencies" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      Process.put({MixTest.Case.Sample, :application}, extra_applications: [:ex_unit])
      File.mkdir_p!("config")

      File.write!("lib/a.ex", """
      defmodule A do
        def test_struct do
          %ExUnit.Test{}
        end
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      recompile = fn ->
        Mix.ProjectStack.pop()
        Mix.Project.push(MixTest.Case.Sample)
        ensure_touched("config/config.exs", "_build/dev/lib/sample/.mix/compile.elixir")
        Mix.Tasks.Loadconfig.load_compile("config/config.exs")
        Mix.Tasks.Compile.Elixir.run(["--verbose"])
      end

      # Adding config recompiles
      File.write!("config/config.exs", """
      import Config
      config :ex_unit, :some, :config
      """)

      assert recompile.() == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Changing config recompiles
      File.write!("config/config.exs", """
      import Config
      config :ex_unit, :some, :another
      """)

      assert recompile.() == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Removing config recompiles
      File.write!("config/config.exs", """
      import Config
      """)

      assert recompile.() == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # No-op does not recompile
      assert recompile.() == {:ok, []}
      refute_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  after
    Application.delete_env(:ex_unit, :some, persistent: true)
  end

  test "recompiles files when config changes with crashes" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      Process.put({MixTest.Case.Sample, :application}, extra_applications: [:logger])
      File.mkdir_p!("config")

      File.write!("lib/a.ex", """
      defmodule A do
        require Logger
        def fun, do: Logger.debug("hello")
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      recompile = fn ->
        Mix.ProjectStack.pop()
        Mix.Project.push(MixTest.Case.Sample)
        ensure_touched("config/config.exs", "_build/dev/lib/sample/.mix/compile.elixir")
        Mix.Tasks.Loadconfig.load_compile("config/config.exs")
        Mix.Tasks.Compile.Elixir.run(["--verbose"])
      end

      # Adding config recompiles due to macros
      File.write!("config/config.exs", """
      import Config
      config :logger, :compile_time_purge_matching, []
      """)

      assert recompile.() == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Inserting a bogus config should crash
      File.write!("config/config.exs", """
      import Config
      config :logger, :compile_time_purge_matching, [level_lower_than: :debug]
      """)

      ExUnit.CaptureIO.capture_io(:stderr, fn -> assert {:error, _} = recompile.() end)

      # Revering the original config should recompile
      File.write!("config/config.exs", """
      import Config
      config :logger, :compile_time_purge_matching, []
      """)

      assert recompile.() == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  after
    Application.put_env(:logger, :compile_time_purge_matching, [])
  end

  test "recompiles files when config changes via compile_env" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      File.mkdir_p!("config")

      File.write!("lib/a.ex", """
      defmodule A do
        _ = Application.compile_env(:logger, :level)
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      recompile = fn ->
        Mix.ProjectStack.pop()
        Mix.Project.push(MixTest.Case.Sample)
        ensure_touched("config/config.exs", "_build/dev/lib/sample/.mix/compile.elixir")
        Mix.Tasks.Loadconfig.load_compile("config/config.exs")
        Mix.Tasks.Compile.Elixir.run(["--verbose"])
      end

      # Adding config recompiles
      File.write!("config/config.exs", """
      import Config
      config :logger, :level, :debug
      """)

      assert recompile.() == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Changing config recompiles
      File.write!("config/config.exs", """
      import Config
      config :logger, :level, :info
      """)

      assert recompile.() == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Removing config recompiles
      File.write!("config/config.exs", """
      import Config
      """)

      assert recompile.() == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Changing self fully recompiles
      File.write!("config/config.exs", """
      import Config
      config :sample, :foo, :bar
      """)

      assert recompile.() == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Changing an unknown dependency returns :ok but does not recompile
      File.write!("config/config.exs", """
      import Config
      config :sample, :foo, :bar
      config :unknown, :unknown, :unknown
      """)

      # We use ensure_touched because an outdated manifest would recompile anyway.
      assert recompile.() == {:ok, []}
      refute_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  after
    Application.delete_env(:sample, :foo, persistent: true)
  end

  test "recompiles files when lock changes" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      Process.put({MixTest.Case.Sample, :application}, extra_applications: [:logger])

      File.write!("lib/a.ex", """
      defmodule A do
        _ = Logger.metadata()
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      recompile = fn ->
        Mix.ProjectStack.pop()
        Mix.Project.push(MixTest.Case.Sample)
        Mix.Tasks.WillRecompile.run([])
        Mix.Tasks.Compile.Elixir.run(["--verbose"])
      end

      # Adding to lock recompiles
      File.write!("mix.lock", """
      %{"logger": :unused}
      """)

      ensure_touched("mix.lock")
      File.touch!("_build/dev/lib/sample/.mix/compile.elixir", @old_time)
      assert recompile.() == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      assert File.stat!("_build/dev/lib/sample/.mix/compile.elixir").mtime > @old_time

      # Changing lock recompiles
      File.write!("mix.lock", """
      %{"logger": :another}
      """)

      ensure_touched("mix.lock")
      File.touch!("_build/dev/lib/sample/.mix/compile.elixir", @old_time)
      assert recompile.() == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      assert File.stat!("_build/dev/lib/sample/.mix/compile.elixir").mtime > @old_time

      # Removing a lock recompiles
      File.write!("mix.lock", """
      %{}
      """)

      ensure_touched("mix.lock")
      File.touch!("_build/dev/lib/sample/.mix/compile.elixir", @old_time)
      assert recompile.() == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      assert File.stat!("_build/dev/lib/sample/.mix/compile.elixir").mtime > @old_time

      # Adding an unknown dependency returns :ok but does not recompile
      File.write!("mix.lock", """
      %{"unknown": :unknown}
      """)

      # We use ensure_touched because an outdated manifest would recompile anyway.
      ensure_touched("mix.lock", "_build/dev/lib/sample/.mix/compile.elixir")
      assert recompile.() == {:ok, []}
      refute_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  end

  test "recompiles files using Erlang modules if Erlang manifest changes" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      File.mkdir_p!("src")

      File.write!("src/foo.erl", """
      -module(foo).
      -export([bar/0]).
      bar() -> ok.
      """)

      File.write!("src/bar.erl", """
      -module(bar).
      -export([baz/0]).
      baz() -> ok.
      """)

      File.write!("lib/a.ex", """
      defmodule A do
        :foo.bar()
      end
      """)

      assert Mix.Tasks.Compile.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      Mix.Task.clear()
      File.touch!("_build/dev/lib/sample/.mix/compile.elixir", @old_time)

      assert Mix.Tasks.Compile.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Now remove the Erlang file, lib/a.ex must recompile
      File.rm!("src/foo.erl")
      File.touch!("_build/dev/lib/sample/.mix/compile.erlang", @old_time)

      Mix.Task.clear()
      assert Mix.Tasks.Compile.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
    end)
  end

  test "recompiles project if Elixir version changes" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      Mix.Tasks.Compile.run([])
      purge([A, B])

      assert File.exists?("_build/dev/lib/sample")
      assert File.exists?("_build/dev/lib/sample/consolidated")
      assert Mix.Dep.ElixirSCM.read() == {:ok, @elixir_otp_version, Mix.SCM.Path}

      Mix.Task.clear()
      File.write!("_build/dev/lib/sample/consolidated/.to_be_removed", "")
      manifest_data = :erlang.term_to_binary({:v1, "0.0.0", nil})
      File.write!("_build/dev/lib/sample/.mix/compile.elixir_scm", manifest_data)
      File.touch!("_build/dev/lib/sample/.mix/compile.elixir_scm", @old_time)

      Mix.Tasks.Compile.run([])
      assert Mix.Dep.ElixirSCM.read() == {:ok, @elixir_otp_version, Mix.SCM.Path}

      assert File.stat!("_build/dev/lib/sample/.mix/compile.elixir_scm").mtime >
               @old_time

      refute File.exists?("_build/dev/lib/sample/consolidated/.to_be_removed")
    end)
  end

  test "recompiles project if scm changes" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      Mix.Tasks.Compile.run(["--verbose"])
      purge([A, B])

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert Mix.Dep.ElixirSCM.read() == {:ok, @elixir_otp_version, Mix.SCM.Path}

      Mix.Task.clear()
      manifest_data = :erlang.term_to_binary({1, @elixir_otp_version, :another})
      File.write!("_build/dev/lib/sample/.mix/compile.elixir_scm", manifest_data)
      File.touch!("_build/dev/lib/sample/.mix/compile.elixir_scm", @old_time)

      Mix.Tasks.Compile.run([])
      assert Mix.Dep.ElixirSCM.read() == {:ok, @elixir_otp_version, Mix.SCM.Path}

      assert File.stat!("_build/dev/lib/sample/.mix/compile.elixir_scm").mtime >
               @old_time
    end)
  end

  test "recompiles project if old manifest" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      Mix.Tasks.Compile.run([])
      purge([A, B])

      manifest = "_build/dev/lib/sample/.mix/compile.elixir"

      File.read!(manifest)
      |> :erlang.binary_to_term()
      |> put_elem(0, 9)
      |> :erlang.term_to_binary()
      |> then(&File.write!(manifest, &1))

      Mix.Task.clear()
      assert Mix.Task.run("compile", ["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  end

  test "recompiles files from path dependencies when its deps change" do
    # Get Git repo first revision
    [last, first | _] = get_git_repo_revs("git_repo")

    in_fixture("no_mixfile", fn ->
      File.mkdir_p!("path_on_git_repo/lib")

      File.write!("path_on_git_repo/mix.exs", """
      defmodule PathOnGitRepo.MixProject do
        use Mix.Project

        def project do
          [
            app: :path_on_git_repo,
            version: "0.1.0",
            deps: [{:git_repo, git: MixTest.Case.fixture_path("git_repo")}]
          ]
        end
      end
      """)

      File.write!("path_on_git_repo/lib/path_on_hello.ex", """
      defmodule PathOnGitRepo.Hello do
        IO.puts("GitRepo is defined: \#{Code.ensure_loaded?(GitRepo)}")
      end
      """)

      File.write!("mix.lock", inspect(%{git_repo: {:git, fixture_path("git_repo"), first, []}}))
      Mix.ProjectStack.post_config(deps: [{:path_on_git_repo, path: "path_on_git_repo"}])
      Mix.Project.push(MixTest.Case.Sample)

      Mix.Tasks.Deps.Get.run([])
      assert capture_io(fn -> Mix.Task.run("compile") end) =~ "GitRepo is defined: false"

      Mix.Task.clear()
      Mix.State.clear_cache()
      purge([GitRepo.MixProject, PathOnGitRepo.MixProject, PathOnGitRepo.Hello])

      # Unload the git repo application so we can pick the new modules definition
      :ok = Application.unload(:git_repo)

      Mix.Tasks.Deps.Update.run(["--all"])
      assert File.read!("mix.lock") =~ last

      # The lock of sample (the parent app) is the one that changed
      # but that should mirror on child path dependencies too.
      ensure_touched(
        "_build/dev/lib/sample/.mix/compile.lock",
        "_build/dev/lib/path_on_git_repo/.mix/compile.elixir"
      )

      assert capture_io(fn -> Mix.Task.run("compile") end) =~ "GitRepo is defined: true"
    end)
  after
    purge([GitRepo, GitRepo.MixProject])
  end

  test "does not write BEAM files down on failures" do
    in_tmp("blank", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      File.mkdir_p!("lib")
      File.write!("lib/a.ex", "raise ~s(oops)")

      capture_io(:stderr, fn ->
        assert {:error, [_]} = Mix.Tasks.Compile.Elixir.run([])
      end)

      refute File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
    end)
  end

  test "removes, purges and deletes old artifacts" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      assert Mix.Tasks.Compile.Elixir.run([]) == {:ok, []}
      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      assert Code.ensure_loaded?(A)

      File.rm!("lib/a.ex")
      assert Mix.Tasks.Compile.Elixir.run([]) == {:ok, []}
      refute File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      refute Code.ensure_loaded?(A)
      refute String.contains?(File.read!("_build/dev/lib/sample/.mix/compile.elixir"), "Elixir.A")
    end)
  end

  test "purges consolidation path if asked" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        defstruct []
      end

      defimpl Inspect, for: A do
        def inspect(_, _), do: "sample"
      end
      """)

      Mix.Project.push(MixTest.Case.Sample)
      assert Mix.Tasks.Compile.run([]) == {:ok, []}
      assert inspect(struct(A, [])) == "sample"

      purge([A, B, Inspect.A])
      Mix.Task.clear()

      assert capture_io(:stderr, fn ->
               {:ok, [_]} = Mix.Tasks.Compile.run(["--force"])
             end) =~
               "the Inspect protocol has already been consolidated"

      purge([A, B, Inspect.A])
      Mix.Task.clear()
      consolidation = Mix.Project.consolidation_path()
      args = ["--force", "--purge-consolidation-path-if-stale", consolidation]
      assert Mix.Tasks.Compile.run(args) == {:ok, []}
    end)
  end

  test "recompiles mtime changed files if content changed but not length" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      Mix.shell().flush()
      purge([A, B])

      same_length_content = "lib/a.ex" |> File.read!() |> String.replace("A", "Z")
      File.write!("lib/a.ex", same_length_content)
      future = {{2038, 1, 1}, {0, 0, 0}}
      File.touch!("lib/a.ex", future)
      Mix.Tasks.Compile.Elixir.run(["--verbose"])

      message =
        "warning: mtime (modified time) for \"lib/a.ex\" was set to the future, resetting to now"

      assert_received {:mix_shell, :error, [^message]}

      message =
        "warning: mtime (modified time) for \"lib/b.ex\" was set to the future, resetting to now"

      refute_received {:mix_shell, :error, [^message]}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      File.touch!("_build/dev/lib/sample/.mix/compile.elixir", future)
      assert Mix.Tasks.Compile.Elixir.run([]) == {:noop, []}
    end)
  end

  test "does not recompile mtime changed but identical files" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      Mix.shell().flush()
      purge([A, B])

      future = {{2038, 1, 1}, {0, 0, 0}}
      File.touch!("lib/a.ex", future)
      Mix.Tasks.Compile.Elixir.run(["--verbose"])

      message =
        "warning: mtime (modified time) for \"lib/a.ex\" was set to the future, resetting to now"

      assert_received {:mix_shell, :error, [^message]}

      message =
        "warning: mtime (modified time) for \"lib/b.ex\" was set to the future, resetting to now"

      refute_received {:mix_shell, :error, [^message]}
      refute_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      File.touch!("_build/dev/lib/sample/.mix/compile.elixir", future)
      assert Mix.Tasks.Compile.Elixir.run([]) == {:noop, []}
    end)
  end

  test "does recompile a file restored after a compile error (and .beam file were deleted)" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      Mix.shell().flush()
      purge([A, B])

      # Compile with error
      original_content = File.read!("lib/b.ex")
      File.write!("lib/b.ex", "this will not compile")

      assert capture_io(:stderr, fn ->
               {:error, _} = Mix.Tasks.Compile.Elixir.run(["--verbose"])
             end) =~ "Compilation error in file lib/b.ex"

      assert_received {:mix_shell, :info, ["Compiling 1 file (.ex)"]}

      # Revert change
      File.write!("lib/b.ex", original_content)
      future = {{2038, 1, 1}, {0, 0, 0}}
      File.touch!("lib/b.ex", future)

      Mix.Tasks.Compile.Elixir.run(["--verbose"])

      message =
        "warning: mtime (modified time) for \"lib/b.ex\" was set to the future, resetting to now"

      assert_received {:mix_shell, :error, [^message]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
    end)
  end

  test "recompiles size changed files" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      past = @old_time
      File.touch!("lib/a.ex", past)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      Mix.shell().flush()
      purge([A, B])

      File.write!("lib/a.ex", File.read!("lib/a.ex") <> "\n")
      File.touch!("lib/a.ex", past)
      Mix.Tasks.Compile.Elixir.run(["--verbose"])

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  end

  test "recompiles dependent changed modules" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      File.write!("lib/a.ex", "defmodule A, do: B.module_info()")

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      Mix.shell().flush()
      purge([A, B])

      force_recompilation("lib/b.ex")
      Mix.Tasks.Compile.Elixir.run(["--verbose"])

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  end

  test "recompiles dependent changed modules without beam files" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.write!("lib/b.ex", """
      defmodule B do
        def a, do: A.__info__(:module)
      end
      """)

      Mix.Tasks.Compile.Elixir.run(["--verbose"])
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.B.beam")

      # Compile directly so it does not point to a .beam file
      Code.put_compiler_option(:ignore_module_conflict, true)
      Code.compile_file("lib/b.ex")

      force_recompilation("lib/a.ex")
      Mix.Tasks.Compile.Elixir.run(["--verbose"])
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
    end)
  after
    Code.put_compiler_option(:ignore_module_conflict, false)
  end

  test "recompiles dependent changed modules even on removal" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      File.write!("lib/a.ex", "defmodule A, do: B.module_info()")

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      Mix.shell().flush()
      purge([A, B])

      File.rm("lib/b.ex")
      File.write!("lib/a.ex", "defmodule A, do: nil")
      Mix.Tasks.Compile.Elixir.run(["--verbose"])

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  end

  test "recompiles dependent changed on conflict" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      purge([A, B])

      capture_io(:stderr, fn ->
        File.write!("lib/a.ex", "defmodule B, do: :not_ok")
        assert {:ok, [_ | _]} = Mix.Tasks.Compile.Elixir.run(["--verbose"])
        assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
        assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
        purge([A, B])
      end)

      capture_io(:stderr, fn ->
        File.write!("lib/a.ex", "defmodule A, do: :ok")
        assert {:ok, []} = Mix.Tasks.Compile.Elixir.run(["--verbose"])
        assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
        assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
        purge([A, B])
      end)
    end)
  end

  test "recompiles dependent changed external resources" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      tmp = tmp_path("c.eex")
      File.touch!("lib/a.eex")

      File.write!("lib/a.ex", """
      defmodule A do
        # Listing directories as external resources
        # is not valid but let's ensure we don't crash
        @external_resource "lib"
        @external_resource "lib/a.eex"
        @external_resource #{inspect(tmp)}
        def a, do: :ok
      end
      """)

      # Compiles with missing external resources
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:noop, []}
      Mix.shell().flush()
      purge([A, B])

      # Update local existing resource timestamp is not enough
      File.touch!("lib/a.eex", {{2038, 1, 1}, {0, 0, 0}})
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:noop, []}

      force_recompilation("lib/a.eex")
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Does not update on old existing resource
      File.touch!("lib/a.eex", @old_time)
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:noop, []}
      Mix.shell().flush()
      purge([A, B])

      # Create external resource
      File.touch!(tmp, {{2038, 1, 1}, {0, 0, 0}})
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Recompiles once resource is deleted
      File.rm_rf!(tmp)
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Recompiles if directories are stale
      File.touch!("lib", {{2038, 1, 1}, {0, 0, 0}})
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  after
    File.rm(tmp_path("c.eex"))
  end

  test "tracks warnings from external resources" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      File.touch!("lib/a.eex")

      File.write!("lib/a.ex", """
      defmodule A do
        @external_resource "lib/a.eex"
        IO.warn("oops", file: Path.absname("lib/a.eex"), line: 13)
      end
      """)

      # Compiles with missing external resources
      file = Path.absname("lib/a.eex")

      assert capture_io(:stderr, fn ->
               assert {:ok, [%Mix.Task.Compiler.Diagnostic{file: ^file, position: 13}]} =
                        Mix.Tasks.Compile.Elixir.run([])

               assert {:noop, [%Mix.Task.Compiler.Diagnostic{file: ^file, position: 13}]} =
                        Mix.Tasks.Compile.Elixir.run(["--all-warnings"])
             end) =~ "oops"

      purge([A])
    end)
  end

  test "recompiles modules with exports tracking" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.write!("lib/a.ex", """
      defmodule A do
        defstruct [:foo]
      end
      """)

      File.write!("lib/b.ex", """
      defmodule B do
        def fun do
          %A{foo: 1}
        end
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      purge([A, B])

      File.write!("lib/a.ex", """
      defmodule A do
        # Some comments
        defstruct [:foo]
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      purge([A, B])

      File.write!("lib/a.ex", """
      defmodule A do
        defstruct [:foo, :bar]
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      purge([A, B])

      File.write!("lib/a.ex", """
      defmodule A do
        @enforce_keys [:foo]
        defstruct [:foo, :bar]
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      purge([A, B])

      File.write!("lib/a.ex", """
      defmodule A do
        @enforce_keys [:foo]
        defstruct [:foo, :bar]
        def some_fun, do: :ok
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      purge([A, B])

      # Remove all code, we should now get a compilation error
      File.write!("lib/a.ex", """
      """)

      assert capture_io(:stderr, fn ->
               {:error, _} = Mix.Tasks.Compile.Elixir.run(["--verbose"])
             end) =~ "A.__struct__/1 is undefined, cannot expand struct A"

      # At the code back and it should work again
      File.write!("lib/a.ex", """
      defmodule A do
        defstruct [:foo, :bar]
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      purge([A, B])

      # Removing the file should have the same effect as removing all code
      File.rm!("lib/a.ex")

      assert capture_io(:stderr, fn ->
               {:error, _} = Mix.Tasks.Compile.Elixir.run(["--verbose"])
             end) =~ "A.__struct__/1 is undefined, cannot expand struct A"
    end)
  end

  test "recompiles modules with async tracking" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.write!("lib/a.ex", """
      Kernel.ParallelCompiler.async(fn ->
        defmodule A do
          def fun, do: :ok
        end
      end) |> Task.await()
      """)

      File.write!("lib/b.ex", """
      defmodule B do
        A.fun()
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      purge([A, B])

      force_recompilation("lib/a.ex")

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      purge([A, B])
    end)
  end

  test "recompiles modules with multiple sources" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.write!("lib/a.ex", """
      defmodule A do
        def one, do: 1
      end

      defmodule B do
        def two, do: 2
      end
      """)

      File.write!("lib/b.ex", """
      B.two()

      defmodule A do
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose", "--ignore-module-conflict"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      refute function_exported?(A, :one, 0)

      Mix.shell().flush()
      purge([A])

      File.rm("lib/b.ex")
      Mix.Tasks.Compile.Elixir.run(["--verbose"])
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      assert function_exported?(A, :one, 0)
    end)
  end

  test "recompiles with --force" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      purge([A, B])

      # Now we have a noop
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:noop, []}

      # --force
      assert Mix.Tasks.Compile.Elixir.run(["--force", "--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
    end)
  end

  test "compiles files with autoload disabled" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.write!("lib/a.ex", """
      defmodule A do
        @compile {:autoload, false}
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      purge([A, B])
    end)
  end

  test "does not recompile files that are empty or have no code" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      File.write!("lib/a.ex", "")
      File.write!("lib/b.ex", "# Just a comment")
      File.write!("lib/c.ex", "\n\n")

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/c.ex"]}

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:noop, []}
      refute_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/c.ex"]}
    end)
  end

  test "recompiles modules with __mix_recompile__ check" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.write!("lib/a.ex", """
      defmodule A do
        def __mix_recompile__?(), do: true
      end
      """)

      File.write!("lib/b.ex", """
      defmodule B do
        def __mix_recompile__?(), do: false
      end
      """)

      File.write!("lib/c.ex", """
      defmodule C do
        @compile {:autoload, false}

        def __mix_recompile__?(), do: true
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiling 3 files (.ex)"]}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/c.ex"]}

      # Mix recompile should work even if the compile path
      # was removed and the module purged
      Code.delete_path(Mix.Project.compile_path())
      purge([A])

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiling 1 file (.ex)"]}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}

      File.rm!("lib/a.ex")
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      refute_received _
    end)
  end

  test "recompiles modules with __mix_recompile__ check with crashes" do
    Agent.start_link(fn -> false end, name: :mix_recompile_raise)

    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.write!("lib/a.ex", """
      defmodule A do
        def __mix_recompile__?(), do: Agent.get(:mix_recompile_raise, & &1)

        if Agent.get(:mix_recompile_raise, & &1) do
          raise "oops"
        end
      end
      """)

      File.touch!("lib/a.ex", @old_time)
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:noop, []}
      assert File.stat!("lib/a.ex").mtime == @old_time

      Agent.update(:mix_recompile_raise, fn _ -> true end)

      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        assert {:error, _} = Mix.Tasks.Compile.Elixir.run(["--verbose"])
      end)

      # After failing to compile and reverting the status, it should still recompile
      Agent.update(:mix_recompile_raise, fn _ -> false end)
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
    end)
  end

  test "prints warnings from non-stale files with --all-warnings" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.write!("lib/a.ex", """
      defmodule A do
        def my_fn(unused), do: :ok
      end
      """)

      # First compilation should print unused variable warning
      assert capture_io(:stderr, fn ->
               Mix.Tasks.Compile.Elixir.run([]) == :ok
             end) =~ "variable \"unused\" is unused"

      assert capture_io(:stderr, fn ->
               Mix.Tasks.Compile.Elixir.run([])
             end) =~ "variable \"unused\" is unused"

      assert capture_io(:stderr, fn ->
               Mix.Tasks.Compile.Elixir.run(["--no-all-warnings"])
             end) == ""

      # Should not print warning once fixed
      File.write!("lib/a.ex", """
      defmodule A do
        def my_fn(_unused), do: :ok
      end
      """)

      assert capture_io(:stderr, fn ->
               Mix.Tasks.Compile.Elixir.run(["--all-warnings"])
             end) == ""
    end)
  end

  test "warning from --all-warnings are treated as errors with --warnings-as-errors" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.write!("lib/a.ex", """
      defmodule A do
        def my_fn(unused), do: :ok
      end
      """)

      message = "variable \"unused\" is unused"

      assert capture_io(:stderr, fn ->
               Mix.Tasks.Compile.Elixir.run([]) == :ok
             end) =~ message

      # Stale compilation fails due to warnings as errors
      assert capture_io(:stderr, fn ->
               catch_exit(Mix.Task.run("compile", ["--all-warnings", "--warnings-as-errors"]))
             end) =~ message

      Mix.Task.clear()

      File.write!("lib/b.ex", """
      defmodule B do
      end
      """)

      # Recompiling a new file still fails due to warnings as errors
      assert capture_io(:stderr, fn ->
               catch_exit(Mix.Task.run("compile", ["--all-warnings", "--warnings-as-errors"]))
             end) =~ message
    end)
  end

  test "returns warning diagnostics" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.write!("lib/a.ex", """
      defmodule A do
        def my_fn(unused), do: :ok
      end
      """)

      file = Path.absname("lib/a.ex")

      message =
        "variable \"unused\" is unused (if the variable is not meant to be used, prefix it with an underscore)"

      capture_io(:stderr, fn ->
        assert {:ok, [diagnostic]} = Mix.Tasks.Compile.Elixir.run([])

        assert %Diagnostic{
                 file: ^file,
                 source: ^file,
                 severity: :warning,
                 position: {2, 13},
                 compiler_name: "Elixir",
                 message: ^message
               } = diagnostic
      end)

      # Recompiling should return :noop status because nothing is stale,
      # but also include previous warning diagnostics
      capture_io(:stderr, fn ->
        assert {:noop, [diagnostic]} = Mix.Tasks.Compile.Elixir.run([])

        assert %Diagnostic{
                 file: ^file,
                 source: ^file,
                 severity: :warning,
                 position: {2, 13},
                 compiler_name: "Elixir",
                 message: ^message
               } = diagnostic

        assert [^diagnostic] = Mix.Tasks.Compile.Elixir.diagnostics()
        assert [^diagnostic] = Mix.Task.Compiler.diagnostics()
      end)
    end)
  end

  test "returns warning diagnostics for external files" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.write!("lib/a.ex", """
      IO.warn "warning", [{nil, nil, 0, file: ~c"lib/foo.txt", line: 3}]
      """)

      capture_io(:stderr, fn ->
        assert {:ok, [diagnostic]} = Mix.Tasks.Compile.Elixir.run([])

        assert %Diagnostic{
                 file: nil,
                 severity: :warning,
                 position: 0,
                 compiler_name: "Elixir",
                 message: "warning",
                 stacktrace: [{nil, nil, 0, [file: ~c"lib/foo.txt", line: 3]}]
               } = diagnostic
      end)
    end)
  end

  test "returns error diagnostics", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(MixTest.Case.Sample)
      File.mkdir_p!("lib")

      File.write!("lib/b.ex", """
      defmodule B do
        def unused_var(unused_var), do: :ok
      end
      """)

      assert capture_io(:stderr, fn ->
               assert {:ok, [_diagnostic]} = Mix.Tasks.Compile.Elixir.run([])
             end) =~ "unused_var"

      file = Path.absname("lib/a.ex")

      File.write!("lib/a.ex", """
      defmodule A do
        def my_fn(), do: $$$
      end
      """)

      refute capture_io(:stderr, fn ->
               purge([B])
               assert {:error, [_warning, error]} = Mix.Tasks.Compile.Elixir.run([])

               assert %Diagnostic{
                        file: ^file,
                        source: ^file,
                        severity: :error,
                        position: {2, 20},
                        message: "** (SyntaxError) invalid syntax found on lib/a.ex:2:" <> _,
                        compiler_name: "Elixir"
                      } = error
             end) =~ "unused_var"

      assert capture_io(:stderr, fn ->
               purge([B])

               assert {:error, [_warning, _error]} =
                        Mix.Tasks.Compile.Elixir.run(["--all-warnings"])
             end) =~ "unused_var"
    end)
  end

  test "returns error diagnostics for invalid struct key", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(MixTest.Case.Sample)
      File.mkdir_p!("lib")

      File.write!("lib/a.ex", """
      defmodule A do
        def my_fn(), do: %Date{invalid_key: 2020}
      end
      """)

      file = Path.absname("lib/a.ex")

      capture_io(:stderr, fn ->
        assert {:error, [diagnostic]} = Mix.Tasks.Compile.Elixir.run([])

        assert %Diagnostic{
                 file: ^file,
                 source: ^file,
                 severity: :error,
                 position: 2,
                 message: "** (KeyError) key :invalid_key not found" <> _,
                 compiler_name: "Elixir"
               } = diagnostic
      end)
    end)
  end

  test "returns error diagnostics when deadlocked" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.write!("lib/a.ex", """
      defmodule A do
        B.__info__(:module)
      end
      """)

      File.write!("lib/b.ex", """
      defmodule B do
        A.__info__(:module)
      end
      """)

      capture_io(:stderr, fn ->
        assert {:error, errors} = Mix.Tasks.Compile.Elixir.run([])
        errors = Enum.sort_by(errors, &Map.get(&1, :file))

        file_a = Path.absname("lib/a.ex")
        file_b = Path.absname("lib/b.ex")

        assert [
                 %Diagnostic{file: ^file_a, message: "deadlocked waiting on module B"},
                 %Diagnostic{file: ^file_b, message: "deadlocked waiting on module A"}
               ] = errors
      end)
    end)
  end

  test "verify runtime dependent modules that haven't been compiled" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)

      File.write!("lib/a.ex", """
      defmodule A do
        def foo(), do: :ok
      end
      """)

      File.write!("lib/b.ex", """
      defmodule B do
        def foo(), do: A.foo()
      end
      """)

      File.write!("lib/c.ex", """
      defmodule C do
        @after_verify __MODULE__
        def foo(), do: B.foo()
        def bar(), do: B.bar()
        def __after_verify__(__MODULE__) do
          IO.warn("AFTER_VERIFY", __ENV__)
        end
      end
      """)

      output =
        capture_io(:stderr, fn ->
          Mix.Tasks.Compile.Elixir.run(["--verbose"])
        end)

      refute output =~ "A.foo/0 is undefined or private"
      assert output =~ "B.bar/0 is undefined or private"
      assert output =~ "AFTER_VERIFY"

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/c.ex"]}

      File.write!("lib/a.ex", """
      defmodule A do
      end
      """)

      output =
        capture_io(:stderr, fn ->
          Mix.Tasks.Compile.Elixir.run(["--verbose", "--no-all-warnings"])
        end)

      # Check B due to direct dependency on A
      assert output =~ "A.foo/0 is undefined or private"
      refute output =~ "B.bar/0 is undefined or private"
      refute output =~ "AFTER_VERIFY"

      # Ensure only A was recompiled
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/c.ex"]}

      # We can retrieve all warnings if desired
      output =
        capture_io(:stderr, fn ->
          Mix.Tasks.Compile.Elixir.run(["--verbose", "--all-warnings"])
        end)

      assert output =~ "A.foo/0 is undefined or private"
      assert output =~ "B.bar/0 is undefined or private"
      assert output =~ "AFTER_VERIFY"

      # Now we change B and it must emit an AFTER_VERIFY warning
      File.write!("lib/b.ex", """
      defmodule B do
      end
      """)

      output =
        capture_io(:stderr, fn ->
          Mix.Tasks.Compile.Elixir.run(["--verbose", "--all-warnings"])
        end)

      assert output =~ "B.foo/0 is undefined or private"
      assert output =~ "B.bar/0 is undefined or private"
      assert output =~ "AFTER_VERIFY"
    end)
  end

  defmodule GitApp do
    def project do
      [
        app: :git_app,
        version: "0.1.0",
        deps: [
          {:git_repo, "0.1.0", git: fixture_path("git_repo"), optional: true}
        ]
      ]
    end
  end

  test "compiles without optional dependencies" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(GitApp)

      File.write!("lib/a.ex", """
      defmodule A do
        def hello, do: GitRepo.hello()
      end
      """)

      assert capture_io(:stderr, fn ->
               Mix.Tasks.Compile.run(["--no-optional-deps"]) == :ok
             end) =~ "GitRepo.hello/0 is undefined"
    end)
  end

  test "recompiles if --no-optional-deps change" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(MixTest.Case.Sample)
      assert Mix.Tasks.Compile.Elixir.run([]) == {:ok, []}
      assert Mix.Tasks.Compile.Elixir.run([]) == {:noop, []}
      assert Mix.Tasks.Compile.Elixir.run(["--no-optional-deps"]) == {:ok, []}
    end)
  end
end
