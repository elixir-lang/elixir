Code.require_file("test_helper.exs", __DIR__)

defmodule MixTest do
  use MixTest.Case

  test "shell" do
    assert Mix.shell() == Mix.Shell.Process
  end

  test "env" do
    assert Mix.env() == :dev
    Mix.env(:prod)
    assert Mix.env() == :prod
  end

  test "debug" do
    refute Mix.debug?()
    Mix.debug(true)
    assert Mix.debug?()
    Mix.debug(false)
  end

  describe "install" do
    @describetag :tmp_dir

    setup %{tmp_dir: tmp_dir} do
      System.put_env("MIX_INSTALL_DIR", Path.join(tmp_dir, "installs"))
    end

    setup :test_project

    test "default options", %{tmp_dir: tmp_dir} do
      Mix.install([
        {:install_test, path: Path.join(tmp_dir, "install_test")}
      ])

      assert File.dir?(Path.join(tmp_dir, "installs"))

      assert Protocol.consolidated?(InstallTest.Protocol)

      assert_received {:mix_shell, :info, ["==> install_test"]}
      assert_received {:mix_shell, :info, ["Compiling 2 files (.ex)"]}
      assert_received {:mix_shell, :info, ["Generated install_test app"]}
      refute_received _

      assert Application.app_dir(:crypto) =~ "crypto"
      assert List.keyfind(Application.started_applications(), :install_test, 0)
      assert apply(InstallTest, :hello, []) == :world
    end

    test "with start_applications: false", %{tmp_dir: tmp_dir} do
      Mix.install(
        [
          {:install_test, path: Path.join(tmp_dir, "install_test")}
        ],
        start_applications: false
      )

      assert List.keyfind(Application.loaded_applications(), :install_test, 0)
      refute List.keyfind(Application.started_applications(), :install_test, 0)
    end

    test "with runtime: false", %{tmp_dir: tmp_dir} do
      Mix.install([
        {:install_test, path: Path.join(tmp_dir, "install_test"), runtime: false}
      ])

      assert File.dir?(Path.join(tmp_dir, "installs"))
      assert_received {:mix_shell, :info, ["==> install_test"]}
      assert_received {:mix_shell, :info, ["Compiling 2 files (.ex)"]}
      assert_received {:mix_shell, :info, ["Generated install_test app"]}
      refute_received _

      refute List.keyfind(Application.started_applications(), :install_test, 0)
    end

    test "works with same deps twice", %{tmp_dir: tmp_dir} do
      Mix.install([
        {:install_test, path: Path.join(tmp_dir, "install_test")}
      ])

      Mix.install([
        {:install_test, path: Path.join(tmp_dir, "install_test")}
      ])
    end

    test "errors on Elixir version mismatch", %{tmp_dir: tmp_dir} do
      assert_raise Mix.Error, ~r"Mix.install/2 declared it supports only Elixir ~> 2.0", fn ->
        Mix.install(
          [
            {:install_test, path: Path.join(tmp_dir, "install_test")}
          ],
          elixir: "~> 2.0"
        )
      end
    end

    test "errors with same deps and force", %{tmp_dir: tmp_dir} do
      Mix.install([
        {:install_test, path: Path.join(tmp_dir, "install_test")}
      ])

      assert_raise Mix.Error, ~r"Mix.install/2 can only be called", fn ->
        Mix.install(
          [
            {:install_test, path: Path.join(tmp_dir, "install_test")}
          ],
          force: true
        )
      end
    end

    test "errors with different deps in the same VM", %{tmp_dir: tmp_dir} do
      Mix.install([
        {:install_test, path: Path.join(tmp_dir, "install_test")}
      ])

      assert_raise Mix.Error, ~r"Mix.install/2 can only be called", fn ->
        Mix.install([
          {:install_test, path: Path.join(tmp_dir, "install_test")},
          :foo
        ])
      end
    end

    test "install after errors", %{tmp_dir: tmp_dir} do
      assert_raise Mix.Error, "Can't continue due to errors on dependencies", fn ->
        Mix.install([
          {:bad, path: Path.join(tmp_dir, "bad")}
        ])
      end

      Mix.install([
        {:install_test, path: Path.join(tmp_dir, "install_test")}
      ])

      assert apply(InstallTest, :hello, []) == :world
    end

    test "consolidate_protocols: false", %{tmp_dir: tmp_dir} do
      Mix.install(
        [
          {:install_test, path: Path.join(tmp_dir, "install_test")}
        ],
        consolidate_protocols: false
      )

      refute Protocol.consolidated?(InstallTest.Protocol)
    end

    test ":config and :system_env", %{tmp_dir: tmp_dir} do
      Mix.install(
        [
          {:install_test, path: Path.join(tmp_dir, "install_test")}
        ],
        config: [unknown_app: [foo: :bar]],
        system_env: %{"MIX_INSTALL_FOO" => "BAR", MIX_INSTALL_BAZ: "BAT"}
      )

      assert Application.fetch_env!(:unknown_app, :foo) == :bar
      assert System.fetch_env!("MIX_INSTALL_FOO") == "BAR"
      assert System.fetch_env!("MIX_INSTALL_BAZ") == "BAT"
    after
      System.delete_env("MIX_INSTALL_FOO")
      System.delete_env("MIX_INSTALL_BAZ")
      Application.delete_env(:unknown_app, :foo, persistent: true)
    end

    test ":config_path with application name", %{tmp_dir: tmp_dir} do
      config_path = Path.join(tmp_dir, "install_test/config/config.exs")
      config_path |> Path.dirname() |> File.mkdir_p!()

      File.write!(config_path, """
      import Config
      config :myapp, :foo, 1
      """)

      Mix.install(
        [
          {:install_test, path: Path.join(tmp_dir, "install_test")}
        ],
        config_path: :install_test
      )

      assert Application.fetch_env!(:myapp, :foo) == 1
    after
      Application.delete_env(:myapp, :foo)
    end

    test ":config_path", %{tmp_dir: tmp_dir} do
      config_path = Path.join(tmp_dir, "config.exs")

      File.write!(config_path, """
      import Config
      config :myapp, :foo, 1
      """)

      Mix.install(
        [
          {:install_test, path: Path.join(tmp_dir, "install_test")}
        ],
        config_path: config_path
      )

      assert Application.fetch_env!(:myapp, :foo) == 1
    after
      Application.delete_env(:myapp, :foo)
    end

    test ":config_path and runtime config", %{tmp_dir: tmp_dir} do
      config_path = Path.join(tmp_dir, "config.exs")

      File.write!(config_path, """
      import Config
      config :myapp, :foo, 1
      """)

      File.write!(Path.join(tmp_dir, "runtime.exs"), """
      import Config
      config :myapp, :bar, 2
      """)

      Mix.install(
        [
          {:install_test, path: Path.join(tmp_dir, "install_test")}
        ],
        config_path: config_path
      )

      assert Application.fetch_env!(:myapp, :foo) == 1
      assert Application.fetch_env!(:myapp, :bar) == 2
    after
      Application.delete_env(:myapp, :foo)
      Application.delete_env(:myapp, :bar)
    end

    test ":config_path that does not exist" do
      assert_raise File.Error, ~r/bad.exs": no such file or directory/, fn ->
        Mix.install([], config_path: "bad.exs")
      end
    end

    defmodule GitApp do
      def project do
        [
          app: :git_app,
          version: "0.1.0",
          deps: [
            {:git_repo, "0.1.0", [git: fixture_path("git_repo")]}
          ]
        ]
      end
    end

    test ":lockfile with first build", %{tmp_dir: tmp_dir} do
      Mix.Project.push(GitApp)
      [_latest_rev, rev | _] = get_git_repo_revs("git_repo")
      lockfile = Path.join(tmp_dir, "lock")
      Mix.Dep.Lock.write(%{git_repo: {:git, fixture_path("git_repo"), rev, []}}, file: lockfile)
      Mix.ProjectStack.pop()

      Mix.install(
        [
          {:git_repo, git: fixture_path("git_repo")}
        ],
        lockfile: lockfile
      )

      assert_received {:mix_shell, :info, ["* Getting git_repo " <> _]}

      install_project_dir = Mix.install_project_dir()
      assert File.read!(Path.join(install_project_dir, "mix.lock")) =~ rev
    end

    test ":lockfile merging", %{tmp_dir: tmp_dir} do
      [rev1, rev2 | _] = get_git_repo_revs("git_repo")

      Mix.install([
        {:git_repo, git: fixture_path("git_repo")}
      ])

      assert_received {:mix_shell, :info, ["* Getting git_repo " <> _]}

      install_project_dir = Mix.install_project_dir()
      assert File.read!(Path.join(install_project_dir, "mix.lock")) =~ rev1

      Mix.Project.push(GitApp)
      lockfile = Path.join(tmp_dir, "lock")
      Mix.Dep.Lock.write(%{git_repo: {:git, fixture_path("git_repo"), rev2, []}}, file: lockfile)
      Mix.ProjectStack.pop()

      Mix.install(
        [
          {:git_repo, git: fixture_path("git_repo")}
        ],
        lockfile: lockfile
      )

      assert File.read!(Path.join(install_project_dir, "mix.lock")) =~ rev1
    end

    test ":lockfile with application name", %{tmp_dir: tmp_dir} do
      lockfile = Path.join(tmp_dir, "install_test/mix.lock")
      lockfile |> Path.dirname() |> File.mkdir_p!()
      Mix.Project.push(GitApp)
      [_latest_rev, rev | _] = get_git_repo_revs("git_repo")
      Mix.Dep.Lock.write(%{git_repo: {:git, fixture_path("git_repo"), rev, []}}, file: lockfile)
      Mix.ProjectStack.pop()

      Mix.install(
        [
          {:install_test, path: Path.join(tmp_dir, "install_test")},
          {:git_repo, git: fixture_path("git_repo")}
        ],
        lockfile: :install_test
      )

      assert_received {:mix_shell, :info, ["* Getting git_repo " <> _]}
      install_project_dir = Mix.install_project_dir()
      assert File.read!(Path.join(install_project_dir, "mix.lock")) =~ rev
    end

    test ":lockfile that does not exist" do
      assert_raise File.Error, ~r/bad": no such file or directory/, fn ->
        Mix.install([], lockfile: "bad")
      end
    end

    test "using restore dir", %{tmp_dir: tmp_dir} do
      with_cleanup(fn ->
        Mix.install([
          {:git_repo, git: fixture_path("git_repo")}
        ])

        assert_received {:mix_shell, :info, ["* Getting git_repo " <> _]}
        assert_received {:mix_shell, :info, ["==> git_repo"]}
        assert_received {:mix_shell, :info, ["Compiling 1 file (.ex)"]}
        assert_received {:mix_shell, :info, ["Generated git_repo app"]}
        refute_received _

        install_project_dir = Mix.install_project_dir()
        build_lib_path = Path.join([install_project_dir, "_build", "dev", "lib"])
        deps_path = Path.join([install_project_dir, "deps"])

        assert File.ls!(build_lib_path) |> Enum.sort() == ["git_repo", "mix_install"]
        assert File.ls!(deps_path) == ["git_repo"]

        System.put_env("MIX_INSTALL_RESTORE_PROJECT_DIR", install_project_dir)
      end)

      # Adding a dependency

      with_cleanup(fn ->
        Mix.install([
          {:git_repo, git: fixture_path("git_repo")},
          {:install_test, path: Path.join(tmp_dir, "install_test")}
        ])

        assert_received {:mix_shell, :info, ["==> install_test"]}
        assert_received {:mix_shell, :info, ["Compiling 2 files (.ex)"]}
        assert_received {:mix_shell, :info, ["Generated install_test app"]}
        refute_received _

        install_project_dir = Mix.install_project_dir()
        build_lib_path = Path.join([install_project_dir, "_build", "dev", "lib"])
        deps_path = Path.join([install_project_dir, "deps"])

        assert File.ls!(build_lib_path) |> Enum.sort() ==
                 ["git_repo", "install_test", "mix_install"]

        assert File.ls!(deps_path) == ["git_repo"]

        System.put_env("MIX_INSTALL_RESTORE_PROJECT_DIR", install_project_dir)
      end)

      # Removing a dependency

      with_cleanup(fn ->
        Mix.install([
          {:install_test, path: Path.join(tmp_dir, "install_test")}
        ])

        refute_received _

        install_project_dir = Mix.install_project_dir()
        build_lib_path = Path.join([install_project_dir, "_build", "dev", "lib"])
        deps_path = Path.join([install_project_dir, "deps"])

        assert File.ls!(build_lib_path) |> Enum.sort() == ["install_test", "mix_install"]
        assert File.ls!(deps_path) == []
      end)
    after
      System.delete_env("MIX_INSTALL_RESTORE_PROJECT_DIR")
    end

    test "using restore dir removes consolidated files when not needed", %{tmp_dir: tmp_dir} do
      with_cleanup(fn ->
        Mix.install([
          {:install_test, path: Path.join(tmp_dir, "install_test")}
        ])

        install_project_dir = Mix.install_project_dir()
        build_lib_path = Path.join([install_project_dir, "_build", "dev", "lib"])
        build_project_path = Path.join(build_lib_path, "mix_install")

        assert File.ls!(build_project_path) |> Enum.sort() == [".mix", "consolidated", "ebin"]

        System.put_env("MIX_INSTALL_RESTORE_PROJECT_DIR", install_project_dir)
      end)

      with_cleanup(fn ->
        Mix.install(
          [
            {:install_test, path: Path.join(tmp_dir, "install_test")}
          ],
          consolidate_protocols: false
        )

        install_project_dir = Mix.install_project_dir()
        build_lib_path = Path.join([install_project_dir, "_build", "dev", "lib"])
        build_project_path = Path.join(build_lib_path, "mix_install")

        assert File.ls!(build_project_path) |> Enum.sort() == [".mix", "ebin"]
      end)
    after
      System.delete_env("MIX_INSTALL_RESTORE_PROJECT_DIR")
    end

    test "installed?", %{tmp_dir: tmp_dir} do
      refute Mix.installed?()

      Mix.install([
        {:install_test, path: Path.join(tmp_dir, "install_test")}
      ])

      assert Mix.installed?()
    end

    test "in_install_project", %{tmp_dir: tmp_dir} do
      Mix.install([
        {:install_test, path: Path.join(tmp_dir, "install_test")}
      ])

      Mix.in_install_project(fn ->
        config = Mix.Project.config()
        assert [{:install_test, [path: _]}] = config[:deps]
      end)
    end

    test "in_install_project recompile", %{tmp_dir: tmp_dir} do
      Mix.install([
        {:install_test, path: Path.join(tmp_dir, "install_test")}
      ])

      File.write!("#{tmp_dir}/install_test/lib/install_test.ex", """
      defmodule InstallTest do
        def hello do
          :universe
        end
      end
      """)

      Mix.in_install_project(fn ->
        Mix.Task.run("compile")
        assert apply(InstallTest, :hello, []) == :universe
      end)
    end

    defp test_project(%{tmp_dir: tmp_dir}) do
      path = :code.get_path()

      on_exit(fn ->
        :code.set_path(path)
        cleanup_deps()
      end)

      Mix.State.put(:installed, nil)

      File.mkdir_p!("#{tmp_dir}/install_test/lib")

      File.write!("#{tmp_dir}/install_test/mix.exs", """
      defmodule InstallTest.MixProject do
        use Mix.Project

        def project do
          [
            app: :install_test,
            version: "0.1.0"
          ]
        end
      end
      """)

      File.write!("#{tmp_dir}/install_test/lib/install_test.ex", """
      defmodule InstallTest do
        def hello do
          :world
        end
      end
      """)

      File.write!("#{tmp_dir}/install_test/lib/install_test_protocol.ex", """
      defprotocol InstallTest.Protocol do
        def foo(x)
      end
      """)

      [tmp_dir: tmp_dir]
    end

    defp with_cleanup(fun) do
      path = :code.get_path()

      try do
        fun.()
      after
        :code.set_path(path)
        cleanup_deps()

        Mix.State.clear_cache()
        Mix.State.put(:installed, nil)
      end
    end

    defp cleanup_deps() do
      purge([
        GitRepo,
        GitRepo.MixProject,
        InstallTest,
        InstallTest.MixProject,
        InstallTest.Protocol
      ])

      ExUnit.CaptureLog.capture_log(fn ->
        Application.stop(:git_repo)
        Application.unload(:git_repo)

        Application.stop(:install_test)
        Application.unload(:install_test)
      end)
    end
  end
end
