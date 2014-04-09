Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.DepTest do
  use MixTest.Case

  defmodule DepsApp do
    def project do
      [ deps: [
          { :ok,         "0.1.0", github: "elixir-lang/ok" },
          { :invalidvsn, "0.2.0", path: "deps/invalidvsn" },
          { :invalidapp, "0.1.0", path: "deps/invalidapp" },
          { :noappfile,  "0.1.0", path: "deps/noappfile" },
          { :uncloned,            git: "https://github.com/elixir-lang/uncloned.git" },
          { :optional,            git: "https://github.com/elixir-lang/optional.git", optional: true }
        ] ]
    end
  end

  defmodule MixVersionApp do
    def project do
      [ deps: [ { :ok, "~> 0.1", github: "elixir-lang/ok" } ] ]
    end
  end

  defmodule NoSCMApp do
    def project do
      [ deps: [ { :ok, "~> 0.1", not_really: :ok } ] ]
    end
  end

  defmodule InvalidDepsReq do
    def project do
      [ deps: [ { :ok, "+- 0.1.0", github: "elixir-lang/ok" } ] ]
    end
  end

  test "extracts all dependencies from the given project" do
    Mix.Project.push DepsApp

    in_fixture "deps_status", fn ->
      deps = Mix.Dep.loaded([])
      assert length(deps) == 6
      assert Enum.find deps, &match?(%Mix.Dep{app: :ok, status: { :ok, _ }}, &1)
      assert Enum.find deps, &match?(%Mix.Dep{app: :invalidvsn, status: { :invalidvsn, :ok }}, &1)
      assert Enum.find deps, &match?(%Mix.Dep{app: :invalidapp, status: { :invalidapp, _ }}, &1)
      assert Enum.find deps, &match?(%Mix.Dep{app: :noappfile, status: { :noappfile, _ }}, &1)
      assert Enum.find deps, &match?(%Mix.Dep{app: :uncloned, status: { :unavailable, _ }}, &1)
      assert Enum.find deps, &match?(%Mix.Dep{app: :optional, status: { :unavailable, _ }}, &1)
    end
  end

  test "use mix version for dependencies" do
    Mix.Project.push MixVersionApp

    in_fixture "deps_status", fn ->
      deps = Mix.Dep.loaded([])
      assert Enum.find deps, &match?(%Mix.Dep{app: :ok, status: { :ok, _ }}, &1)
    end
  end

  test "raises when no SCM is specified" do
    Mix.Project.push NoSCMApp

    in_fixture "deps_status", fn ->
      msg = "Mix.DepTest.NoSCMApp did not specify a supported scm for app :ok, " <>
            "expected one of :git, :path or :in_umbrella"
      assert_raise Mix.Error, msg, fn -> Mix.Dep.loaded([]) end
    end
  end

  test "does not set the manager before the dependency was loaded" do
    # It is important to not eagerly set the manager because the dependency
    # needs to be loaded (i.e. available in the filesystem) in order to get
    # the proper manager.
    Mix.Project.push DepsApp

    { _, true, _ } =
      Mix.Dep.unloaded(false, [], nil, fn dep, acc, lock ->
        assert nil?(dep.manager)
        { dep, acc or true, lock }
      end)
  end

  test "raises on invalid deps req" do
    Mix.Project.push InvalidDepsReq

    in_fixture "deps_status", fn ->
      assert_raise Mix.Error, ~r"Invalid requirement", fn ->
        Mix.Dep.loaded([])
      end
    end
  end

  defmodule NestedDepsApp do
    def project do
      [
        app: :raw_sample,
        version: "0.1.0",
        deps: [
          { :deps_repo, "0.1.0", path: "custom/deps_repo" }
        ]
      ]
    end
  end

  test "nested deps come first" do
    Mix.Project.push NestedDepsApp

    in_fixture "deps_status", fn ->
      assert Enum.map(Mix.Dep.loaded([]), &(&1.app)) == [:git_repo, :deps_repo]
    end
  end

  test "nested optional deps are never added" do
    Mix.Project.push NestedDepsApp

    in_fixture "deps_status", fn ->
      File.write! "custom/deps_repo/mix.exs", """
      defmodule DepsRepo do
        use Mix.Project

        def project do
          [
            app: :deps_repo,
            version: "0.1.0",
            deps: [
              { :git_repo, "0.2.0", git: MixTest.Case.fixture_path("git_repo"), optional: true }
            ]
          ]
        end
      end
      """

      assert Enum.map(Mix.Dep.loaded([]), &(&1.app)) == [:deps_repo]
    end
  end

  defmodule ConvergedDepsApp do
    def project do
      [
        app: :raw_sample,
        version: "0.1.0",
        deps: [
          { :deps_repo, "0.1.0", path: "custom/deps_repo" },
          { :git_repo, "0.1.0", git: MixTest.Case.fixture_path("git_repo") }
        ]
      ]
    end
  end

  test "correctly order converged deps" do
    Mix.Project.push ConvergedDepsApp

    in_fixture "deps_status", fn ->
      assert Enum.map(Mix.Dep.loaded([]), &(&1.app)) == [:git_repo, :deps_repo]
    end
  end

  test "correctly order converged deps even with optional dependencies" do
    Mix.Project.push ConvergedDepsApp

    in_fixture "deps_status", fn ->
      File.write! "custom/deps_repo/mix.exs", """
      defmodule DepsRepo do
        use Mix.Project

        def project do
          [
            app: :deps_repo,
            version: "0.1.0",
            deps: [
              { :git_repo, "0.2.0", git: MixTest.Case.fixture_path("git_repo"), optional: true }
            ]
          ]
        end
      end
      """

      assert Enum.map(Mix.Dep.loaded([]), &(&1.app)) == [:git_repo, :deps_repo]
    end
  end

  defmodule IdentityRemoteConverger do
    @behaviour Mix.RemoteConverger

    def remote?(_app), do: true

    def converge(_deps, lock) do
      Process.put(:remote_converger, true)
      lock
    end
  end

  test "remote converger" do
    Mix.Project.push ConvergedDepsApp
    Mix.RemoteConverger.register(IdentityRemoteConverger)

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Get.run([])

      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received { :mix_shell, :info, [^message] }

      assert Process.get(:remote_converger)
    end
  after
    Mix.RemoteConverger.register(nil)
  end

  defmodule OnlyDeps do
    def project do
      [ deps: [ { :foo, github: "elixir-lang/foo" },
                { :bar, github: "elixir-lang/bar", only: :other_env }  ] ]
    end
  end

  test "only extract deps matching environment" do
    Mix.Project.push OnlyDeps

    in_fixture "deps_status", fn ->
      { deps, _acc, _lock } = Mix.Dep.unloaded([], nil, [env: :other_env], &{ &1, &2, &3 })
      assert length(deps) == 2

      { deps, _acc, _lock } = Mix.Dep.unloaded([], nil, [], &{ &1, &2, &3 })
      assert length(deps) == 2

      { deps, _acc, _lock } = Mix.Dep.unloaded([], nil, [env: :prod], &{ &1, &2, &3 })
      assert length(deps) == 1
      assert Enum.find deps, &match?(%Mix.Dep{app: :foo}, &1)
    end
  end

  defmodule OnlyChildDeps do
    def project do
      [ app: :raw_sample,
        version: "0.1.0",
        deps: [ { :only_deps, path: fixture_path("only_deps") } ] ]
    end
  end

  test "only fetch child deps matching prod env" do
    Mix.Project.push OnlyChildDeps

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Get.run([])
      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      refute_received { :mix_shell, :info, [^message] }
    end
  end

  defmodule OnlyParentDeps do
    def project do
      [ app: :raw_sample,
        version: "0.1.0",
        deps: [ { :only, github: "elixir-lang/only", only: :dev } ] ]
    end
  end

  test "only fetch parent deps matching specified env" do
    Mix.Project.push OnlyParentDeps

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Get.run(["--only", "prod"])
      refute_received { :mix_shell, :info, ["* Getting" <> _] }

      assert_raise Mix.Error, "Can't continue due to errors on dependencies", fn ->
        Mix.Tasks.Deps.Check.run([])
      end

      Mix.env(:prod)
      Mix.Tasks.Deps.Check.run([])
    end
  end
end
