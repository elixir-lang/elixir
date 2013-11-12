Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.DepsTest do
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

  test "extracts all dependencies from the given project" do
    Mix.Project.push DepsApp

    in_fixture "deps_status", fn ->
      deps = Mix.Deps.loaded
      assert length(deps) == 6
      assert Enum.find deps, &match?(Mix.Dep[app: :ok, status: { :ok, _ }], &1)
      assert Enum.find deps, &match?(Mix.Dep[app: :invalidvsn, status: { :invalidvsn, :ok }], &1)
      assert Enum.find deps, &match?(Mix.Dep[app: :invalidapp, status: { :invalidapp, _ }], &1)
      assert Enum.find deps, &match?(Mix.Dep[app: :noappfile, status: { :noappfile, _ }], &1)
      assert Enum.find deps, &match?(Mix.Dep[app: :uncloned, status: { :unavailable, _ }], &1)
      assert Enum.find deps, &match?(Mix.Dep[app: :optional, status: { :unavailable, _ }], &1)
    end
  after
    Mix.Project.pop
  end

  test "use mix version for dependencies" do
    Mix.Project.push MixVersionApp

    in_fixture "deps_status", fn ->
      deps = Mix.Deps.loaded
      assert Enum.find deps, &match?(Mix.Dep[app: :ok, status: { :ok, _ }], &1)
    end
  after
    Mix.Project.pop
  end

  test "raises when no SCM is specified" do
    Mix.Project.push NoSCMApp

    in_fixture "deps_status", fn ->
      msg = "Mix.DepsTest.NoSCMApp did not specify a supported scm for app :ok, " <>
            "expected one of :git, :path or :in_umbrella"
      assert_raise Mix.Error, msg, fn -> Mix.Deps.loaded end
    end
  after
    Mix.Project.pop
  end

  test "does not set the manager before the dependency was loaded" do
    # It is important to not eagerly set the manager because the dependency
    # needs to be loaded (i.e. available in the filesystem) in order to get
    # the proper manager.
    Mix.Project.push DepsApp

    { _, true } =
      Mix.Deps.unloaded(false, fn dep, acc ->
        assert nil?(dep.manager)
        { dep, acc or true }
      end)
  after
    Mix.Project.pop
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
      assert Enum.map(Mix.Deps.loaded, &(&1.app)) == [:git_repo, :deps_repo]
    end
  after
    Mix.Project.pop
  end

  test "nested optional deps are never added" do
    Mix.Project.push NestedDepsApp

    in_fixture "deps_status", fn ->
      File.write!("custom/deps_repo/mix.exs", """)
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

      assert Enum.map(Mix.Deps.loaded, &(&1.app)) == [:deps_repo]
    end
  after
    Mix.Project.pop
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
      assert Enum.map(Mix.Deps.loaded, &(&1.app)) == [:git_repo, :deps_repo]
    end
  after
    Mix.Project.pop
  end

  test "correctly order converged deps even with optional dependencies" do
    Mix.Project.push ConvergedDepsApp

    in_fixture "deps_status", fn ->
      File.write!("custom/deps_repo/mix.exs", """)
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

      assert Enum.map(Mix.Deps.loaded, &(&1.app)) == [:git_repo, :deps_repo]
    end
  after
    Mix.Project.pop
  end
end
