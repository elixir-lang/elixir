Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.DepsTest do
  use MixTest.Case

  defmodule DepsApp do
    def project do
      [
        deps: [
          { :ok,         "0.1.0", github: "elixir-lang/ok" },
          { :invalidvsn, "0.2.0", path: "deps/invalidvsn" },
          { :invalidapp, "0.1.0", path: "deps/invalidapp" },
          { :noappfile,  "0.1.0", path: "deps/noappfile" },
          { :uncloned,            git: "https://github.com/elixir-lang/uncloned.git" }
        ]
      ]
    end
  end

  defmodule MixVersionApp do
    def project do
      [ deps: [ { :ok, "~> 0.1", github: "elixir-lang/ok" } ] ]
    end
  end

  test "extracts all dependencies from the given project" do
    Mix.Project.push DepsApp

    in_fixture "deps_status", fn ->
      deps = Mix.Deps.all
      assert Enum.find deps, &match?(Mix.Dep[app: :ok, status: { :ok, _ }], &1)
      assert Enum.find deps, &match?(Mix.Dep[app: :invalidvsn, status: { :invalidvsn, :ok }], &1)
      assert Enum.find deps, &match?(Mix.Dep[app: :invalidapp, status: { :invalidapp, _ }], &1)
      assert Enum.find deps, &match?(Mix.Dep[app: :noappfile, status: { :noappfile, _ }], &1)
      assert Enum.find deps, &match?(Mix.Dep[app: :uncloned, status: { :unavailable, _ }], &1)
    end
  after
    Mix.Project.pop
  end

  test "use mix version for dependencies" do
    Mix.Project.push MixVersionApp

    in_fixture "deps_status", fn ->
      deps = Mix.Deps.all
      assert Enum.find deps, &match?(Mix.Dep[app: :ok, status: { :ok, _ }], &1)
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

  test "correctly order overriden deps" do
    Mix.Project.push ConvergedDepsApp

    in_fixture "deps_status", fn ->
      assert [:git_repo, :deps_repo] == Enum.map(Mix.Deps.all, &(&1.app))
    end
  after
    Mix.Project.pop
  end
end
