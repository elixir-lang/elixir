Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.DepsTest do
  use MixTest.Case

  defmodule DepsApp do
    def project do
      [
        deps: [
          { :ok, "0.1.0",         github: "elixir-lang/ok" },
          { :invalidvsn, "0.2.0", path: "deps/invalidvsn" },
          { :invalidapp, "0.1.0", path: "deps/invalidapp" },
          { :noappfile, "0.1.0",  path: "deps/noappfile" },
          { :uncloned,            git: "https://github.com/elixir-lang/uncloned.git" }
        ]
      ]
    end
  end

  defmodule SuccessfulDepsApp do
    def project do
      [
        deps: [
          { :ok, "0.1.0", path: "deps/ok" }
        ]
      ]
    end
  end

  defmodule OutOfDateDepsApp do
    def project do
      [
        deps: [
          { :ok,       "0.1.0", git: "https://github.com/elixir-lang/ok.git" },
          { :uncloned, "0.1.0", git: "https://github.com/elixir-lang/uncloned.git" }
        ]
      ]
    end
  end

  defmodule ReqDepsApp do
    def project do
      [
        deps: [
          { :ok, ">= 2.0",  path: "deps/ok" },
          { :noappfile,     path: "deps/noappfile", app: false },
          { :apppath,       path: "deps/noappfile", app: "../deps/ok/ebin/ok.app" }
        ]
      ]
    end
  end

  test "prints list of dependencies and their status" do
    Mix.Project.push DepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.run []

      assert_received { :mix_shell, :info, ["* ok [git: \"git://github.com/elixir-lang/ok.git\"]"] }
      assert_received { :mix_shell, :info, ["  locked at \"abcdef\""] }
      assert_received { :mix_shell, :info, ["  lock mismatch: the dependency is out of date"] }
      assert_received { :mix_shell, :info, ["* invalidvsn [path: \"deps/invalidvsn\"]"] }
      assert_received { :mix_shell, :info, ["  the app file contains an invalid version: :ok"] }
      assert_received { :mix_shell, :info, ["* invalidapp [path: \"deps/invalidapp\"]"] }
      assert_received { :mix_shell, :info, ["  the app file at deps/invalidapp/custom_ebin/invalidapp.app is invalid"] }
      assert_received { :mix_shell, :info, ["* noappfile [path: \"deps/noappfile\"]"] }
      assert_received { :mix_shell, :info, ["  could not find an app file at deps/noappfile/ebin/noappfile.app"] }
      assert_received { :mix_shell, :info, ["* uncloned [git: \"https://github.com/elixir-lang/uncloned.git\"]"] }
      assert_received { :mix_shell, :info, ["  the dependency is not available, run `mix deps.get`"] }
    end
  after
    Mix.Project.pop
  end

  test "prints list of dependencies and their status, including req mismatches and custom apps" do
    Mix.Project.push ReqDepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.run []

      assert_received { :mix_shell, :info, ["* ok [path: \"deps/ok\"]"] }
      assert_received { :mix_shell, :info, ["  the dependency does not match the requirement >= 2.0, got 0.1.0"] }
      assert_received { :mix_shell, :info, ["* noappfile [path: \"deps/noappfile\"]"] }
      assert_received { :mix_shell, :info, ["* apppath [path: \"deps/noappfile\"]"] }
      refute_received { :mix_shell, :info, ["  could not find app file at deps/noappfile/ebin/apppath.app"] }
      refute_received { :mix_shell, :info, ["  could not find app file at deps/noappfile/ebin/noappfile.app"] }
    end
  after
    Mix.Project.pop
  end

  test "check list of dependencies and their status with success" do
    Mix.Project.push SuccessfulDepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Check.run []
    end
  after
    Mix.Project.pop
  end

  test "check list of dependencies and their status with failure" do
    Mix.Project.push OutOfDateDepsApp

    in_fixture "deps_status", fn ->
      assert_raise Mix.Error, fn ->
        Mix.Tasks.Deps.Check.run []
      end

      assert_received { :mix_shell, :error, ["* uncloned [git: \"https://github.com/elixir-lang/uncloned.git\"]"] }
    end
  after
    Mix.Project.pop
  end

  test "check list of dependencies and their status on failure" do
    Mix.Project.push DepsApp

    in_fixture "deps_status", fn ->
      assert_raise Mix.Error, fn ->
        Mix.Tasks.Deps.Check.run []
      end

      assert_received { :mix_shell, :error, ["* ok [git: \"git://github.com/elixir-lang/ok.git\"]"] }
      assert_received { :mix_shell, :error, ["  lock mismatch: the dependency is out of date"] }
      assert_received { :mix_shell, :error, ["* invalidvsn [path: \"deps/invalidvsn\"]"] }
      assert_received { :mix_shell, :error, ["  the app file contains an invalid version: :ok"] }
      assert_received { :mix_shell, :error, ["* invalidapp [path: \"deps/invalidapp\"]"] }
      assert_received { :mix_shell, :error, ["  the app file at deps/invalidapp/custom_ebin/invalidapp.app is invalid"] }
      assert_received { :mix_shell, :error, ["* noappfile [path: \"deps/noappfile\"]"] }
      assert_received { :mix_shell, :error, ["  could not find an app file at deps/noappfile/ebin/noappfile.app"] }
      assert_received { :mix_shell, :error, ["* uncloned [git: \"https://github.com/elixir-lang/uncloned.git\"]"] }
      assert_received { :mix_shell, :error, ["  the dependency is not available, run `mix deps.get`"] }
    end
  after
    Mix.Project.pop
  end

  test "unlocks all deps" do
    Mix.Project.push DepsApp
    in_fixture "no_mixfile", fn ->
      Mix.Deps.Lock.write [git_repo: "abcdef"]
      assert Mix.Deps.Lock.read == [git_repo: "abcdef"]
      Mix.Tasks.Deps.Unlock.run ["--all"]
      assert Mix.Deps.Lock.read == []
    end
  after
    Mix.Project.pop
  end

  test "unlocks specific deps" do
    Mix.Project.push DepsApp
    in_fixture "no_mixfile", fn ->
      Mix.Deps.Lock.write [git_repo: "abcdef", another: "hash"]
      Mix.Tasks.Deps.Unlock.run ["git_repo", "unknown"]
      assert Mix.Deps.Lock.read == [another: "hash"]
    end
  after
    Mix.Project.pop
  end

  ## Deps environment

  defmodule DepsEnvApp do
    def project do
      [
        app: :raw_sample,
        version: "0.1.0",
        deps: [
          { :raw_repo, "0.1.0", path: "custom/raw_repo" }
        ]
      ]
    end
  end

  defmodule CustomDepsEnvApp do
    def project do
      [
        app: :raw_sample,
        version: "0.1.0",
        deps: [
          { :raw_repo, "0.1.0", path: "custom/raw_repo", env: :dev }
        ]
      ]
    end
  end

  test "by default sets deps env to prod" do
    Mix.Project.push DepsEnvApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Update.run ["--all"]
      assert_received { :mix_shell, :info, [":raw_repo env is prod"] }
    end
  after
    Mix.Project.pop
  end

  test "can customize environment" do
    Mix.Project.push CustomDepsEnvApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Update.run ["--all"]
      assert_received { :mix_shell, :info, [":raw_repo env is dev"] }
    end
  after
    Mix.Project.pop
  end

  ## Nested dependencies

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

  defmodule DivergedDepsApp do
    def project do
      [
        app: :raw_sample,
        version: "0.1.0",
        deps: [
          { :deps_repo, "0.1.0", path: "custom/deps_repo" },
          { :bad_deps_repo, "0.1.0", path: "custom/bad_deps_repo" }
        ]
      ]
    end
  end

  defmodule ConvergedDepsApp do
    def project do
      [
        app: :raw_sample,
        version: "0.1.0",
        deps: [
          { :deps_repo, "0.1.0", path: "custom/deps_repo" },
          { :bad_deps_repo, "0.1.0", path: "custom/bad_deps_repo" },
          { :git_repo, "0.1.0", git: MixTest.Case.fixture_path("git_repo"), override: true }
        ]
      ]
    end
  end

  defmodule UnConvergedDepsApp do
    def project do
      [
        app: :raw_sample,
        version: "0.1.0",
        deps: [
          { :deps_repo, "0.1.0", path: "custom/deps_repo" },
          { :bad_deps_repo, "0.1.0", path: "custom/bad_deps_repo" },
          { :git_repo, "0.1.0", git: MixTest.Case.fixture_path("git_repo") }
        ]
      ]
    end
  end

  test "works with nested dependencies" do
    Mix.Project.push NestedDepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Get.run []
      message = "* Getting git_repo [git: #{inspect fixture_path("git_repo")}]"
      assert_received { :mix_shell, :info, [^message] }
      assert_received { :mix_shell, :info, ["* Compiling deps_repo"] }
      assert_received { :mix_shell, :info, ["Generated git_repo.app"] }

      Mix.Tasks.Deps.Update.run ["--all"]
      assert_received { :mix_shell, :info, ["* Updating deps_repo (0.1.0) [path: \"custom/deps_repo\"]"] }
      assert_received { :mix_shell, :info, ["* Compiling deps_repo"] }
    end
  after
    Mix.Project.pop
  end

  test "fails on diverged dependencies" do
    Mix.Project.push DivergedDepsApp

    in_fixture "deps_status", fn ->
      assert_raise Mix.Error, fn ->
        Mix.Tasks.Deps.Check.run []
      end

      assert_received { :mix_shell, :error, ["  different specs were given for this dependency, choose one in your deps:" <> _] }
    end
  after
    Mix.Project.pop
  end

  test "works with converged dependencies" do
    Mix.Project.push ConvergedDepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Get.run []
      message = "* Getting git_repo [git: #{inspect fixture_path("git_repo")}]"
      assert_received { :mix_shell, :info, [^message] }
      assert_received { :mix_shell, :info, ["Generated git_repo.app"] }

      # Make sure retriever uses converger
      refute_received { :mix_shell, :info, [^message] }

      Mix.Task.clear
      Mix.Tasks.Deps.Update.run ["--all"]

      assert_received { :mix_shell, :info, ["* Updating deps_repo (0.1.0) [path: \"custom/deps_repo\"]"] }
      assert_received { :mix_shell, :info, ["* Updating bad_deps_repo (0.1.0) [path: \"custom/bad_deps_repo\"]"] }

      Mix.Tasks.Deps.Check.run []
    end
  after
    purge [GitRepo, GitRepo.Mix]
    Mix.Project.pop
  end

  test "converged dependencies will error if not overriding" do
    Mix.Project.push UnConvergedDepsApp

    in_fixture "deps_status", fn ->
      assert_raise Mix.Error, fn ->
        Mix.Tasks.Deps.Check.run []
      end
      assert_received { :mix_shell, :error, ["  the dependency is overriding another dependency of one of your dependencies" <> _] }
    end
  after
    purge [GitRepo, GitRepo.Mix]
    Mix.Project.pop
  end

  test "converged dependencies are properly ordered" do
    Mix.Project.push NestedDepsApp

    in_fixture "deps_status", fn ->
      # Nested dependencies need to come first. They are
      # listed first, compiled first, etc.
      assert [Mix.Dep[app: :git_repo], Mix.Dep[app: :deps_repo]] = Mix.Deps.all
    end
  after
    Mix.Project.pop
  end

  test "update parent dependencies" do
    Mix.Project.push NestedDepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Get.run []
      Mix.Task.clear
      Mix.Tasks.Deps.Update.run ["git_repo"]

      message = "* Updating git_repo (0.1.0) [git: \"#{fixture_path("git_repo")}\"]"
      assert_received { :mix_shell, :info, [^message] }
      assert_received { :mix_shell, :info, ["* Updating deps_repo (0.1.0) [path: \"custom/deps_repo\"]"] }
    end
  after
    Mix.Project.pop
  end
end
