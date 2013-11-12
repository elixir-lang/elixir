Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.DepsTest do
  use MixTest.Case

  defmodule DepsApp do
    def project do
      [ app: :deps, version: "0.1.0",
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
      [ app: :sample, version: "0.1.0",
        deps: [
          { :ok, "0.1.0", path: "deps/ok" }
        ]
      ]
    end
  end

  defmodule PerEnvironemtDepsApp do
    def project do
      [ app: :sample, version: "0.1.0", build_per_environment: true,
        deps: [
          { :ok, "0.1.0", path: "deps/ok" }
        ]
      ]
    end
  end

  defmodule ReqDepsApp do
    def project do
      [ app: :req_deps, version: "0.1.0",
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

      assert_received { :mix_shell, :info, ["* ok (git://github.com/elixir-lang/ok.git)"] }
      assert_received { :mix_shell, :info, ["  the dependency is not locked"] }
      assert_received { :mix_shell, :info, ["* invalidvsn (deps/invalidvsn)"] }
      assert_received { :mix_shell, :info, ["  the app file contains an invalid version: :ok"] }
      assert_received { :mix_shell, :info, ["* invalidapp (deps/invalidapp)"] }
      assert_received { :mix_shell, :info, ["  the app file at _build/shared/lib/invalidapp/ebin/invalidapp.app is invalid"] }
      assert_received { :mix_shell, :info, ["* noappfile (deps/noappfile)"] }
      assert_received { :mix_shell, :info, ["  could not find an app file at _build/shared/lib/noappfile/ebin/noappfile.app" <> _] }
      assert_received { :mix_shell, :info, ["* uncloned (https://github.com/elixir-lang/uncloned.git)"] }
      assert_received { :mix_shell, :info, ["  the dependency is not available, run `mix deps.get`"] }
    end
  after
    Mix.Project.pop
  end

  test "prints list of dependencies and their status, including req mismatches and custom apps" do
    Mix.Project.push ReqDepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.run []

      assert_received { :mix_shell, :info, ["* ok (deps/ok)"] }
      assert_received { :mix_shell, :info, ["  the dependency does not match the requirement \">= 2.0\", got \"0.1.0\""] }
      assert_received { :mix_shell, :info, ["* noappfile (deps/noappfile)"] }
      assert_received { :mix_shell, :info, ["* apppath (deps/noappfile)"] }
      refute_received { :mix_shell, :info, ["  could not find app file at _build/shared/lib/noappfile/ebin/apppath.app" <> _] }
      refute_received { :mix_shell, :info, ["  could not find app file at _build/shared/lib/noappfile/ebin/noappfile.app" <> _] }
    end
  after
    Mix.Project.pop
  end

  test "prints list of dependencies including elixir req mismatches" do
    Mix.Project.push ReqDepsApp

    in_fixture "deps_status", fn ->
      File.write!("deps/ok/mix.exs", """)
      defmodule Deps.OkApp do
        use Mix.Project

        def project do
          [elixir: "~> 0.1.0", app: :ok, version: "2.0"]
        end
      end
      """

      Mix.Tasks.Deps.run []
      assert_received { :mix_shell, :info, ["* ok (deps/ok)"] }
      message = "  the dependency requires Elixir ~> 0.1.0 but you are running on v#{System.version}"
      assert_received { :mix_shell, :info, [^message] }
    end
  after
    Mix.Project.pop
  end

  test "prints list of dependencies and their lock status" do
    Mix.Project.push DepsApp

    in_fixture "deps_status", fn ->
      Mix.Deps.Lock.write [ok: { :git, "git://github.com/elixir-lang/ok.git", "abcdefghi", [] }]
      Mix.Tasks.Deps.run []

      assert_received { :mix_shell, :info, ["* ok (git://github.com/elixir-lang/ok.git)"] }
      assert_received { :mix_shell, :info, ["  locked at abcdefg"] }
      assert_received { :mix_shell, :info, ["  lock mismatch: the dependency is out of date"] }

      Mix.Deps.Lock.write [ok: { :git, "git://github.com/elixir-lang/another.git", "abcdefghi", [] }]
      Mix.Tasks.Deps.run []

      assert_received { :mix_shell, :info, ["* ok (git://github.com/elixir-lang/ok.git)"] }
      assert_received { :mix_shell, :info, ["  lock outdated: the lock is outdated compared to the options in your mixfile"] }
    end
  after
    Mix.Project.pop
  end

  test "checks list of dependencies and their status with success" do
    Mix.Project.push SuccessfulDepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Check.run []
    end
  after
    Mix.Project.pop
  end

  test "checks list of dependencies and their status on failure" do
    Mix.Project.push DepsApp

    in_fixture "deps_status", fn ->
      assert_raise Mix.Error, fn ->
        Mix.Tasks.Deps.Check.run []
      end

      assert_received { :mix_shell, :error, ["* ok (git://github.com/elixir-lang/ok.git)"] }
      assert_received { :mix_shell, :error, ["  the dependency is not locked"] }
      assert_received { :mix_shell, :error, ["* invalidvsn (deps/invalidvsn)"] }
      assert_received { :mix_shell, :error, ["  the app file contains an invalid version: :ok"] }
      assert_received { :mix_shell, :error, ["* invalidapp (deps/invalidapp)"] }
      assert_received { :mix_shell, :error, ["  the app file at _build/shared/lib/invalidapp/ebin/invalidapp.app is invalid"] }
      assert_received { :mix_shell, :error, ["* noappfile (deps/noappfile)"] }
      assert_received { :mix_shell, :error, ["  could not find an app file at _build/shared/lib/noappfile/ebin/noappfile.app" <> _] }
      assert_received { :mix_shell, :error, ["* uncloned (https://github.com/elixir-lang/uncloned.git)"] }
      assert_received { :mix_shell, :error, ["  the dependency is not available, run `mix deps.get`"] }
    end
  after
    Mix.Project.pop
  end

  test "compiles and prunes builds per environment" do
    Mix.Project.push PerEnvironemtDepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Compile.run []
      Mix.Tasks.Deps.Check.run []
      assert File.exists?("_build/dev/lib/ok/ebin/ok.app")
      assert File.exists?("_build/dev/lib/ok/priv/sample")

      Mix.Tasks.Compile.run []
      assert File.exists?("_build/dev/lib/sample/ebin/sample.app")

      Mix.ProjectStack.post_config [deps: []]
      Mix.Project.pop
      Mix.Project.push PerEnvironemtDepsApp

      Mix.Tasks.Deps.Check.run []
      refute File.exists?("_build/dev/lib/ok/ebin/ok.app")
      assert File.exists?("_build/dev/lib/sample/ebin/sample.app")
    end
  after
    Mix.Project.pop
  end

  test "copies dev dependency when building per environment" do
    Mix.Project.push PerEnvironemtDepsApp

    in_fixture "deps_status", fn ->
      File.mkdir_p!("deps/ok/lib")
      File.write!("deps/ok/lib/empty.ex", "")

      Mix.Tasks.Deps.Compile.run []
      assert File.exists?("_build/dev/lib/ok/ebin/ok.app")
      assert_received { :mix_shell, :info, ["Generated ok.app"] }

      Mix.Task.clear
      Mix.env(:test)
      Mix.Project.pop
      Mix.Project.push PerEnvironemtDepsApp

      Mix.Tasks.Deps.Compile.run []
      assert File.exists?("_build/test/lib/ok/ebin/ok.app")
      assert File.read!("_build/test/lib/ok/priv/sample") == "SAMPLE"
      # We don't get a message becaused we copied the contents over
      refute_received { :mix_shell, :info, ["Generated ok.app"] }
    end
  after
    Mix.env(:dev)
    Mix.Project.pop
  end

  test "does not choke when another environment is compiled first than dep" do
    Mix.Project.push PerEnvironemtDepsApp
    Mix.env(:test)

    in_fixture "deps_status", fn ->
      File.mkdir_p!("deps/ok/lib")
      File.write!("deps/ok/lib/empty.ex", "")

      Mix.Tasks.Deps.Compile.run []
      assert File.exists?("_build/test/lib/ok/ebin/ok.app")
      assert File.read!("_build/test/lib/ok/priv/sample") == "SAMPLE"
      assert_received { :mix_shell, :info, ["Generated ok.app"] }
    end
  after
    Mix.env(:dev)
    Mix.Project.pop
  end

  test "does not copy if environments do not match" do
    Mix.ProjectStack.post_config(deps: [{ :ok, "0.1.0", path: "deps/ok", env: Mix.env }])
    Mix.Project.push PerEnvironemtDepsApp

    in_fixture "deps_status", fn ->
      File.mkdir_p!("deps/ok/lib")
      File.write!("deps/ok/lib/empty.ex", "")

      Mix.Tasks.Deps.Compile.run []
      assert File.exists?("_build/dev/lib/ok/ebin/ok.app")
      assert_received { :mix_shell, :info, ["Generated ok.app"] }

      Mix.Task.clear
      Mix.env(:test)
      Mix.Project.pop
      Mix.Project.push PerEnvironemtDepsApp

      Mix.Tasks.Deps.Compile.run []
      assert_received { :mix_shell, :info, ["Generated ok.app"] }
    end
  after
    Mix.env(:dev)
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

  test "sets deps env to prod by default" do
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
          { :git_repo, ">= 0.1", git: MixTest.Case.fixture_path("git_repo") }
        ]
      ]
    end
  end

  defmodule OverridenDepsApp do
    def project do
      [
        app: :raw_sample,
        version: "0.1.0",
        deps: [
          { :bad_deps_repo, "0.1.0", path: "custom/bad_deps_repo" },
          { :git_repo, "0.1.0", git: MixTest.Case.fixture_path("git_repo"), override: true }
        ]
      ]
    end
  end

  defmodule NonOverridenDepsApp do
    def project do
      [
        app: :raw_sample,
        version: "0.1.0",
        deps: [
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
      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received { :mix_shell, :info, [^message] }
      assert_received { :mix_shell, :info, ["* Compiling deps_repo"] }
      assert_received { :mix_shell, :info, ["Generated git_repo.app"] }

      Mix.Task.clear

      Mix.Tasks.Deps.Update.run ["--all"]
      assert_received { :mix_shell, :info, ["* Updating deps_repo (custom/deps_repo)"] }
      assert_received { :mix_shell, :info, ["* Compiling deps_repo"] }
    end
  after
    Mix.Project.pop
  end

  test "respects --quiet in deps.compile" do
    Mix.Project.push NestedDepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Get.run ["--quiet"]
      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received { :mix_shell, :info, [^message] }
      refute_received { :mix_shell, :info, ["* Compiling deps_repo"] }
      assert_received { :mix_shell, :info, ["Generated git_repo.app"] }
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

      receive do
        { :mix_shell, :error, ["  different specs were given for the git_repo app:" <> _ = msg] } ->
          assert msg =~ "In custom/deps_repo/mix.exs:"
          assert msg =~ "{:git_repo, \"0.1.0\", [git: #{inspect fixture_path("git_repo")}]}"
      after
        0 -> flunk "expected diverged error message"
      end
    end
  after
    Mix.Project.pop
  end

  test "fails on diverged dependencies by requirement" do
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
              { :git_repo, "0.2.0", git: MixTest.Case.fixture_path("git_repo") }
            ]
          ]
        end
      end
      """

      assert_raise Mix.Error, fn ->
        Mix.Tasks.Deps.Get.run []
      end

      receive do
        { :mix_shell, :error, ["  the dependency git_repo defined" <> _ = msg] } ->
          assert msg =~ "In custom/deps_repo/mix.exs:"
          assert msg =~ "{:git_repo, \"0.2.0\", [git: #{inspect fixture_path("git_repo")}]}"
      after
        0 -> flunk "expected diverged req error message"
      end
    end
  after
    Mix.Project.pop
  end

  test "fails on diverged dependencies even when optional" do
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
              { :git_repo, git: MixTest.Case.fixture_path("bad_git_repo"), branch: "omg" }
            ]
          ]
        end
      end
      """

      assert_raise Mix.Error, fn ->
        Mix.Tasks.Deps.Get.run []
      end

      assert_received { :mix_shell, :error, ["  the dependency git_repo in mix.exs is overriding" <> _] }
    end
  after
    Mix.Project.pop
  end

  test "works with converged dependencies" do
    Mix.Project.push ConvergedDepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Get.run []
      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received { :mix_shell, :info, [^message] }
      assert_received { :mix_shell, :info, ["Generated git_repo.app"] }

      # Make sure retriever uses converger,
      # so the message appears just once
      refute_received { :mix_shell, :info, [^message] }

      Mix.Task.clear
      Mix.Tasks.Deps.Update.run ["--all"]

      message = "* Updating deps_repo (custom/deps_repo)"
      assert_received { :mix_shell, :info, [^message] }
      message = "* Updating git_repo (#{fixture_path("git_repo")})"
      assert_received { :mix_shell, :info, [^message] }

      Mix.Tasks.Deps.Check.run []
    end
  after
    purge [GitRepo, GitRepo.Mix]
    Mix.Project.pop
  end

  test "works with overriden dependencies" do
    Mix.Project.push OverridenDepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Get.run []
      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received { :mix_shell, :info, [^message] }
      assert_received { :mix_shell, :info, ["Generated git_repo.app"] }

      # Make sure retriever uses converger,
      # so the message appears just once
      refute_received { :mix_shell, :info, [^message] }

      Mix.Task.clear
      Mix.Tasks.Deps.Update.run ["--all"]

      message = "* Updating bad_deps_repo (custom/bad_deps_repo)"
      assert_received { :mix_shell, :info, [^message] }
      message = "* Updating git_repo (#{fixture_path("git_repo")})"
      assert_received { :mix_shell, :info, [^message] }
    end
  after
    purge [GitRepo, GitRepo.Mix]
    Mix.Project.pop
  end

  test "converged dependencies errors if not overriding" do
    Mix.Project.push NonOverridenDepsApp

    in_fixture "deps_status", fn ->
      assert_raise Mix.Error, fn ->
        Mix.Tasks.Deps.Check.run []
      end

      receive do
        { :mix_shell, :error, ["  the dependency git_repo in mix.exs" <> _ = msg] } ->
          assert msg =~ "In mix.exs:"
          assert msg =~ "{:git_repo, \"0.1.0\", [git: #{inspect fixture_path("git_repo")}]}"
      after
        0 -> flunk "expected overriding error message"
      end
    end
  after
    purge [GitRepo, GitRepo.Mix]
    Mix.Project.pop
  end

  test "updates parent dependencies" do
    Mix.Project.push NestedDepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Get.run []
      Mix.Task.clear
      Mix.Tasks.Deps.Update.run ["git_repo"]

      message = "* Updating git_repo (#{fixture_path("git_repo")})"
      assert_received { :mix_shell, :info, [^message] }
      assert_received { :mix_shell, :info, ["* Compiling deps_repo"] }
    end
  after
    Mix.Project.pop
  end

  test "checks if dependencies are using old elixir version" do
    Mix.Project.push SuccessfulDepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Compile.run []
      Mix.Tasks.Deps.Check.run []

      File.mkdir_p!("_build/shared/lib/ok/ebin")
      File.write!("_build/shared/lib/ok/.compile.lock", "the_future")
      Mix.Task.clear

      assert_raise Mix.Error, "Can't continue due to errors on dependencies", fn ->
        Mix.Tasks.Deps.Check.run []
      end

      assert_received { :mix_shell, :error, ["* ok (deps/ok)"] }
      assert_received { :mix_shell, :error, ["  the dependency is built with an out-of-date elixir version, run `mix deps.get`"] }

      Mix.Tasks.Deps.Get.run []
      assert_received { :mix_shell, :info, ["* Compiling ok"] }
    end
  after
    Mix.Project.pop
  end

  defmodule NonCompilingDeps do
    def project do
      [
        app: :raw_sample,
        version: "0.1.0",
        deps: [
          { :deps_repo, "0.1.0", path: "custom/deps_repo", compile: false },
          { :git_repo, "0.1.0", git: MixTest.Case.fixture_path("git_repo"), compile: false }
        ]
      ]
    end
  end

  test "does not compile deps that have explicit flag" do
    Mix.Project.push NonCompilingDeps

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Compile.run []
      refute_received { :mix_shell, :info, ["* Compiling deps_repo"] }
      refute_received { :mix_shell, :info, ["* Compiling git_repo"] }
    end
  after
    Mix.Project.pop
  end

  defmodule DupDeps do
    def project do
      [
        app: :raw_sample,
        version: "0.1.0",
        deps: [
          # Simulate dependencies gathered together from umbrella
          { :ok, "0.1.0", path: "deps/ok" },
          { :ok, "0.1.0", path: "deps/ok" }
        ]
      ]
    end
  end

  test "converges duplicated deps at the same level" do
    Mix.Project.push DupDeps

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.run []

      msg = "* ok 0.1.0 (deps/ok)"
      assert_received { :mix_shell, :info, [^msg] }
      refute_received { :mix_shell, :info, [^msg] }
    end
  after
    Mix.Project.pop
  end
end
