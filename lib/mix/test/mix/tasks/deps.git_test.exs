Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.DepsGitTest do
  use MixTest.Case

  defmodule DepsOnGitApp do
    def project do
      [ app: :deps_on_git_app,
        version: "0.1.0",
        deps: [
          { :deps_on_git_repo, "0.2.0", git: MixTest.Case.fixture_path("deps_on_git_repo") }
        ] ]
    end
  end

  defmodule GitApp do
    def project do
      [ app: :git_app,
        version: "0.1.0",
        deps: [
          { :git_repo, "0.1.0", git: MixTest.Case.fixture_path("git_repo") }
        ] ]
    end
  end

  defmodule GitSubmodulesApp do
    def project do
      [ app: :git_app,
        version: "0.1.0",
        deps: [
          { :git_repo, "0.1.0", git: MixTest.Case.fixture_path("git_repo"), submodules: true }
        ] ]
    end
  end

  defmodule GitErrorApp do
    def project do
      [ deps: [
          { :git_repo, "0.1.0", git: MixTest.Case.fixture_path("not_git_repo") }
        ] ]
    end
  end

  test "get, update and clean git repos with compilation" do
    Mix.Project.push GitApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Deps.Get.run []
      message = "* Getting git_repo [git: #{inspect fixture_path("git_repo")}]"
      assert_received { :mix_shell, :info, [^message] }
      assert_received { :mix_shell, :info, ["* Compiling git_repo"] }
      assert_received { :mix_shell, :info, ["Compiled lib/git_repo.ex"] }
      assert_received { :mix_shell, :info, ["Generated git_repo.app"] }
      assert File.exists?("deps/git_repo/ebin/Elixir.GitRepo.beam")
      assert File.read!("mix.lock") =~ %r/"git_repo": {:git, #{inspect fixture_path("git_repo")}, "[a-f0-9]+", \[\]}/

      purge [GitRepo]
      File.touch!("deps/git_repo/ebin/.compile.elixir", { { 2010, 4, 17 }, { 14, 0, 0 } })
      Mix.Task.clear

      Mix.Tasks.Deps.Update.run ["--all"]
      message = "* Updating git_repo (0.1.0) [git: #{inspect fixture_path("git_repo")}]"
      assert_received { :mix_shell, :info, [^message] }
      assert_received { :mix_shell, :info, ["* Compiling git_repo"] }
      assert_received { :mix_shell, :info, ["Compiled lib/git_repo.ex"] }

      Mix.Tasks.Deps.Clean.run ["--all"]
      message = "* Cleaning git_repo (0.1.0) [git: #{inspect fixture_path("git_repo")}]"
      assert_received { :mix_shell, :info, [^message] }
      refute File.exists?("deps/git_repo/ebin/Elixir.Git.Repo.beam")
    end
  after
    Mix.Project.pop
  end

  test "gets many levels deep dependencies" do
    Mix.Project.push DepsOnGitApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Deps.Get.run []

      message = "* Getting git_repo [git: #{inspect fixture_path("git_repo")}]"
      assert_received { :mix_shell, :info, [^message] }
      assert_received { :mix_shell, :info, ["* Compiling git_repo"] }


      message = "* Getting deps_on_git_repo [git: #{inspect fixture_path("deps_on_git_repo")}]"
      assert_received { :mix_shell, :info, [^message] }
      assert_received { :mix_shell, :info, ["* Compiling deps_on_git_repo"] }

      assert File.exists?("deps/deps_on_git_repo/mix.exs")
      assert File.exists?("deps/git_repo/mix.exs")
    end
  after
    purge [GitRepo, GitRepo.Mix]
    Mix.Project.pop
  end

  test "does not check if repo information changes" do
    Mix.Project.push GitApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Deps.Get.run []
      message = "* Getting git_repo [git: #{inspect fixture_path("git_repo")}]"
      assert_received { :mix_shell, :info, [^message] }

      # We can compile just fine
      Mix.Task.clear
      Mix.Tasks.Run.run ["-e", "1+2"]

      # Now let's add a submodules option
      Mix.Project.pop
      Mix.Project.push GitSubmodulesApp

      # We should fail because the lock diverged
      Mix.Task.clear
      assert_raise Mix.Error, fn ->
        Mix.Tasks.Run.run ["1+2"]
      end
    end
  after
    purge [GitRepo, GitRepo.Mix]
    Mix.Project.pop
  end

  test "recompiles the project when a deps change" do
    Mix.Project.push GitApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Deps.Get.run []
      message = "* Getting git_repo [git: #{inspect fixture_path("git_repo")}]"
      assert_received { :mix_shell, :info, [^message] }

      # We can compile just fine
      Mix.Task.clear
      Mix.Tasks.Compile.run []

      # Sleep so touch picks up a time difference
      :timer.sleep(1_000)

      # Recompile the dependency
      Mix.Tasks.Deps.Compile.run ["git_repo"]

      # We are forced to recompile
      purge [A, B, C]
      Mix.shell.flush
      Mix.Task.clear
      Mix.Tasks.Compile.run []
      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
    end
  after
    purge [GitRepo, GitRepo.Mix]
    Mix.Project.pop
  end

  test "does not recompiles dependencies if nothing changed" do
    Mix.Project.push GitApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Deps.Get.run []
      message = "* Getting git_repo [git: #{inspect fixture_path("git_repo")}]"
      assert_received { :mix_shell, :info, [^message] }

      # Sleep so touch picks up a time difference
      :timer.sleep(1_000)

      # Recompile the dependency
      Mix.Tasks.Deps.Compile.run ["git_repo"]
      refute_received { :mix_shell, :info, ["Compiled lib/a.ex"] }

      Mix.Tasks.Deps.Compile.run ["git_repo"]
      refute_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
    end
  after
    purge [GitRepo, GitRepo.Mix]
    Mix.Project.pop
  end

  test "all up to date dependencies" do
    Mix.Project.push GitApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Deps.Get.run []

      message = "* Getting git_repo [git: #{inspect fixture_path("git_repo")}]"
      assert_received { :mix_shell, :info, [^message] }
      assert_received { :mix_shell, :info, ["* Compiling git_repo"] }

      Mix.Tasks.Deps.Get.run []
      assert_received { :mix_shell, :info, ["All dependencies up to date"] }
    end
  after
    purge [GitRepo, GitRepo.Mix]
    Mix.Project.pop
  end

  test "requires dependencies before compilation" do
    Mix.Project.push GitApp

    in_fixture "no_mixfile", fn ->
      assert_raise Mix.Error, fn ->
        Mix.Tasks.Compile.run []
      end
    end
  after
    purge [GitRepo, GitRepo.Mix]
    Mix.Project.pop
  end

  test "checks out specific revision and updates it" do
    Mix.Project.push GitApp

    # Get git repo first revision
    [last, first|_] = get_git_repo_revs

    in_fixture "no_mixfile", fn ->
      Mix.Deps.Lock.write [git_repo: first]

      Mix.Tasks.Deps.Get.run []
      refute File.exists?("deps/git_repo/lib/git_repo.ex")
      assert File.read!("mix.lock") =~ first

      Mix.Tasks.Deps.Update.run ["git_repo"]
      assert File.exists?("deps/git_repo/lib/git_repo.ex")
      assert File.read!("mix.lock") =~ last

      Mix.Tasks.Deps.Clean.run ["--all"]
      refute File.exists?("deps/git_repo/ebin/Elixir.Git.Repo.beam")
      assert File.read!("mix.lock") =~ last

      Mix.Tasks.Deps.Clean.run ["--unlock", "--all"]
      refute File.read!("mix.lock") =~ last
    end
  after
    purge [GitRepo, GitRepo.Mix]
    Mix.Project.pop
  end

  test "checks out specific revision and gets new lock" do
    Mix.Project.push GitApp

    # Get git repo first revision
    [last, first|_] = get_git_repo_revs

    in_fixture "no_mixfile", fn ->
      Mix.Deps.Lock.write [git_repo: { :git, fixture_path("git_repo"), first, [branch: "master"] }]

      Mix.Tasks.Deps.Get.run []
      refute File.exists?("deps/git_repo/lib/git_repo.ex")
      assert File.read!("mix.lock") =~ first

      message = "* Getting git_repo [git: #{inspect fixture_path("git_repo")}]"
      assert_received { :mix_shell, :info, [^message] }

      Mix.Deps.Lock.write [git_repo: last]
      assert_raise Mix.Error, fn ->
        Mix.Tasks.Deps.Check.run []
      end

      # Flush the errors we got on out of date deps
      Mix.shell.flush
      Mix.Task.clear

      Mix.Tasks.Deps.Get.run []
      assert File.exists?("deps/git_repo/lib/git_repo.ex")
      assert File.read!("mix.lock") =~ last

      message = "* Getting git_repo [git: #{inspect fixture_path("git_repo")}]"
      assert_received { :mix_shell, :info, [^message] }

      # Check we got no error
      refute_received { :mix_shell, :error, _ }
    end
  after
    purge [GitRepo, GitRepo.Mix]
    Mix.Project.pop
  end

  test "does not attempt to compile projects that could not be retrieved" do
    Mix.Project.push GitErrorApp

    in_fixture "no_mixfile", fn ->
      exception = assert_raise Mix.Error, fn ->
        Mix.Tasks.Deps.Get.run []
      end
      assert exception.message =~ "Command `git clone"
    end
  after
    Mix.Project.pop
  end

  defp get_git_repo_revs do
    File.cd! fixture_path("git_repo"), fn ->
      Regex.split %r(\n), System.cmd("git log --format=%H")
    end
  end
end
