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
      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received { :mix_shell, :info, [^message] }
      assert File.read!("mix.lock") =~ ~r/"git_repo": {:git, #{inspect fixture_path("git_repo")}, "[a-f0-9]+", \[\]}/

      Mix.Tasks.Deps.Update.run ["--all"]
      message = "* Updating git_repo (#{fixture_path("git_repo")})"
      assert_received { :mix_shell, :info, [^message] }
    end
  end

  test "gets many levels deep dependencies" do
    Mix.Project.push DepsOnGitApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Deps.Get.run []

      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received { :mix_shell, :info, [^message] }

      message = "* Getting deps_on_git_repo (#{fixture_path("deps_on_git_repo")})"
      assert_received { :mix_shell, :info, [^message] }

      assert File.exists?("deps/deps_on_git_repo/mix.exs")
      assert File.exists?("deps/git_repo/mix.exs")
    end
  after
    purge [GitRepo, GitRepo.Mix]
  end

  test "checks if repo information changes" do
    Mix.Project.push GitApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Deps.Get.run []
      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received { :mix_shell, :info, [^message] }

      # We can compile just fine
      Mix.Task.clear
      Mix.Tasks.Run.run ["-e", "1+2"]
      assert_received { :mix_shell, :info, ["* Compiling git_repo"] }

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
  end

  test "recompiles the project when a deps change" do
    Mix.Project.push GitApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Deps.Get.run []
      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert File.exists?("_build/dev/lib/git_app/.compile.lock")
      assert_received { :mix_shell, :info, [^message] }

      # We can compile just fine
      Mix.Task.clear
      Mix.Tasks.Compile.run []

      # Notify a deps changed
      Mix.shell.flush
      File.touch!("_build/dev/lib/git_app/.compile.lock", { { 2020, 4, 17 }, { 14, 0, 0 } })

      # We are forced to recompile
      purge [A, B, C]
      Mix.Task.clear
      Mix.Tasks.Compile.run []
      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
    end
  after
    purge [GitRepo, GitRepo.Mix]
  end

  test "all up to date dependencies" do
    Mix.Project.push GitApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Deps.Get.run []
      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received { :mix_shell, :info, [^message] }

      Mix.Tasks.Deps.Get.run []
      assert_received { :mix_shell, :info, ["All dependencies up to date"] }
    end
  after
    purge [GitRepo, GitRepo.Mix]
  end

  test "updates the lock when the repo updates" do
    Mix.Project.push GitApp

    # Get git repo first revision
    [last, first|_] = get_git_repo_revs

    in_fixture "no_mixfile", fn ->
      Mix.Dep.Lock.write [git_repo: { :git, fixture_path("git_repo"), first, [] }]

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
  end

  test "updates the repo when the lock updates" do
    Mix.Project.push GitApp
    [last, first|_] = get_git_repo_revs

    in_fixture "no_mixfile", fn ->
      Mix.Dep.Lock.write [git_repo: { :git, fixture_path("git_repo"), first, [] }]

      Mix.Tasks.Deps.Get.run []
      refute File.exists?("deps/git_repo/lib/git_repo.ex")
      assert File.read!("mix.lock") =~ first

      # Update the lock and now we should get an error
      Mix.Dep.Lock.write [git_repo: { :git, fixture_path("git_repo"), last, [] }]
      assert_raise Mix.Error, fn ->
        Mix.Tasks.Deps.Check.run []
      end

      # Flush the errors we got, move to a clean slate
      Mix.shell.flush
      Mix.Task.clear

      # Calling get should update the dependency
      Mix.Tasks.Deps.Get.run []
      assert File.exists?("deps/git_repo/lib/git_repo.ex")
      assert File.read!("mix.lock") =~ last

      message = "* Updating git_repo (#{fixture_path("git_repo")})"
      assert_received { :mix_shell, :info, [^message] }

      # Check we got no error
      refute_received { :mix_shell, :error, _ }
    end
  after
    purge [GitRepo, GitRepo.Mix]
  end

  test "updates the repo and the lock when the mixfile updates" do
    Mix.Project.push GitApp
    [last, first|_] = get_git_repo_revs

    in_fixture "no_mixfile", fn ->
      # Move to the first version
      Mix.Dep.Lock.write [git_repo: { :git, fixture_path("git_repo"), first, [] }]

      Mix.Tasks.Deps.Get.run []
      assert File.read!("mix.lock") =~ first

      # Update the project configuration. It should force an update.
      refresh deps: [{ :git_repo, "0.1.0", git: fixture_path("git_repo"), ref: last }]

      # Check an update was triggered
      Mix.Tasks.Deps.Get.run []
      assert File.read!("mix.lock") =~ last

      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received { :mix_shell, :info, [^message] }

      # Check we got no error
      refute_received { :mix_shell, :error, _ }
    end
  after
    purge [GitRepo, GitRepo.Mix]
  end

  test "does not attempt to compile projects that could not be retrieved" do
    Mix.Project.push GitErrorApp

    in_fixture "no_mixfile", fn ->
      exception = assert_raise Mix.Error, fn ->
        Mix.Tasks.Deps.Get.run []
      end
      assert exception.message =~ "Command `git clone"
    end
  end

  test "does not load bad mix files on get" do
    Mix.Project.push GitApp
    [last, _, bad|_] = get_git_repo_revs

    in_fixture "no_mixfile", fn ->
      Mix.Dep.Lock.write [git_repo: { :git, fixture_path("git_repo"), bad, [] }]
      catch_error(Mix.Tasks.Deps.Get.run [])

      Mix.Dep.Lock.write [git_repo: { :git, fixture_path("git_repo"), last, [] }]
      Mix.Tasks.Deps.Get.run []
      assert File.read!("mix.lock") =~ last
    end
  after
    purge [GitRepo, GitRepo.Mix]
  end

  test "does not load bad mix files on update" do
    Mix.Project.push GitApp
    [last, _, bad|_] = get_git_repo_revs

    in_fixture "no_mixfile", fn ->
      Mix.Dep.Lock.write [git_repo: { :git, fixture_path("git_repo"), bad, [] }]
      catch_error(Mix.Tasks.Deps.Get.run [])

      Mix.Tasks.Deps.Update.run ["git_repo"]
      Mix.Tasks.Deps.Compile.run ["git_repo"]
      assert File.read!("mix.lock") =~ last
    end
  after
    purge [GitRepo, GitRepo.Mix]
  end

  defp refresh(post_config) do
    { current, _config, file } = Mix.Project.pop
    Mix.ProjectStack.post_config(post_config)
    Mix.Project.push(current, file)
  end

  defp get_git_repo_revs do
    File.cd! fixture_path("git_repo"), fn ->
      Regex.split ~r(\r?\n), System.cmd("git log --format=%H")
    end
  end
end
