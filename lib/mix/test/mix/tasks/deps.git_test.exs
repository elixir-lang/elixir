Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.DepsGitTest do
  use MixTest.Case

  defmodule DepsOnGitApp do
    def project do
      [
        app: :deps_on_git_app,
        version: "0.1.0",
        deps: [
          {:deps_on_git_repo, "0.2.0", git: fixture_path("deps_on_git_repo")}
        ]
      ]
    end
  end

  defmodule GitApp do
    def project do
      opts = Process.get(:git_repo_opts) || []

      [
        app: :git_app,
        version: "0.1.0",
        deps: [
          {:git_repo, "0.1.0", [git: fixture_path("git_repo")] ++ opts}
        ]
      ]
    end
  end

  defmodule GitSubmodulesApp do
    def project do
      [
        app: :git_app,
        version: "0.1.0",
        deps: [
          {:git_repo, "0.1.0", git: fixture_path("git_repo"), submodules: true}
        ]
      ]
    end
  end

  defmodule GitErrorApp do
    def project do
      [
        deps: [
          {:git_repo, "0.1.0", git: fixture_path("not_git_repo")}
        ]
      ]
    end
  end

  test "gets and updates Git repos with compilation" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(GitApp)

      Mix.Tasks.Deps.Get.run([])
      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received {:mix_shell, :info, [^message]}

      assert File.read!("mix.lock") =~
               ~r/"git_repo": {:git, #{inspect(fixture_path("git_repo"))}, "[a-f0-9]+", \[\]}/

      Mix.Tasks.Deps.Update.run(["--all"])
      message = "* Updating git_repo (#{fixture_path("git_repo")})"
      assert_received {:mix_shell, :info, [^message]}
    end)
  end

  test "gets and updates Git repos with submodules" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(GitSubmodulesApp)

      Mix.Tasks.Deps.Get.run([])
      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received {:mix_shell, :info, [^message]}
      assert File.read!("mix.lock") =~ "submodules: true"
    end)
  end

  @tag :git_sparse
  test "gets and updates Git repos with sparse checkout" do
    Process.put(:git_repo_opts, sparse: "sparse_dir")

    in_fixture("no_mixfile", fn ->
      Mix.Project.push(GitApp)

      Mix.Tasks.Deps.Get.run([])
      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received {:mix_shell, :info, [^message]}
      refute File.exists?("deps/git_repo/mix.exs")
      assert File.exists?("deps/git_repo/sparse_dir/mix.exs")
      assert File.read!("mix.lock") =~ "sparse: \"sparse_dir\""
    end)
  end

  test "gets and updates Git repos with subdir" do
    Process.put(:git_repo_opts, subdir: "subdir")

    in_fixture("no_mixfile", fn ->
      Mix.Project.push(GitApp)

      Mix.Tasks.Deps.Get.run([])
      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received {:mix_shell, :info, [^message]}
      assert File.read!("mix.lock") =~ "subdir: \"subdir\""
    end)
  end

  test "handles invalid .git directory" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(GitApp)

      File.mkdir_p!("deps/git_repo/.git")
      Mix.Tasks.Deps.Get.run([])
      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received {:mix_shell, :info, [^message]}
    end)
  end

  test "handles missing .git directory" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(GitApp)

      Mix.Tasks.Deps.Get.run([])
      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received {:mix_shell, :info, [^message]}
      File.rm_rf!("deps/git_repo/.git")

      assert_raise Mix.Error, "Can't continue due to errors on dependencies", fn ->
        Mix.Tasks.Deps.Loadpaths.run(["git_repo"])
      end
    end)
  end

  test "gets and updates many levels deep dependencies" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(DepsOnGitApp)

      Mix.Tasks.Deps.Get.run([])

      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received {:mix_shell, :info, [^message]}

      message = "* Getting deps_on_git_repo (#{fixture_path("deps_on_git_repo")})"
      assert_received {:mix_shell, :info, [^message]}

      assert File.exists?("deps/deps_on_git_repo/mix.exs")
      assert File.rm("deps/deps_on_git_repo/.fetch") == :ok
      assert File.exists?("deps/git_repo/mix.exs")
      assert File.rm("deps/git_repo/.fetch") == :ok

      # Compile the dependencies
      Mix.Tasks.Deps.Compile.run([])

      # Now update children and make sure it propagates
      Mix.Tasks.Deps.Update.run(["git_repo"])
      assert File.exists?("deps/deps_on_git_repo/.fetch")
      assert File.exists?("deps/git_repo/.fetch")

      # Clear tasks to recompile Git repo but unload it so...
      purge([GitRepo])
      Mix.Task.clear()
      Mix.Tasks.Deps.Compile.run(["git_repo"])
      assert File.exists?("_build/dev/lib/git_repo/ebin")
      Code.delete_path("_build/dev/lib/git_repo/ebin")

      # Deps on Git repo loads it automatically on compile
      Mix.Task.reenable("deps.loadpaths")
      Mix.Tasks.Deps.Compile.run(["deps_on_git_repo"])
      assert File.exists?("_build/dev/lib/deps_on_git_repo/ebin")
    end)
  after
    purge([GitRepo, GitRepo.MixProject])
  end

  test "compiles many levels deep dependencies" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(DepsOnGitApp)

      Mix.Tasks.Deps.Get.run([])
      refute File.exists?("_build/dev/lib/deps_on_git_repo")
      refute File.exists?("_build/dev/lib/git_repo")

      # Compile the parent with children
      Mix.Tasks.Deps.Compile.run(["deps_on_git_repo", "--include-children"])
      assert File.exists?("_build/dev/lib/deps_on_git_repo")
      assert File.exists?("_build/dev/lib/git_repo")
    end)
  after
    purge([GitRepo, GitRepo.MixProject])
  end

  test "all dependencies are up to date" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(GitApp)

      Mix.Tasks.Deps.Get.run([])
      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received {:mix_shell, :info, [^message]}

      Mix.Tasks.Deps.Get.run([])
      assert_received {:mix_shell, :info, ["All dependencies are up to date"]}
    end)
  after
    purge([GitRepo, GitRepo.MixProject])
  end

  test "updates the lock when the repo updates" do
    Mix.Project.push(GitApp)

    # Get Git repo first revision
    [last, first | _] = get_git_repo_revs("git_repo")

    in_fixture("no_mixfile", fn ->
      Mix.Dep.Lock.write(%{git_repo: {:git, fixture_path("git_repo"), first, []}})

      Mix.Tasks.Deps.Get.run([])
      refute File.exists?("deps/git_repo/lib/git_repo.ex")
      assert File.read!("mix.lock") =~ first

      Mix.Tasks.Deps.Update.run(["git_repo"])
      assert File.exists?("deps/git_repo/lib/git_repo.ex")
      assert File.read!("mix.lock") =~ last

      Mix.Tasks.Deps.Clean.run(["--all"])
      refute File.exists?("deps/git_repo/lib/git_repo.ex")
      assert File.read!("mix.lock") =~ last

      Mix.Tasks.Deps.Clean.run(["--unlock", "--all"])
      refute File.read!("mix.lock") =~ last
    end)
  after
    purge([GitRepo, GitRepo.MixProject])
  end

  test "updates the repo when the lock updates" do
    Mix.Project.push(GitApp)
    [last, first | _] = get_git_repo_revs("git_repo")

    in_fixture("no_mixfile", fn ->
      Mix.Dep.Lock.write(%{git_repo: {:git, fixture_path("git_repo"), first, []}})

      Mix.Tasks.Deps.Get.run([])
      refute File.exists?("deps/git_repo/lib/git_repo.ex")
      assert File.read!("mix.lock") =~ first

      # Update the lock and now we should get an error
      Mix.Dep.Lock.write(%{git_repo: {:git, fixture_path("git_repo"), last, []}})

      assert_raise Mix.Error, fn -> Mix.Tasks.Deps.Loadpaths.run([]) end

      # Flush the errors we got, move to a clean slate
      Mix.shell().flush
      Mix.Task.clear()

      # Calling get should update the dependency
      Mix.Tasks.Deps.Get.run([])
      assert File.exists?("deps/git_repo/lib/git_repo.ex")
      assert File.read!("mix.lock") =~ last

      message = "* Updating git_repo (#{fixture_path("git_repo")})"
      assert_received {:mix_shell, :info, [^message]}

      # Check we got no error
      refute_received {:mix_shell, :error, _}
    end)
  after
    purge([GitRepo, GitRepo.MixProject])
  end

  @tag :git_sparse
  test "updates the repo when sparse is turned off" do
    Process.put(:git_repo_opts, sparse: "sparse_dir")

    in_fixture("no_mixfile", fn ->
      Mix.Project.push(GitApp)

      Mix.Tasks.Deps.Get.run([])
      refute File.exists?("deps/git_repo/lib/git_repo.ex")

      # Flush the errors we got, move to a clean slate
      Mix.shell().flush
      Mix.Task.clear()
      Process.delete(:git_repo_opts)
      Mix.Project.pop()
      Mix.Project.push(GitApp)

      # Calling get should update the dependency
      Mix.Tasks.Deps.Get.run([])
      refute File.read!("mix.lock") =~ "sparse_dir"
      assert File.exists?("deps/git_repo/lib/git_repo.ex")

      message = "* Updating git_repo (#{fixture_path("git_repo")})"
      assert_received {:mix_shell, :info, [^message]}

      # Check we got no error
      refute_received {:mix_shell, :error, _}
    end)
  end

  @tag :git_sparse
  test "updates the repo when sparse is turned on" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(GitApp)

      Mix.Tasks.Deps.Get.run([])
      assert File.exists?("deps/git_repo/lib/git_repo.ex")

      # Flush the errors we got, move to a clean slate
      Mix.shell().flush
      Mix.Task.clear()
      Process.put(:git_repo_opts, sparse: "sparse_dir")
      Mix.Project.pop()
      Mix.Project.push(GitApp)

      # Calling get should update the dependency
      Mix.Tasks.Deps.Get.run([])
      assert File.read!("mix.lock") =~ "sparse_dir"
      refute File.exists?("deps/git_repo/lib/git_repo.ex")

      message = "* Updating git_repo (#{fixture_path("git_repo")})"
      assert_received {:mix_shell, :info, [^message]}

      # Check we got no error
      refute_received {:mix_shell, :error, _}
    end)
  end

  test "updates the repo and the lock when the mixfile updates" do
    Mix.Project.push(GitApp)
    [last, first | _] = get_git_repo_revs("git_repo")

    in_fixture("no_mixfile", fn ->
      # Move to the first version
      Mix.Dep.Lock.write(%{git_repo: {:git, fixture_path("git_repo"), first, []}})

      Mix.Tasks.Deps.Get.run([])
      assert File.read!("mix.lock") =~ first

      # Update the project configuration. It should force an update.
      refresh(deps: [{:git_repo, "0.1.0", git: fixture_path("git_repo"), ref: last}])

      Mix.Tasks.Deps.run([])

      msg =
        "  lock outdated: the lock is outdated compared to the options in your mix.exs. To fetch locked version run \"mix deps.get\""

      assert_received {:mix_shell, :info, [^msg]}

      # Check an update was triggered
      Mix.Tasks.Deps.Get.run([])
      assert File.read!("mix.lock") =~ last

      message = "* Getting git_repo (#{fixture_path("git_repo")})"
      assert_received {:mix_shell, :info, [^message]}

      # Check we got no error
      refute_received {:mix_shell, :error, _}
    end)
  after
    purge([GitRepo, GitRepo.MixProject])
  end

  test "fetches children on updates" do
    Mix.Project.push(DepsOnGitApp)

    # Get Git repo first revision
    [last, first | _] = get_git_repo_revs("deps_on_git_repo")

    in_fixture("no_mixfile", fn ->
      Mix.Dep.Lock.write(%{deps_on_git_repo: {:git, fixture_path("deps_on_git_repo"), first, []}})

      Mix.Tasks.Deps.Get.run([])
      assert File.exists?("deps/deps_on_git_repo/mix.exs")
      refute File.exists?("deps/git_repo/lib/git_repo.ex")
      assert File.read!("mix.lock") =~ first

      Mix.Task.clear()
      Mix.State.clear_cache()
      purge([DepsOnGitRepo.MixProject])

      Mix.Tasks.Deps.Update.run(["deps_on_git_repo"])
      assert File.exists?("deps/git_repo/lib/git_repo.ex")
      assert File.read!("mix.lock") =~ last
      refute File.exists?("_build/dev/lib/git_repo/.mix/compile.fetch")
    end)
  after
    purge([GitRepo, GitRepo.MixProject])
  end

  test "fetches children on get" do
    Mix.Project.push(DepsOnGitApp)

    # Get Git repo first revision
    [last, first | _] = get_git_repo_revs("deps_on_git_repo")

    in_fixture("no_mixfile", fn ->
      Mix.Dep.Lock.write(%{deps_on_git_repo: {:git, fixture_path("deps_on_git_repo"), first, []}})

      Mix.Tasks.Deps.Get.run([])
      assert File.exists?("deps/deps_on_git_repo/mix.exs")
      refute File.exists?("deps/git_repo/lib/git_repo.ex")
      assert File.read!("mix.lock") =~ first

      Mix.Task.clear()
      Mix.State.clear_cache()
      purge([DepsOnGitRepo.MixProject])

      Mix.Dep.Lock.write(%{deps_on_git_repo: {:git, fixture_path("deps_on_git_repo"), last, []}})
      Mix.Tasks.Deps.Get.run([])
      assert File.exists?("deps/git_repo/lib/git_repo.ex")
      assert File.read!("mix.lock") =~ last
      refute File.exists?("_build/dev/lib/git_repo/.mix/compile.fetch")
    end)
  after
    purge([GitRepo, GitRepo.MixProject])
  end

  test "does not attempt to compile projects that could not be retrieved" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(GitErrorApp)

      exception = assert_raise Mix.Error, fn -> Mix.Tasks.Deps.Get.run([]) end

      assert Exception.message(exception) =~ "Command \"git --git-dir=.git fetch"
    end)
  end

  test "does not load bad mixfiles on get" do
    Mix.Project.push(GitApp)
    [last, _, bad | _] = get_git_repo_revs("git_repo")

    in_fixture("no_mixfile", fn ->
      Mix.Dep.Lock.write(%{git_repo: {:git, fixture_path("git_repo"), bad, []}})
      catch_error(Mix.Tasks.Deps.Get.run([]))

      Mix.Dep.Lock.write(%{git_repo: {:git, fixture_path("git_repo"), last, []}})
      Mix.Tasks.Deps.Get.run([])
      assert File.read!("mix.lock") =~ last
    end)
  after
    purge([GitRepo, GitRepo.MixProject])
  end

  test "updates on Git opts change" do
    in_fixture("no_mixfile", fn ->
      Mix.Project.push(GitApp)

      Process.put(:git_repo_opts, tag: "without_module")
      refresh([])
      Mix.Tasks.Deps.Get.run([])
      refute File.regular?("deps/git_repo/lib/git_repo.ex")

      Process.put(:git_repo_opts, tag: "with_module")
      refresh([])
      Mix.Tasks.Deps.Get.run([])
      assert File.regular?("deps/git_repo/lib/git_repo.ex")
    end)
  after
    purge([GitRepo, GitRepo.MixProject])
  end

  test "does not load bad mixfiles on update" do
    Mix.Project.push(GitApp)
    [last, _, bad | _] = get_git_repo_revs("git_repo")

    in_fixture("no_mixfile", fn ->
      Mix.Dep.Lock.write(%{git_repo: {:git, fixture_path("git_repo"), bad, []}})
      catch_error(Mix.Tasks.Deps.Get.run([]))

      Mix.Tasks.Deps.Update.run(["git_repo"])
      Mix.Tasks.Deps.Compile.run(["git_repo"])
      assert File.read!("mix.lock") =~ last
    end)
  after
    purge([GitRepo, GitRepo.MixProject])
  end

  defp refresh(post_config) do
    %{name: name, file: file} = Mix.Project.pop()
    Mix.ProjectStack.post_config(post_config)
    Mix.Project.push(name, file)
  end
end
