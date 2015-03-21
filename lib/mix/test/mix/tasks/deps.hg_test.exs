Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.DepsHgTest do
  use MixTest.Case
  
  defmodule DepsOnHgApp do
    def project do
      [ app: :deps_on_hg_app,
        version: "0.1.0",
        deps: [
          {:deps_on_hg_repo, "0.2.0", hg: MixTest.Case.fixture_path("deps_on_hg_repo")}
        ] ]
    end
  end

  defmodule HgApp do
    def project do
      [ app: :hg_app,
        version: "0.1.0",
        deps: [
          {:hg_repo, "0.1.0", hg: MixTest.Case.fixture_path("hg_repo")}
        ] ]
    end
  end

  defmodule HgErrorApp do
    def project do
      [ deps: [
          {:hg_repo, "0.1.0", hg: MixTest.Case.fixture_path("not_hg_repo")}
        ] ]
    end
  end

  test "gets and updates hg repos with compilation" do
    Mix.Project.push HgApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Deps.Get.run []
      message = "* Getting hg_repo (#{fixture_path("hg_repo")})"
      assert_received {:mix_shell, :info, [^message]}
      assert File.read!("mix.lock") =~
             ~r/"hg_repo": {:hg, #{inspect fixture_path("hg_repo")}, "[a-f0-9]+", \[\]}/

      Mix.Tasks.Deps.Update.run ["--all"]
      message = "* Updating hg_repo (#{fixture_path("hg_repo")})"
      assert_received {:mix_shell, :info, [^message]}
    end
  end

  test "handles invalid .hg directory" do
    Mix.Project.push HgApp

    in_fixture "no_mixfile", fn ->
      File.mkdir_p!("deps/hg_repo/.hg")
      Mix.Tasks.Deps.Get.run []
      message = "* Getting hg_repo (#{fixture_path("hg_repo")})"
      assert_received {:mix_shell, :info, [^message]}
    end
  end

  test "handles missing .hg directory" do
    Mix.Project.push HgApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Deps.Get.run []
      message = "* Getting hg_repo (#{fixture_path("hg_repo")})"
      assert_received {:mix_shell, :info, [^message]}
      File.rm_rf!("deps/hg_repo/.hg")

      assert_raise Mix.Error, "Can't continue due to errors on dependencies", fn ->
        Mix.Tasks.Deps.Check.run ["hg_repo"]
      end
    end
  end

  test "gets and updates many levels deep dependencies" do
    Mix.Project.push DepsOnHgApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Deps.Get.run []

      message = "* Getting hg_repo (#{fixture_path("hg_repo")})"
      assert_received {:mix_shell, :info, [^message]}

      message = "* Getting deps_on_hg_repo (#{fixture_path("deps_on_hg_repo")})"
      assert_received {:mix_shell, :info, [^message]}

      assert File.exists?("deps/deps_on_hg_repo/mix.exs")
      assert File.rm("deps/deps_on_hg_repo/.fetch") == :ok
      assert File.exists?("deps/hg_repo/mix.exs")
      assert File.rm("deps/hg_repo/.fetch") == :ok

      # Compile the dependencies
      Mix.Tasks.Deps.Compile.run []

      # Now update children and make sure it propagates
      Mix.Tasks.Deps.Update.run ["hg_repo"]
      assert File.exists?("deps/deps_on_hg_repo/.fetch")
      assert File.exists?("deps/hg_repo/.fetch")

      # Compile hg repo but unload it so...
      Mix.Tasks.Deps.Compile.run ["hg_repo"]
      assert File.exists?("_build/dev/lib/hg_repo/ebin")
      Code.delete_path("_build/dev/lib/hg_repo/ebin")

      # Deps on hg repo loads it automatically on compile
      Mix.Task.reenable "deps.loadpaths"
      Mix.Tasks.Deps.Compile.run ["deps_on_hg_repo"]
      assert File.exists?("_build/dev/lib/deps_on_hg_repo/ebin")
    end
  after
    purge [HgRepo, HgRepo.Mix]
  end

  test "recompiles the project when a dep is fetched" do
    Mix.Project.push HgApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Deps.Get.run []
      assert File.exists?("deps/hg_repo/.fetch")

      # We can compile just fine
      Mix.Tasks.Compile.run []
      assert_received {:mix_shell, :info, ["Compiled lib/hg_repo.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}

      # Clear up to prepare for the update
      File.rm("_build/dev/lib/hg_repo/ebin/Elixir.HgRepo.beam")
      File.rm("_build/dev/lib/hg_repo/.compile.elixir")
      File.rm("deps/hg_repo/.fetch")
      Mix.Task.clear
      Mix.shell.flush
      purge [A, B, C, HgRepo]

      # Update will mark the update required
      Mix.Tasks.Deps.Update.run ["hg_repo"]
      assert File.exists?("deps/hg_repo/.fetch")
      ensure_touched("deps/hg_repo/.fetch") # Ensure timestamp differs

      # mix deps.compile is required...
      Mix.Tasks.Deps.run []
      msg = "  the dependency build is outdated, please run `mix deps.compile`"
      assert_received {:mix_shell, :info, [^msg]}

      # But also ran automatically
      Mix.Tasks.Compile.run []
      assert_received {:mix_shell, :info, ["Compiled lib/hg_repo.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert File.exists?("_build/dev/lib/hg_repo/.compile.fetch")
      :ok
    end
  after
    purge [A, B, C, HgRepo, HgRepo.Mix]
  end

  test "all up to date dependencies" do
    Mix.Project.push HgApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Deps.Get.run []
      message = "* Getting hg_repo (#{fixture_path("hg_repo")})"
      assert_received {:mix_shell, :info, [^message]}

      Mix.Tasks.Deps.Get.run []
      assert_received {:mix_shell, :info, ["All dependencies up to date"]}
    end
  after
    purge [HgRepo, HgRepo.Mix]
  end

  test "updates the lock when the repo updates" do
    Mix.Project.push HgApp

    # Get hg repo first revision
    [last, first|_] = get_hg_repo_revs

    in_fixture "no_mixfile", fn ->
      Mix.Dep.Lock.write %{hg_repo: {:hg, fixture_path("hg_repo"), first, []}}

      Mix.Tasks.Deps.Get.run []
      refute File.exists?("deps/hg_repo/lib/hg_repo.ex")
      assert File.read!("mix.lock") =~ first

      Mix.Tasks.Deps.Update.run ["hg_repo"]
      assert File.exists?("deps/hg_repo/lib/hg_repo.ex")
      assert File.read!("mix.lock") =~ last

      Mix.Tasks.Deps.Clean.run ["--all"]
      refute File.exists?("deps/hg_repo/lib/hg_repo.ex")
      assert File.read!("mix.lock") =~ last

      Mix.Tasks.Deps.Clean.run ["--unlock", "--all"]
      refute File.read!("mix.lock") =~ last
    end
  after
    purge [HgRepo, HgRepo.Mix]
  end

  test "updates the repo when the lock updates" do
    Mix.Project.push HgApp
    [last, first|_] = get_hg_repo_revs

    in_fixture "no_mixfile", fn ->
      Mix.Dep.Lock.write %{hg_repo: {:hg, fixture_path("hg_repo"), first, []}}

      Mix.Tasks.Deps.Get.run []
      refute File.exists?("deps/hg_repo/lib/hg_repo.ex")
      assert File.read!("mix.lock") =~ first

      # Update the lock and now we should get an error
      Mix.Dep.Lock.write %{hg_repo: {:hg, fixture_path("hg_repo"), last, []}}
      assert_raise Mix.Error, fn ->
        Mix.Tasks.Deps.Check.run []
      end

      # Flush the errors we got, move to a clean slate
      Mix.shell.flush
      Mix.Task.clear

      # Calling get should update the dependency
      Mix.Tasks.Deps.Get.run []
      assert File.exists?("deps/hg_repo/lib/hg_repo.ex")
      assert File.read!("mix.lock") =~ last

      message = "* Updating hg_repo (#{fixture_path("hg_repo")})"
      assert_received {:mix_shell, :info, [^message]}

      # Check we got no error
      refute_received {:mix_shell, :error, _}
    end
  after
    purge [HgRepo, HgRepo.Mix]
  end

  test "updates the repo and the lock when the mixfile updates" do
    Mix.Project.push HgApp
    [last, first|_] = get_hg_repo_revs

    in_fixture "no_mixfile", fn ->
      # Move to the first version
      Mix.Dep.Lock.write %{hg_repo: {:hg, fixture_path("hg_repo"), first, []}}

      Mix.Tasks.Deps.Get.run []
      assert File.read!("mix.lock") =~ first

      # Update the project configuration. It should force an update.
      refresh deps: [{:hg_repo, "0.1.0", hg: fixture_path("hg_repo"), ref: last}]

      Mix.Tasks.Deps.run []
      msg = "  lock outdated: the lock is outdated compared to the options in your mixfile"
      assert_received {:mix_shell, :info, [^msg]}

      # Check an update was triggered
      Mix.Tasks.Deps.Get.run []
      assert File.read!("mix.lock") =~ last

      message = "* Getting hg_repo (#{fixture_path("hg_repo")})"
      assert_received {:mix_shell, :info, [^message]}

      # Check we got no error
      refute_received {:mix_shell, :error, _}
    end
  after
    purge [HgRepo, HgRepo.Mix]
  end

  test "does not attempt to compile projects that could not be retrieved" do
    Mix.Project.push HgErrorApp

    in_fixture "no_mixfile", fn ->
      exception = assert_raise Mix.Error, fn ->
        Mix.Tasks.Deps.Get.run []
      end
      assert Exception.message(exception) =~ "Command `hg clone"
    end
  end

  test "does not load bad mixfiles on get" do
    Mix.Project.push HgApp
    [last, _, bad|_] = get_hg_repo_revs

    in_fixture "no_mixfile", fn ->
      Mix.Dep.Lock.write %{hg_repo: {:hg, fixture_path("hg_repo"), bad, []}}
      catch_error(Mix.Tasks.Deps.Get.run [])

      Mix.Dep.Lock.write %{hg_repo: {:hg, fixture_path("hg_repo"), last, []}}
      Mix.Tasks.Deps.Get.run []
      assert File.read!("mix.lock") =~ last
    end
  after
    purge [HgRepo, HgRepo.Mix]
  end

  test "does not load bad mixfiles on update" do
    Mix.Project.push HgApp
    [last, _, bad|_] = get_hg_repo_revs

    in_fixture "no_mixfile", fn ->
      Mix.Dep.Lock.write %{hg_repo: {:hg, fixture_path("hg_repo"), bad, []}}
      catch_error(Mix.Tasks.Deps.Get.run [])

      Mix.Tasks.Deps.Update.run ["hg_repo"]
      Mix.Tasks.Deps.Compile.run ["hg_repo"]
      assert File.read!("mix.lock") =~ last
    end
  after
    purge [HgRepo, HgRepo.Mix]
  end

  defp refresh(post_config) do
    %{name: name, file: file} = Mix.Project.pop
    Mix.ProjectStack.post_config(post_config)
    Mix.Project.push(name, file)
  end

  defp get_hg_repo_revs do
    File.cd! fixture_path("hg_repo"), fn ->
      Regex.split ~r(\r?\n), System.cmd("hg", ["log", "-T", "{node}\n"]) |> elem(0) |> String.strip
    end
  end
end
