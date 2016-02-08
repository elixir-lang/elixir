Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Deps.TreeTest do
  use MixTest.Case

  defmodule ConvergedDepsApp do
    def project do
      [
        app: :sample,
        version: "0.1.0",
        deps: [
          {:deps_on_git_repo, "0.2.0", git: fixture_path("deps_on_git_repo")},
          {:git_repo, ">= 0.1.0", git: MixTest.Case.fixture_path("git_repo")}
        ]
      ]
    end
  end

  defmodule OverriddenDepsApp do
    def project do
      [
        app: :sample,
        version: "0.1.0",
        deps: [
          {:deps_on_git_repo, ~r"0.2.0", git: fixture_path("deps_on_git_repo"), only: :test},
          {:git_repo, git: MixTest.Case.fixture_path("git_repo"), override: true}
        ]
      ]
    end
  end

  test "shows the dependency tree", context do
    Mix.Project.push ConvergedDepsApp

    in_tmp context.test, fn ->
      Mix.Tasks.Deps.Tree.run(["--pretty"])
      assert_received {:mix_shell, :info, ["sample"]}
      assert_received {:mix_shell, :info, ["├── git_repo >= 0.1.0 (" <> _]}
      assert_received {:mix_shell, :info, ["└── deps_on_git_repo 0.2.0 (" <> _]}
      refute_received {:mix_shell, :info, ["    └── git_repo (" <> _]}

      Mix.Tasks.Deps.Get.run([])
      Mix.Tasks.Deps.Tree.run(["--pretty"])
      assert_received {:mix_shell, :info, ["sample"]}
      assert_received {:mix_shell, :info, ["├── git_repo >= 0.1.0 (" <> _]}
      assert_received {:mix_shell, :info, ["└── deps_on_git_repo 0.2.0 (" <> _]}
      assert_received {:mix_shell, :info, ["    └── git_repo (" <> _]}
    end
  after
    purge [DepsOnGitRepo.Mixfile, GitRepo.Mixfile]
  end

  test "show the dependency tree for umbrella apps" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        Mix.Task.run "deps.tree", ["--pretty"]
        assert_received {:mix_shell, :info, ["foo"]}
        assert_received {:mix_shell, :info, ["bar"]}
        assert_received {:mix_shell, :info, ["└── foo (../foo)"]}
      end)
    end
  end

  test "shows the given dependency", context do
    Mix.Project.push ConvergedDepsApp

    in_tmp context.test, fn ->
      assert_raise Mix.Error, "could not find dependency unknown", fn ->
        Mix.Tasks.Deps.Tree.run(["--pretty", "unknown"])
      end

      Mix.Tasks.Deps.Tree.run(["--pretty", "deps_on_git_repo"])
      assert_received {:mix_shell, :info, ["deps_on_git_repo 0.2.0 (" <> _]}
      refute_received {:mix_shell, :info, ["└── git_repo (" <> _]}
    end
  end

  test "shows overridden deps", context do
    Mix.Project.push OverriddenDepsApp

    in_tmp context.test, fn ->
      Mix.Tasks.Deps.Tree.run(["--pretty"])
      assert_received {:mix_shell, :info, ["sample"]}
      assert_received {:mix_shell, :info, ["├── git_repo (" <> msg]}
      assert_received {:mix_shell, :info, ["└── deps_on_git_repo ~r/0.2.0/ (" <> _]}
      assert msg =~ "*override*"
    end
  end

  test "excludes the given deps", context do
    Mix.Project.push OverriddenDepsApp

    in_tmp context.test, fn ->
      Mix.Tasks.Deps.Tree.run(["--pretty", "--exclude", "deps_on_git_repo"])
      assert_received {:mix_shell, :info, ["sample"]}
      assert_received {:mix_shell, :info, ["└── git_repo (" <> _]}
      refute_received {:mix_shell, :info, ["└── deps_on_git_repo ~r/0.2.0/ (" <> _]}
    end
  end

  test "shows a particular environment", context do
    Mix.Project.push OverriddenDepsApp

    in_tmp context.test, fn ->
      Mix.Tasks.Deps.Tree.run(["--pretty", "--only", "prod"])
      assert_received {:mix_shell, :info, ["sample"]}
      assert_received {:mix_shell, :info, ["└── git_repo (" <> _]}
      refute_received {:mix_shell, :info, ["└── deps_on_git_repo ~r/0.2.0/ (" <> _]}
    end
  end
end
