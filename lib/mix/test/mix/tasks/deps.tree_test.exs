Code.require_file("../../test_helper.exs", __DIR__)

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

  defmodule UmbrellaApp do
    def project do
      [
        app: :sample,
        version: "0.1.0",
        deps: [
          {:umbrella_dep, in_umbrella: true}
        ]
      ]
    end
  end

  test "shows the dependency tree", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(ConvergedDepsApp)

      Mix.Tasks.Deps.Tree.run(["--format", "pretty"])
      assert_received {:mix_shell, :info, ["sample"]}
      assert_received {:mix_shell, :info, ["├── deps_on_git_repo 0.2.0 (" <> _]}
      assert_received {:mix_shell, :info, ["└── git_repo >= 0.1.0 (" <> _]}

      Mix.Tasks.Deps.Get.run([])
      Mix.Tasks.Deps.Tree.run(["--format", "pretty"])
      assert_received {:mix_shell, :info, ["sample"]}
      assert_received {:mix_shell, :info, ["├── deps_on_git_repo 0.2.0 (" <> _]}
      assert_received {:mix_shell, :info, ["│   └── git_repo (" <> _]}
      assert_received {:mix_shell, :info, ["└── git_repo >= 0.1.0 (" <> _]}
    end)
  after
    purge([DepsOnGitRepo.MixProject, GitRepo.MixProject])
  end

  test "show the dependency tree for umbrella apps" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        Mix.Task.run("deps.tree", ["--format", "pretty"])
        assert_received {:mix_shell, :info, ["foo"]}
        assert_received {:mix_shell, :info, ["bar"]}
        assert_received {:mix_shell, :info, ["└── foo (../foo)"]}
      end)
    end)
  end

  test "filters umbrella deps only with --umbrella-only" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        Mix.Task.run("deps.tree", ["--format", "pretty", "--umbrella-only"])
        assert_received {:mix_shell, :info, ["foo"]}
        assert_received {:mix_shell, :info, ["bar"]}
        assert_received {:mix_shell, :info, ["└── foo (../foo)"]}
      end)
    end)
  end

  test "shows the given dependency", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(ConvergedDepsApp)

      assert_raise Mix.Error, "could not find dependency unknown", fn ->
        Mix.Tasks.Deps.Tree.run(["--format", "pretty", "unknown"])
      end

      Mix.Tasks.Deps.Tree.run(["--format", "pretty", "deps_on_git_repo"])
      assert_received {:mix_shell, :info, ["deps_on_git_repo 0.2.0 (" <> _]}
      refute_received {:mix_shell, :info, ["└── git_repo (" <> _]}
    end)
  end

  test "shows overridden deps", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(OverriddenDepsApp)

      Mix.Tasks.Deps.Tree.run(["--format", "pretty"])
      assert_received {:mix_shell, :info, ["sample"]}
      assert_received {:mix_shell, :info, ["└── git_repo (" <> msg]}
      assert_received {:mix_shell, :info, ["├── deps_on_git_repo ~r/0.2.0/ (" <> _]}
      assert msg =~ "*override*"
    end)
  end

  test "excludes the given deps", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(OverriddenDepsApp)

      Mix.Tasks.Deps.Tree.run(["--format", "pretty", "--exclude", "deps_on_git_repo"])
      assert_received {:mix_shell, :info, ["sample"]}
      assert_received {:mix_shell, :info, ["└── git_repo (" <> _]}
      refute_received {:mix_shell, :info, ["└── deps_on_git_repo ~r/0.2.0/ (" <> _]}
    end)
  end

  test "shows a particular environment", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(OverriddenDepsApp)

      Mix.Tasks.Deps.Tree.run(["--format", "pretty", "--only", "prod"])
      assert_received {:mix_shell, :info, ["sample"]}
      assert_received {:mix_shell, :info, ["└── git_repo (" <> _]}
      refute_received {:mix_shell, :info, ["└── deps_on_git_repo ~r/0.2.0/ (" <> _]}
    end)
  end

  test "shows the dependency tree in DOT graph format", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(ConvergedDepsApp)

      Mix.Tasks.Deps.Tree.run(["--format", "dot"])

      assert File.read!("deps_tree.dot") == """
             digraph "dependency tree" {
               "sample"
               "sample" -> "deps_on_git_repo" [label="0.2.0"]
               "sample" -> "git_repo" [label=">= 0.1.0"]
             }
             """

      Mix.Tasks.Deps.Get.run([])
      Mix.Tasks.Deps.Tree.run(["--format", "dot"])

      assert File.read!("deps_tree.dot") == """
             digraph "dependency tree" {
               "sample"
               "sample" -> "deps_on_git_repo" [label="0.2.0"]
               "deps_on_git_repo" -> "git_repo" [label=""]
               "sample" -> "git_repo" [label=">= 0.1.0"]
             }
             """
    end)
  after
    purge([DepsOnGitRepo.MixProject, GitRepo.MixProject])
  end
end
