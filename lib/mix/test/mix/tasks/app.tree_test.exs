Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.App.TreeTest do
  use MixTest.Case

  defmodule AppDepsSample do
    def project do
      [app: :test, version: "0.1.0"]
    end

    def application do
      [applications: [:logger, :app_deps_sample]]
    end
  end

  @tag apps: [:test, :app_deps_sample, :app_deps2_sample, :app_deps3_sample, :app_deps4_sample]
  test "shows the application tree", context do
    Mix.Project.push(AppDepsSample)

    in_tmp(context.test, fn ->
      load_apps()
      Mix.Tasks.App.Tree.run(["--format", "pretty"])

      assert_received {:mix_shell, :info, ["test"]}
      assert_received {:mix_shell, :info, ["├── app_deps_sample"]}
      assert_received {:mix_shell, :info, ["│   ├── app_deps2_sample"]}
      assert_received {:mix_shell, :info, ["│   │   └── app_deps4_sample (included)"]}
      assert_received {:mix_shell, :info, ["│   └── app_deps3_sample"]}
      assert_received {:mix_shell, :info, ["├── elixir"]}
      assert_received {:mix_shell, :info, ["└── logger"]}
      assert_received {:mix_shell, :info, ["    └── elixir"]}
    end)
  end

  @tag apps: [:foo, :bar]
  test "show the application tree for umbrella apps" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        Mix.Task.run("app.tree", ["--format", "pretty"])
        assert_received {:mix_shell, :info, ["├── elixir"]}
        assert_received {:mix_shell, :info, ["foo"]}
        assert_received {:mix_shell, :info, ["    └── elixir"]}
      end)
    end)
  end

  @tag apps: [:test, :app_deps_sample, :app_deps2_sample, :app_deps3_sample, :app_deps4_sample]
  test "shows the given application tree", context do
    Mix.Project.push(AppDepsSample)

    in_tmp(context.test, fn ->
      assert_raise Mix.Error, "could not find application app_deps_sample", fn ->
        Mix.Tasks.App.Tree.run(["--format", "pretty", "app_deps_sample"])
      end

      load_apps()
      Mix.Tasks.App.Tree.run(["--format", "pretty", "app_deps_sample"])

      assert_received {:mix_shell, :info, ["app_deps_sample"]}
      assert_received {:mix_shell, :info, ["├── app_deps2_sample"]}
      assert_received {:mix_shell, :info, ["│   └── app_deps4_sample (included)"]}
      assert_received {:mix_shell, :info, ["└── app_deps3_sample"]}
    end)
  end

  @tag apps: [:test, :app_deps_sample, :app_deps2_sample, :app_deps3_sample, :app_deps4_sample]
  test "shows the application dependency tree excluding applications", context do
    Mix.Project.push(AppDepsSample)

    in_tmp(context.test, fn ->
      load_apps()

      exclude = ["--exclude", "app_deps4_sample", "--exclude", "app_deps3_sample"]
      Mix.Tasks.App.Tree.run(["--format", "pretty" | exclude])

      assert_received {:mix_shell, :info, ["test"]}
      assert_received {:mix_shell, :info, ["├── app_deps_sample"]}
      assert_received {:mix_shell, :info, ["│   └── app_deps2_sample"]}
      assert_received {:mix_shell, :info, ["├── elixir"]}
      assert_received {:mix_shell, :info, ["└── logger"]}
      assert_received {:mix_shell, :info, ["    └── elixir"]}
    end)
  end

  @tag apps: [:test, :app_deps_sample, :app_deps2_sample, :app_deps3_sample, :app_deps4_sample]
  test "shows the application tree in dot form", context do
    Mix.Project.push(AppDepsSample)

    in_tmp(context.test, fn ->
      load_apps()
      Mix.Tasks.App.Tree.run(["--format", "dot"])

      assert File.read!("app_tree.dot") == """
             digraph "application tree" {
               "test"
               "test" -> "app_deps_sample"
               "app_deps_sample" -> "app_deps2_sample"
               "app_deps2_sample" -> "app_deps4_sample" [label="(included)"]
               "app_deps_sample" -> "app_deps3_sample"
               "test" -> "elixir"
               "test" -> "logger"
               "logger" -> "elixir"
             }
             """
    end)
  end

  def load_apps() do
    :ok = :application.load({:application, :app_deps4_sample, [vsn: '1.0.0', env: []]})
    :ok = :application.load({:application, :app_deps3_sample, [vsn: '1.0.0', env: []]})

    opts = [vsn: '1.0.0', env: [], included_applications: [:app_deps4_sample]]
    :ok = :application.load({:application, :app_deps2_sample, opts})

    opts = [vsn: '1.0.0', env: [], applications: [:app_deps2_sample, :app_deps3_sample]]
    :ok = :application.load({:application, :app_deps_sample, opts})
  end
end
