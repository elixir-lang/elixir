Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.App.TreeTest do
  use MixTest.Case

  defmodule AppDepsSample do
    def project do
      [app: :test, version: "0.1.0", start_permanent: true]
    end

    def application do
      [applications: [:logger, :app_deps_sample]]
    end
  end

  @tag apps: [:test, :app_deps_sample, :app_deps2_sample, :app_deps3_sample, :app_deps4_sample]
  test "shows the application tree", context do
    Mix.Project.push AppDepsSample

    in_tmp context.test, fn ->
      load_apps()
      Mix.Tasks.App.Tree.run(["--pretty"])

      assert_received {:mix_shell, :info, ["test"]}
      assert_received {:mix_shell, :info, ["└── app_deps_sample"]}
      assert_received {:mix_shell, :info, ["    ├── app_deps2_sample"]}
      assert_received {:mix_shell, :info, ["    │   └── app_deps4_sample (included)"]}
      assert_received {:mix_shell, :info, ["    └── app_deps3_sample"]}
    end
  end

  @tag apps: [:foo, :bar]
  test "show the application tree for umbrella apps" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        Mix.Task.run "app.tree", ["--pretty"]
        assert_received {:mix_shell, :info, ["foo"]}
        assert_received {:mix_shell, :info, ["└── elixir"]}
        assert_received {:mix_shell, :info, ["bar"]}
        assert_received {:mix_shell, :info, ["└── elixir"]}
      end)
    end
  end

  @tag apps: [:test, :app_deps_sample, :app_deps2_sample, :app_deps3_sample, :app_deps4_sample]
  test "shows the given application tree", context do
    Mix.Project.push AppDepsSample

    in_tmp context.test, fn ->
      assert_raise Mix.Error, "could not find application app_deps_sample", fn ->
        Mix.Tasks.App.Tree.run(["--pretty", "app_deps_sample"])
      end

      load_apps()
      Mix.Tasks.App.Tree.run(["--pretty", "app_deps_sample"])

      assert_received {:mix_shell, :info, ["app_deps_sample"]}
      assert_received {:mix_shell, :info, ["├── app_deps2_sample"]}
      assert_received {:mix_shell, :info, ["│   └── app_deps4_sample (included)"]}
      assert_received {:mix_shell, :info, ["└── app_deps3_sample"]}
    end
  end

  @tag apps: [:test, :app_deps_sample, :app_deps2_sample, :app_deps3_sample, :app_deps4_sample]
  test "shows the application dependency tree excluding applications", context do
    Mix.Project.push AppDepsSample

    in_tmp context.test, fn ->
      load_apps()
      Mix.Tasks.App.Tree.run(["--pretty", "--exclude", "app_deps4_sample", "--exclude", "app_deps3_sample"])

      assert_received {:mix_shell, :info, ["test"]}
      assert_received {:mix_shell, :info, ["└── app_deps_sample"]}
      assert_received {:mix_shell, :info, ["    └── app_deps2_sample"]}
      refute_received {:mix_shell, :info, ["    │   └── app_deps4_sample (included)"]}
      refute_received {:mix_shell, :info, ["    └── app_deps3_sample"]}
    end
  end

  @tag apps: [:test, :app_deps_sample, :app_deps2_sample, :app_deps3_sample, :app_deps4_sample]
  test "shows the application tree in dot form", context do
    Mix.Project.push AppDepsSample

    in_tmp context.test, fn ->
      load_apps()
      Mix.Tasks.App.Tree.run(["--dot"])

      # Why is this generated when it doesn't appear in other tests?
      assert_received {:mix_shell, :info, ["Generated test app"]}

      assert_received {:mix_shell, :info, ["digraph \"application tree\" {"]}
      assert_received {:mix_shell, :info, ["  \"test\" -> \"app_deps_sample\""]}
      assert_received {:mix_shell, :info, ["  \"app_deps_sample\" -> \"app_deps2_sample\""]}
      assert_received {:mix_shell, :info, ["  \"app_deps2_sample\" -> \"app_deps4_sample (included)\""]}
      assert_received {:mix_shell, :info, ["  \"app_deps_sample\" -> \"app_deps3_sample\""]}
      assert_received {:mix_shell, :info, ["}"]}
    end
  end

  def load_apps() do
    :ok = :application.load({:application, :app_deps4_sample, [vsn: '1.0.0', env: []]})
    :ok = :application.load({:application, :app_deps3_sample, [vsn: '1.0.0', env: []]})
    :ok = :application.load({:application, :app_deps2_sample, [vsn: '1.0.0', env: [], included_applications: [:app_deps4_sample]]})
    :ok = :application.load({:application, :app_deps_sample, [vsn: '1.0.0', env: [], applications: [:app_deps2_sample, :app_deps3_sample]]})
  end
end
