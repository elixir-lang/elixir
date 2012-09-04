Code.require_file "../../../test_helper.exs", __FILE__

defmodule Mix.Tasks.RunTest do
  use MixTest.Case

  defmodule GetApp do
    def project do
      [
        app: :get_app,
        version: "0.1.0",
        deps: [
          { :git_repo, "0.1.0", git: MixTest.Case.fixture_path("git_repo") }
        ]
      ]
    end
  end

  defmodule CustomPrepareApp do
    def project do
      [
        app: :get_app,
        version: "0.1.0",
        prepare_task: "hello"
      ]
    end
  end

  test "run command with dependencies" do
    Mix.Project.push GetApp

    in_fixture "only_mixfile", fn ->
      Mix.Tasks.Deps.Get.run []
      Mix.Tasks.Run.run ["Mix.shell.info", "GitRepo.hello"]
      assert_received { :mix_shell, :info, ["World"] }
    end
  after
    purge [GitRepo, GitRepo.Mix]
    Mix.Project.pop
  end

  test "run command with custom prepare" do
    Mix.Project.push CustomPrepareApp

    in_fixture "only_mixfile", fn ->
      Mix.Tasks.Run.run ["Mix.shell.info", "Mix.Task.run(:hello) /> to_binary"]
      assert_received { :mix_shell, :info, ["noop"] }
    end
  after
    Mix.Project.pop
  end
end