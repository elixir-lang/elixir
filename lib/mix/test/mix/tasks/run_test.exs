Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.RunTest do
  use MixTest.Case

  defmodule SimpleApp do
    def project do
      [ app: :simple_app,
        version: "0.1.0" ]
    end
  end

  defmodule GetApp do
    def project do
      [ app: :get_app,
        version: "0.1.0",
        deps: [
          { :git_repo, "0.1.0", git: MixTest.Case.fixture_path("git_repo") }
        ] ]
    end
  end

  test "run command with dependencies" do
    Mix.Project.push GetApp

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Deps.Get.run []
      Mix.Tasks.Run.run ["-e", "Mix.shell.info GitRepo.hello"]
      assert_received { :mix_shell, :info, ["World"] }
    end
  after
    purge [GitRepo, GitRepo.Mix]
    Mix.Project.pop
  end

  test "run requires before commands" do
    Mix.Project.push SimpleApp
    git_repo = fixture_path("git_repo/lib/git_repo.ex")

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Run.run ["-r", git_repo, "-e", "Mix.shell.info GitRepo.hello"]
      assert_received { :mix_shell, :info, ["World"] }

      Mix.Tasks.Run.run ["-pr", git_repo, "-e", "Mix.shell.info GitRepo.hello"]
      assert_received { :mix_shell, :info, ["World"] }
    end
  after
    Mix.Project.pop
  end
end
