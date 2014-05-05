Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.RunTest do
  use MixTest.Case

  defmodule GetApp do
    def project do
      [ app: :get_app,
        version: "0.1.0",
        deps: [
          {:git_repo, "0.1.0", git: MixTest.Case.fixture_path("git_repo")}
        ] ]
    end
  end

  test "run requires before commands" do
    Mix.Project.push MixTest.Case.Sample
    git_repo = fixture_path("git_repo/lib/git_repo.ex")

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Run.run ["-r", git_repo, "-e", "Mix.shell.info GitRepo.hello"]
      assert_received {:mix_shell, :info, ["World"]}

      Mix.Tasks.Run.run ["-pr", git_repo, "-e", "Mix.shell.info GitRepo.hello"]
      assert_received {:mix_shell, :info, ["World"]}
    end
  after
    purge [GitRepo]
  end
end
