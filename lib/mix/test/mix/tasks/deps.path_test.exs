Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.DepsPathTest do
  use MixTest.Case

  defmodule DepsApp do
    def project do
      [
        app: :raw_sample,
        version: "0.1.0",
        deps: [
          { :raw_repo, "0.1.0", path: "custom/raw_repo" }
        ]
      ]
    end
  end

  test "updates and cleans path repos with compilation" do
    Mix.Project.push DepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Update.run ["--all"]
      assert_received { :mix_shell, :info, ["* Updating raw_repo [path: \"custom/raw_repo\"]"] }
      assert_received { :mix_shell, :info, ["Compiled lib/raw_repo.ex"] }
      assert_received { :mix_shell, :info, ["Generated raw_repo.app"] }
      assert File.exists?("custom/raw_repo/ebin/Elixir.RawRepo.beam")

      Mix.Tasks.Deps.Clean.run ["--all"]
      assert_received { :mix_shell, :info, ["* Cleaning raw_repo (0.1.0) [path: \"custom/raw_repo\"]"] }
      assert_received { :mix_shell, :info, ["  custom/raw_repo is a path dependency, it was not cleaned"] }
    end
  after
    Mix.Project.pop
  end

  test "runs even if lock does not match" do
    Mix.Project.push DepsApp

    in_fixture "deps_status", fn ->
      Mix.Deps.Lock.write [raw_repo: "abcdef"]
      Mix.Tasks.Deps.Compile.run ["raw_repo"]
      Mix.Tasks.Run.run ["-e", "Mix.shell.info RawRepo.hello"]
      assert_received { :mix_shell, :info, ["world"] }
    end
  after
    Mix.Project.pop
  end
end