Code.require_file "../../../test_helper.exs", __FILE__

defmodule Mix.Tasks.DepsPathTest do
  use MixTest.Case

  defmodule DepsApp do
    def project do
      [
        app: :raw_sample,
        version: "0.1.0",
        deps: [
          { :raw_repo, "0.1.0", raw: "custom/raw_repo" }
        ]
      ]
    end
  end

  test "get, update and clean raw repos with compilation" do
    Mix.Project.push DepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Get.run []
      assert_received { :mix_shell, :info, ["* Compiling raw_repo"] }
      assert_received { :mix_shell, :info, ["Compiled lib/raw_repo.ex"] }
      assert_received { :mix_shell, :info, ["Generated raw_repo.app"] }
      assert File.exists?("custom/raw_repo/ebin/Elixir-RawRepo.beam")

      purge [RawRepo, RawRepo.Mix]
      Mix.Task.clear

      Mix.Tasks.Deps.Update.run []
      assert_received { :mix_shell, :info, ["* Updating raw_repo (0.1.0) [raw: \"custom/raw_repo\"]"] }

      Mix.Tasks.Deps.Clean.run []
      assert_received { :mix_shell, :info, ["* Cleaning raw_repo (0.1.0) [raw: \"custom/raw_repo\"]"] }
      assert_received { :mix_shell, :info, ["  custom/raw_repo is a raw dependency, it was not cleaned"] }
    end
  after
    purge [RawRepo, RawRepo.Mix]
    Mix.Project.pop
  end

  test "runs even if lock does not match" do
    Mix.Project.push DepsApp

    in_fixture "deps_status", fn ->
      Mix.Deps.Lock.write [raw_repo: "abcdef"]
      Mix.Tasks.Deps.Compile.run ["raw_repo"]
      Mix.Tasks.Run.run ["Mix.shell.info", "RawRepo.hello"]
      assert_received { :mix_shell, :info, ["world"] }
    end
  after
    purge [RawRepo, RawRepo.Mix]
    Mix.Project.pop
  end
end