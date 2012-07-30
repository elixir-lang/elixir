Code.require_file "../../../test_helper", __FILE__

defmodule Mix.Tasks.DepsPathTest do
  use MixTest.Case

  defmodule RawApp do
    def project do
      [
        deps: [
          { :raw_repo, "0.1.0", raw: "custom/raw_repo" }
        ]
      ]
    end
  end

  test "get, update and clean raw repos with compilation" do
    Mix.Project.push RawApp

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
end