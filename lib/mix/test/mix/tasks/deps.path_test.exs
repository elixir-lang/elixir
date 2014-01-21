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
      assert_received { :mix_shell, :info, ["* Updating raw_repo (custom/raw_repo)"] }
      assert_received { :mix_shell, :info, ["Compiled lib/raw_repo.ex"] }
      assert_received { :mix_shell, :info, ["Generated raw_repo.app"] }
      assert File.exists?("_build/shared/lib/raw_repo/ebin/Elixir.RawRepo.beam")
    end
  end

  test "runs even if lock does not match" do
    Mix.Project.push DepsApp

    in_fixture "deps_status", fn ->
      Mix.Deps.Lock.write [raw_repo: "abcdef"]
      Mix.Tasks.Deps.Compile.run ["raw_repo"]
      Mix.Tasks.Run.run ["-e", "Mix.shell.info RawRepo.hello"]
      assert_received { :mix_shell, :info, ["world"] }
    end
  end

  defmodule InvalidPathDepsApp do
    def project do
      [
        app: :rebar_as_dep,
        version: "0.1.0",
        deps: [{ :rebar_dep, path: MixTest.Case.tmp_path("rebar_dep") }]
      ]
    end
  end

  test "raises on non-mix path deps" do
    Mix.Project.push InvalidPathDepsApp
    assert_raise Mix.Error, %r/:path option can only be used with mix projects/, fn ->
      Mix.Tasks.Deps.Get.run []
    end
  end
end
