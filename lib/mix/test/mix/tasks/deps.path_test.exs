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

  test "marks for compilation across environments on get/update" do
    Mix.Project.push DepsApp

    in_fixture "deps_status", fn ->
      File.mkdir_p!("_build/dev/lib/raw_repo")
      File.mkdir_p!("_build/test/lib/raw_repo")

      Mix.Tasks.Deps.Get.run ["--all"]
      assert File.exists?("_build/dev/lib/raw_repo/.compile")
      assert File.exists?("_build/test/lib/raw_repo/.compile")

      Mix.Tasks.Run.run ["-e", "Mix.shell.info RawRepo.hello"]
      assert_received { :mix_shell, :info, ["* Compiling raw_repo"] }
      assert_received { :mix_shell, :info, ["world"] }
    end
  end

  test "compiles ands runs even if lock does not match" do
    Mix.Project.push DepsApp

    in_fixture "deps_status", fn ->
      Mix.Dep.Lock.write [raw_repo: "abcdef"]
      Mix.Tasks.Run.run ["-e", "Mix.shell.info RawRepo.hello"]
      assert_received { :mix_shell, :info, ["* Compiling raw_repo"] }
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
    assert_raise Mix.Error, ~r/:path option can only be used with mix projects/, fn ->
      Mix.Tasks.Deps.Get.run []
    end
  end
end
