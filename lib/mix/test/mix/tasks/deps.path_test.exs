Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.DepsPathTest do
  use MixTest.Case

  defmodule DepsApp do
    def project do
      [
        app: :raw_sample,
        version: "0.1.0",
        deps: [
          {:raw_repo, "0.1.0", path: "custom/raw_repo"}
        ]
      ]
    end
  end

  defmodule MismatchDepsApp do
    def project do
      [
        app: :raw_sample,
        version: "0.1.0",
        deps: [
          {:cooked_repo, "0.1.0", path: "custom/raw_repo"}
        ]
      ]
    end
  end

  @tag apps: [:raw_sample]
  test "does not mark for compilation on get/update" do
    Mix.Project.push DepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Get.run ["--all"]
      refute File.exists?("custom/raw_repo/.fetch")
    end
  end

  @tag apps: [:raw_sample]
  test "compiles ands runs even if lock does not match" do
    Mix.Project.push DepsApp

    in_fixture "deps_status", fn ->
      Mix.Dep.Lock.write %{raw_repo: "abcdef"}
      Mix.Tasks.Run.run ["-e", "Mix.shell.info RawRepo.hello"]
      assert_received {:mix_shell, :info, ["==> raw_repo"]}
      assert_received {:mix_shell, :info, ["world"]}
    end
  end

  @tag apps: [:raw_sample]
  test "uses the name of the app, not the path basename" do
    Mix.Project.push MismatchDepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.Compile.run []
      assert File.exists?("_build/dev/lib/cooked_repo/ebin")
    end
  end
end
