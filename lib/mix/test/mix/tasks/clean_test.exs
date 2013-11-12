Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.CleanTest do
  use MixTest.Case

  defmodule Sample do
    def project do
      [
        app: :sample,
        version: "0.1.0",
        deps: [
          { :ok, "0.1.0", path: "deps/ok" },
          { :unknown, "0.1.0", git: "deps/unknown" }
        ]
      ]
    end
  end

  setup do
    Mix.Project.push Sample
    :ok
  end

  teardown do
    Mix.Project.pop
    :ok
  end

  test "removes the build application" do
    in_fixture "deps_status", fn ->
      Mix.Tasks.Compile.run ["--no-deps"]
      assert File.exists?("_build/shared/lib/sample")

      Mix.Tasks.Clean.run []
      refute File.exists?("_build/shared/lib/sample")
    end
  end

  test "cleans deps" do
    in_fixture "deps_status", fn ->
      assert File.exists?("_build/shared/lib/ok")
      Mix.Tasks.Deps.Clean.run ["--all"]

      assert File.exists?("_build/shared")
      refute File.exists?("_build/shared/lib/ok")
      assert_received { :mix_shell, :info, ["* Cleaning ok"] }

      # Assert we don't choke on unfetched deps
      assert_received { :mix_shell, :info, ["* Cleaning unknown"] }
    end
  end

  test "cleans all deps and builds" do
    in_fixture "deps_status", fn ->
      assert File.exists?("_build/shared/lib/ok")
      Mix.Tasks.Clean.run ["--all"]

      assert File.exists?("_build/shared")
      refute File.exists?("_build/shared/lib")
      assert_received { :mix_shell, :info, ["* Cleaning ok"] }
      assert_received { :mix_shell, :info, ["* Cleaning unknown"] }
    end
  end
end
