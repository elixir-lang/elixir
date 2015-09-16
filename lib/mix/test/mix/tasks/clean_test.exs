Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.CleanTest do
  use MixTest.Case

  defmodule Sample do
    def project do
      [
        app: :sample,
        version: "0.1.0",
        deps: [
          {:ok, "0.1.0", path: "deps/ok"},
          {:unknown, "0.1.0", git: "deps/unknown"}
        ]
      ]
    end
  end

  setup do
    Mix.Project.push Sample
    :ok
  end

  test "cleans the application build" do
    in_fixture "deps_status", fn ->
      File.mkdir_p! "_build/dev/consolidated"
      File.mkdir_p! "_build/dev/lib/sample"
      File.mkdir_p! "_build/test/lib/sample"
      File.mkdir_p! "_build/dev/lib/ok"

      Mix.Tasks.Clean.run []
      refute File.exists?("_build/dev/consolidated")
      refute File.exists?("_build/dev/lib/sample")
      refute File.exists?("_build/test/lib/sample")
      assert File.exists?("_build/dev/lib/ok")
    end
  end

  test "cleans dependencies build" do
    in_fixture "deps_status", fn ->
      File.mkdir_p! "_build/dev/lib/ok"
      File.mkdir_p! "_build/test/lib/ok"

      Mix.Tasks.Clean.run ["--deps", "--only", "dev"]
      refute File.exists?("_build/dev")
      assert File.exists?("_build/test")

      Mix.Tasks.Clean.run ["--deps"]
      refute File.exists?("_build/test")
    end
  end
end
