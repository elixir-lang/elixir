Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.WillRecompileTest do
  use MixTest.Case

  test "marks current project to recompile" do
    in_fixture("deps_status/deps/ok", fn ->
      Mix.Project.in_project(:ok, ".", fn _ ->
        refute File.exists?("_build/dev/lib/ok/.mix/compile.lock")
        Mix.Task.run("will_recompile")
        assert File.exists?("_build/dev/lib/ok/.mix/compile.lock")
      end)
    end)
  end

  test "marks all projects in umbrella to recompile" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        refute File.exists?("_build/dev/lib/foo/.mix/compile.lock")
        refute File.exists?("_build/dev/lib/bar/.mix/compile.lock")
        Mix.Task.run("will_recompile")
        assert File.exists?("_build/dev/lib/foo/.mix/compile.lock")
        assert File.exists?("_build/dev/lib/bar/.mix/compile.lock")
      end)
    end)
  end
end
