Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.ReleaseTest do
  use MixTest.Case

  test "ships a bootable release with ERTS" do
    in_fixture("release_test", fn ->
      Mix.Project.in_project(:release_test, ".", fn _ ->
        Mix.Task.run("release")

        assert System.cmd(Path.absname("_build/dev/rel/release_test/bin/start"), []) == {"", 0}
      end)
    end)
  end
end
