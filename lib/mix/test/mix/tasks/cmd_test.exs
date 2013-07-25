Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.CmdTest do
  use MixTest.Case

  test "runs the command for each app" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        Mix.Task.run "cmd", ["echo", "hello"]
        assert_received { :mix_shell, :info, ["==> bar"] }
        assert_received { :mix_shell, :run, ["hello\n"] }
        assert_received { :mix_shell, :info, ["==> foo"] }
        assert_received { :mix_shell, :run, ["hello\n"] }
      end)
    end
  end
end
