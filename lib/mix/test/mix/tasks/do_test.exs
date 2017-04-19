Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.DoTest do
  use MixTest.Case

  test "runs given tasks", context do
    in_tmp context.test, fn ->
      Mix.Tasks.Do.run ["compile", "--list,", "help,", "help"]
      # Allow running a task multiple times within one "do"
      assert_received {:mix_shell, :info, ["mix help" <> _]}
      assert_received {:mix_shell, :info, ["mix help" <> _]}
      assert_received {:mix_shell, :info, ["mix compile.app" <> _]}
    end
  end

  test "gather_command returns a list of commands" do
    import Mix.Tasks.Do, only: [gather_commands: 1]
    assert gather_commands(["compile", "--list,", "help"]) == [["compile", "--list"], ["help"]]
    assert gather_commands(["help,", "compile", "--list"]) == [["help"], ["compile", "--list"]]
    assert gather_commands(["compile,", "run", "-e", "IO.puts :hello"]) == [["compile"], ["run", "-e", "IO.puts :hello"]]
    assert gather_commands(["compile,", "run", "-e", "[1, 2]"]) == [["compile"], ["run", "-e", "[1, 2]"]]
    assert gather_commands(["test", ",", "help"]) == [["test"], ["help"]]
  end
end
