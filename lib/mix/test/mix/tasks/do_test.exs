Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.DoTest do
  use MixTest.Case

  test "runs given tasks", context do
    in_tmp context.test, fn ->
      Mix.Tasks.Do.run ["compile", "--list,", "help"]
      assert_received {:mix_shell, :info, ["mix help" <> _]}
      assert_received {:mix_shell, :info, ["mix compile.app" <> _]}
    end
  end
end