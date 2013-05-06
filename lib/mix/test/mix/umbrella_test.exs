Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.UmbrellaTest do
  use MixTest.Case

  test "compile umbrella" do
    in_fixture "umbrella", fn ->
      Mix.Project.load_project(:umbrella)
      Mix.Task.run "compile"

      assert_received { :mix_shell, :info, ["==> bar"] }
      assert_received { :mix_shell, :info, ["Compiled lib/bar.ex"] }
      assert_received { :mix_shell, :info, ["Generated bar.app"] }
      assert_received { :mix_shell, :info, ["==> foo"] }
      assert_received { :mix_shell, :info, ["Compiled lib/foo.ex"] }
      assert_received { :mix_shell, :info, ["Generated foo.app"] }
    end
  after
    Mix.Project.pop
    purge [Umbrella.Mixfile]
  end
end
