Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.HelpTest do
  use MixTest.Case

  test "help lists all tasks" do
    in_fixture "only_mixfile", fn ->
      Mix.Tasks.Help.run []
      assert_received { :mix_shell, :info, ["mix" <> _] }
      assert_received { :mix_shell, :info, ["mix help" <> _] }
      assert_received { :mix_shell, :info, ["mix compile" <> _] }
    end
  end

  test "help list default task" do
    in_fixture "only_mixfile", fn ->
      Mix.Tasks.Help.run []

      { _, _, [output] } =
        assert_received { :mix_shell, :info, [_] }
      assert output =~ %r/^mix\s+# Run the default task \(current: mix run\)/m
    end
  end

  test "help TASK" do
    in_fixture "only_mixfile", fn ->
      Mix.Tasks.Help.run ["compile"]

      { _, _, [output] } =
        assert_received { :mix_shell, :info, [_] }
      assert output =~ "# mix help compile"

      { _, _, [output] } =
        assert_received { :mix_shell, :info, [_] }
      assert output =~ "## Command line options"

      { _, _, [output] } =
        assert_received { :mix_shell, :info, [_] }
      assert output =~ %r/^Location:/m
    end
  end
end
