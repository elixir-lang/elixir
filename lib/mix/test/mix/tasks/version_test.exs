Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.VersionTest do
  use MixTest.Case

  test "prints version string" do
    _sys_version = System.version()
    Mix.Task.run("version")
    assert_received {:mix_shell, :info, [_sys_version]}
  end
end
