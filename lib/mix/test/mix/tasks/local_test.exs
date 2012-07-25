Code.require_file "../../../test_helper", __FILE__

defmodule Mix.Tasks.LocalTest do
  use MixTest.Case

  test "manage local tasks" do
    File.rm_rf! tmp_path("userhome")
    System.put_env "MIXHOME", tmp_path("userhome")

    # Install it!
    self <- { :mix_shell_input, :yes?, true }
    Mix.Tasks.Local.Install.run [fixture_path("beams/Elixir-Mix-Tasks-Local-Sample.beam")]
    assert File.regular? tmp_path("userhome/.mix/tasks/Elixir-Mix-Tasks-Local-Sample.beam")

    # List it!
    Mix.append_local_tasks_path!
    Mix.Tasks.Local.run []
    assert_received { :mix_shell, :info, ["mix local.sample # A local install sample"] }

    # Run it!
    Mix.Task.run "local.sample"
    assert_received { :mix_shell, :info, ["sample"] }

    # Remove it!
    Mix.Tasks.Local.Uninstall.run ["local.sample"]
    refute File.regular? tmp_path("userhome/.mix/tasks/Elixir-Mix-Tasks-Local-Sample.beam")
  end
end