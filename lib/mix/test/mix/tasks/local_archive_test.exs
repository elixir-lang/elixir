Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.LocalArchiveTest do
  use MixTest.Case

  setup_all do
    File.rm_rf! tmp_path("userhome")
    System.put_env "MIX_HOME", tmp_path("userhome/.mix")
  end

  test "archive" do
    in_fixture "archive", fn() ->
      # Install it!
      Mix.Tasks.Archive.run ["--no_compile"]
      assert File.regular? "test archive.ez"

      self <- { :mix_shell_input, :yes?, true }
      Mix.Tasks.Local.Install.run ["test archive.ez"]
      assert File.regular? tmp_path("userhome/.mix/tasks/test archive.ez")

      # List it!
      Mix.Local.append_tasks
      Mix.Tasks.Local.run []
      assert_received { :mix_shell, :info, ["mix local.sample # A local install sample"] }

      # Run it!
      Mix.Task.run "local.sample"
      assert_received { :mix_shell, :info, ["sample"] }

      # Try to remove it by task name!
      assert_raise Mix.Error, %r"The task local.sample is part of archive test archive.ez", fn ->
        Mix.Tasks.Local.Uninstall.run ["local.sample"]
      end
      assert File.regular? tmp_path("userhome/.mix/tasks/test archive.ez")

      # Remove it for real!
      Mix.Tasks.Local.Uninstall.run ["test archive.ez"]
      refute File.regular? tmp_path("userhome/.mix/tasks/test archive.ez")

      # Cleanup
      File.rm "test archive.ez"
    end
  end
end
