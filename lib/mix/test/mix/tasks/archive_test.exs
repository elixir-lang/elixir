Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.ArchiveTest do
  use MixTest.Case

  defmodule ArchiveProject do
    def project do
      [ app: :archive, version: "0.1.0" ]
    end
  end

  defmodule ArchiveProject2 do
    def project do
      [ app: :archive, version: "0.2.0" ]
    end
  end

  test "archive" do
    File.rm_rf! tmp_path("userhome")
    System.put_env "MIX_HOME", tmp_path("userhome/.mix")
    Mix.Project.push(ArchiveProject)

    in_fixture "archive", fn() ->
      # Install it!
      Mix.Tasks.Archive.Build.run []
      assert File.regular? "archive-0.1.0.ez"

      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Archive.Install.run []
      assert File.regular? tmp_path("userhome/.mix/archives/archive-0.1.0.ez")

      archive = tmp_path("userhome/.mix/archives/archive-0.1.0.ez/archive-0.1.0/ebin")
      assert to_char_list(archive) in :code.get_path

      # List it!
      Mix.Local.append_archives
      Mix.Tasks.Local.run []
      assert_received {:mix_shell, :info, ["mix local.sample # A local install sample"]}

      Mix.Tasks.Archive.run []
      assert_received {:mix_shell, :info, ["* archive-0.1.0.ez"]}

      # Run it!
      Mix.Task.run "local.sample"
      assert_received {:mix_shell, :info, ["sample"]}

      # Install new version!
      Mix.Project.push(ArchiveProject2)
      Mix.Tasks.Archive.Build.run ["--no_compile"]
      assert File.regular? "archive-0.2.0.ez"

      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Archive.Install.run []
      assert File.regular? tmp_path("userhome/.mix/archives/archive-0.2.0.ez")

      # We don't do the assertion below on Windows because
      # the archive is open by Erlang code server and the archive
      # is not effectively removed until the Erlang process exits.
      unless match? {:win32, _}, :os.type do
        refute File.regular? tmp_path("userhome/.mix/archives/archive-0.1.0.ez")
      end

      Mix.Local.append_archives

      # Remove it!
      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Archive.Uninstall.run ["archive-0.2.0.ez"]
      
      # See reason for previous refutation.
      unless match? {:win32, _}, :os.type do
        refute File.regular? tmp_path("userhome/.mix/archives/archive-0.2.0.ez")
      end
    end
  end
end
