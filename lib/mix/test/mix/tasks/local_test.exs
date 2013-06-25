Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.LocalTest do
  use MixTest.Case

  test "archive" do
    File.rm_rf! tmp_path("userhome")
    System.put_env "MIX_HOME", tmp_path("userhome/.mix")

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
     Mix.Project.push(ArchiveProject)

    in_fixture "archive", fn() ->
      # Install it!
      Mix.Tasks.Archive.run ["--no_compile"]
      assert File.regular? "archive-0.1.0.ez"

      self <- { :mix_shell_input, :yes?, true }
      Mix.Tasks.Local.Install.run []
      assert File.regular? tmp_path("userhome/.mix/archives/archive-0.1.0.ez")

      # List it!
      Mix.Local.append_archives
      Mix.Tasks.Local.run []
      assert_received { :mix_shell, :info, ["mix local.sample # A local install sample"] }

      # Run it!
      Mix.Task.run "local.sample"
      assert_received { :mix_shell, :info, ["sample"] }

      #Install new version!
      Mix.Project.push(ArchiveProject2)
      Mix.Tasks.Archive.run ["--no_compile"]
      assert File.regular? "archive-0.2.0.ez"

      self <- { :mix_shell_input, :yes?, true }
      self <- { :mix_shell_input, :yes?, true }
      Mix.Tasks.Local.Install.run []
      assert File.regular? tmp_path("userhome/.mix/archives/archive-0.2.0.ez")
      refute File.regular? tmp_path("userhome/.mix/archives/archive-0.1.0.ez")
      Mix.Local.append_archives

      # Remove it!
      self <- { :mix_shell_input, :yes?, true }
      Mix.Tasks.Local.Uninstall.run ["local.sample"]
      refute File.regular? tmp_path("userhome/.mix/archives/archive-0.2.0.ez")
      Mix.Project.pop
    end
     Mix.Project.pop
  end

  test "MIX_PATH" do
    File.rm_rf! tmp_path("mixpath")
    System.put_env "MIX_PATH", tmp_path("mixpath/ebin")

    File.mkdir_p! tmp_path("mixpath/ebin")
    Mix.Local.append_paths

    # Install on MIX_PATH manually
    File.copy! fixture_path("beams/Elixir.Mix.Tasks.Local.Sample.beam"),
               tmp_path("mixpath/ebin/Elixir.Mix.Tasks.Local.Sample.beam")

    # Run it
    Mix.Task.run "local.sample"
    assert_received { :mix_shell, :info, ["sample"] }
  end
end
