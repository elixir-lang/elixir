Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.ArchiveTest do
  use MixTest.Case

  defmodule ArchiveProject do
    def project do
      [app: :archive, version: "0.1.0", elixir: "~> 0.1.0"]
    end
  end

  defmodule ArchiveProject2 do
    def project do
      [app: :archive, version: "0.2.0"]
    end
  end

  setup do
    File.rm_rf! tmp_path("userhome")
    System.put_env "MIX_ARCHIVES", tmp_path("userhome/.mix/archives/")
    Mix.Project.push(ArchiveProject)
    :ok
  end

  test "archive" do
    in_fixture "archive", fn() ->
      # Build and install archive
      Mix.Tasks.Archive.Build.run ["--no-elixir-version-check"]
      assert File.regular? "archive-0.1.0.ez"
      assert_received {:mix_shell, :info, ["Generated archive \"archive-0.1.0.ez\" with MIX_ENV=dev"]}

      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Archive.Install.run []
      assert File.regular? tmp_path("userhome/.mix/archives/archive-0.1.0.ez")

      # Check that the version warning is printed after installation
      version_error = "warning: the archive archive-0.1.0 requires Elixir \"~> 0.1.0\" but you are running on v#{System.version}"
      assert_received {:mix_shell, :error, [^version_error]}

      archive = tmp_path("userhome/.mix/archives/archive-0.1.0.ez/archive-0.1.0/ebin")
      assert to_char_list(archive) in :code.get_path

      # Loading the archive should emit warning again
      Mix.Local.append_archives
      assert_received {:mix_shell, :error, [^version_error]}

      # List archive
      Mix.Tasks.Local.run []
      info  = "mix local.sample # A local install sample"
      assert_received {:mix_shell, :info, [^info]}

      Mix.Tasks.Archive.run []
      assert_received {:mix_shell, :info, ["* archive-0.1.0.ez"]}

      # Run archived task
      Mix.Task.run "local.sample"
      assert_received {:mix_shell, :info, ["sample"]}
    end
  end

  test "archive update" do
    in_fixture "archive", fn() ->
      # Install previous version
      Mix.Tasks.Archive.Build.run ["--no-elixir-version-check"]
      assert File.regular? "archive-0.1.0.ez"
      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Archive.Install.run []
      assert_received {:mix_shell, :error, [_]}

      # Build new version
      Mix.Project.push(ArchiveProject2)
      Mix.Tasks.Archive.Build.run ["--no-compile"]
      assert File.regular? "archive-0.2.0.ez"
      assert_received {:mix_shell, :info, ["Generated archive \"archive-0.2.0.ez\" with MIX_ENV=dev"]}

      # Install new version
      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Archive.Install.run []
      assert File.regular? tmp_path("userhome/.mix/archives/archive-0.2.0.ez")

      # Re-install current version should not change system
      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Archive.Install.run []
      assert File.regular? tmp_path("userhome/.mix/archives/archive-0.2.0.ez")

      # Try to install a missing version does not remove archive
      assert_raise Mix.Error, fn ->
        send self, {:mix_shell_input, :yes?, true}
        Mix.Tasks.Archive.Install.run ["./archive-0.0.0.ez"]
      end

      assert File.regular? tmp_path("userhome/.mix/archives/archive-0.2.0.ez")

      # We don't do the assertion below on Windows because
      # the archive is open by Erlang code server and the archive
      # is not effectively removed until the Erlang process exits.
      unless match? {:win32, _}, :os.type do
        refute File.regular? tmp_path("userhome/.mix/archives/archive-0.1.0.ez")
      end

      # Load archive without warnings because there is no :elixir requirement in mix.exs
      Mix.Local.append_archives
      refute_received {:mix_shell, :error, [_]}

      # Remove archive
      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Archive.Uninstall.run ["archive-0.2.0.ez"]

      # See reason for previous refutation.
      unless match? {:win32, _}, :os.type do
        refute File.regular? tmp_path("userhome/.mix/archives/archive-0.2.0.ez")
      end
    end
  end

  test "archive checksum" do
    in_fixture "archive", fn() ->
      Mix.Tasks.Archive.Build.run ["--no-elixir-version-check"]
      assert File.regular? "archive-0.1.0.ez"
      send self, {:mix_shell_input, :yes?, true}

      # Install with wrong checksum
      assert_raise Mix.Error, ~r"Data does not match the given sha512 checksum", fn ->
        send self, {:mix_shell_input, :yes?, true}
        Mix.Tasks.Archive.Install.run ["--sha512", "wrong"]
      end

      # Install with correct checksum
      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Archive.Install.run ["--sha512", sha512("archive-0.1.0.ez")]
      assert File.regular? tmp_path("userhome/.mix/archives/archive-0.1.0.ez")
    end
  end

  test "archive check" do
    # Install the archive
    in_fixture "archive", fn() ->
      Mix.Tasks.Archive.Build.run ["--no-elixir-version-check"]
      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Archive.Install.run []
    end

    assert_raise Mix.Error, ~r/Expected archive to be in the format/, fn ->
      archive_check [:archive]
    end

    assert_raise Mix.Error, ~r/Archive "archive" could not be found/, fn ->
      archive_check [{:archive, ">= 1.0.0"}]
    end

    # Load the archive
    Mix.Local.append_archives

    assert_raise Mix.Error, ~r/Archive \"archive-0.1.0\" does not match requirement >= 1.0.0/, fn ->
      archive_check [{:archive, ">= 1.0.0"}]
    end

    archive_check [{:archive, ">= 0.0.0"}]
  end

  defp archive_check(archives) do
    Mix.Project.pop
    Mix.ProjectStack.post_config archives: archives
    Mix.Project.push MixTest.Case.Sample
    Mix.Tasks.Archive.Check.run([])
  end

  defp sha512(file) do
    Base.encode16 :crypto.hash(:sha512, File.read!(file)), case: :lower
  end
end
