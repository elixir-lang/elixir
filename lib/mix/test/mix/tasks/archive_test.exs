Code.require_file("../../test_helper.exs", __DIR__)

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
    File.rm_rf!(tmp_path("userhome"))
    System.put_env("MIX_ARCHIVES", tmp_path("userhome/.mix/archives/"))
    Mix.Project.push(ArchiveProject)
    :ok
  end

  test "archive build" do
    in_fixture("archive", fn ->
      Mix.Tasks.Archive.Build.run(["--no-elixir-version-check"])
      assert_archive_content_default()
      refute has_zip_file?('archive-0.1.0.ez', 'archive-0.1.0/priv/.dot_file')
    end)
  end

  test "archive build with include-dot-files" do
    in_fixture("archive", fn ->
      Mix.Tasks.Archive.Build.run(["--no-elixir-version-check", "--include-dot-files"])
      assert_archive_content_default()
      assert has_zip_file?('archive-0.1.0.ez', 'archive-0.1.0/priv/.dot_file')
    end)
  end

  def assert_archive_content_default() do
    assert File.regular?('archive-0.1.0.ez')
    assert has_zip_file?('archive-0.1.0.ez', 'archive-0.1.0/.elixir')
    assert has_zip_file?('archive-0.1.0.ez', 'archive-0.1.0/priv/not_really_an.so')

    assert has_zip_file?(
             'archive-0.1.0.ez',
             'archive-0.1.0/ebin/Elixir.Mix.Tasks.Local.Sample.beam'
           )

    assert has_zip_file?('archive-0.1.0.ez', 'archive-0.1.0/ebin/archive.app')
  end

  test "archive install --force" do
    in_fixture("archive", fn ->
      Mix.Tasks.Archive.Build.run(["--no-elixir-version-check"])
      Mix.Tasks.Archive.Install.run(["--force"])

      message = "Generated archive \"archive-0.1.0.ez\" with MIX_ENV=dev"
      assert_received {:mix_shell, :info, [^message]}

      Mix.Tasks.Archive.Uninstall.run(["archive-0.1.0", "--force"])
      refute File.dir?(tmp_path("userhome/.mix/archives/archive-0.1.0/archive-0.1.0/ebin"))
    end)
  end

  test "archive install" do
    in_fixture("archive", fn ->
      # Build and install archive
      Mix.Tasks.Archive.Build.run(["--no-elixir-version-check"])

      message = "Generated archive \"archive-0.1.0.ez\" with MIX_ENV=dev"
      assert_received {:mix_shell, :info, [^message]}

      assert File.regular?('archive-0.1.0.ez')

      send(self(), {:mix_shell_input, :yes?, true})
      Mix.Tasks.Archive.Install.run([])
      refute File.regular?(tmp_path("userhome/.mix/archives/archive-0.1.0.ez"))
      assert File.dir?(tmp_path("userhome/.mix/archives/archive-0.1.0/archive-0.1.0/ebin"))

      # Check that the version warning is printed after installation
      version_error =
        "warning: the archive archive-0.1.0 requires Elixir \"~> 0.1.0\" " <>
          "but you are running on v#{System.version()}"

      assert_received {:mix_shell, :error, [^version_error]}

      archive = tmp_path("userhome/.mix/archives/archive-0.1.0/archive-0.1.0/ebin")
      assert to_charlist(archive) in :code.get_path()

      # Try to override it with URL
      send(self(), {:mix_shell_input, :yes?, false})
      Mix.Tasks.Archive.Install.run(["https://example.com/archive-0.1.0?hello.ez"])
      assert_received {:mix_shell, :yes?, ["Found existing entry: " <> _]}

      # Loading the archive should emit warning again
      Mix.Local.append_archives()
      assert_received {:mix_shell, :error, [^version_error]}

      # List archive
      Mix.Tasks.Local.run([])
      info = "mix local.sample # A local install sample"
      assert_received {:mix_shell, :info, [^info]}

      Mix.Tasks.Archive.run([])
      assert_received {:mix_shell, :info, ["* archive-0.1.0"]}

      # Run archived task
      Mix.Task.run("local.sample")
      assert_received {:mix_shell, :info, ["sample"]}
    end)
  end

  test "archive install invalid file" do
    in_fixture("archive", fn ->
      file_name = "invalid-archive-0.1.0.ez"
      assert File.regular?(file_name)

      send(self(), {:mix_shell_input, :yes?, true})

      assert_raise Mix.Error, ~r/invalid archive file/, fn ->
        Mix.Tasks.Archive.Install.run([file_name])
      end
    end)
  end

  test "archive install missing file" do
    message = ~r[Expected "./unlikely-to-exist-0.1.0.ez" to be a URL or a local file path]

    assert_raise Mix.Error, message, fn ->
      Mix.Tasks.Archive.Install.run(["./unlikely-to-exist-0.1.0.ez"])
    end
  end

  test "archive update" do
    in_fixture("archive", fn ->
      # Install previous version
      Mix.Tasks.Archive.Build.run(["--no-elixir-version-check"])
      assert File.regular?("archive-0.1.0.ez")
      send(self(), {:mix_shell_input, :yes?, true})
      Mix.Tasks.Archive.Install.run([])
      assert_received {:mix_shell, :error, [_]}

      # Build new version
      Mix.Project.push(ArchiveProject2)
      Mix.Tasks.Archive.Build.run(["--no-compile"])
      assert File.regular?("archive-0.2.0.ez")

      message = "Generated archive \"archive-0.2.0.ez\" with MIX_ENV=dev"
      assert_received {:mix_shell, :info, [^message]}

      # Install new version
      send(self(), {:mix_shell_input, :yes?, true})
      Mix.Tasks.Archive.Install.run([])
      refute File.regular?(tmp_path("userhome/.mix/archives/archive-0.2.0.ez"))
      assert File.dir?(tmp_path("userhome/.mix/archives/archive-0.2.0/archive-0.2.0/ebin"))

      # Re-install current version should not change system
      send(self(), {:mix_shell_input, :yes?, true})
      Mix.Tasks.Archive.Install.run([])
      refute File.regular?(tmp_path("userhome/.mix/archives/archive-0.2.0.ez"))
      assert File.dir?(tmp_path("userhome/.mix/archives/archive-0.2.0/archive-0.2.0/ebin"))

      # Try to install a missing version does not remove archive
      assert_raise Mix.Error, fn ->
        Mix.Tasks.Archive.Install.run(["./archive-0.0.0.ez"])
      end

      assert File.dir?(tmp_path("userhome/.mix/archives/archive-0.2.0/archive-0.2.0/ebin"))
      refute File.regular?(tmp_path("userhome/.mix/archives/archive-0.1.0.ez"))

      # Load archive without warnings because there is no :elixir requirement in mix.exs
      Mix.Local.append_archives()
      refute_received {:mix_shell, :error, [_]}

      # Check uninstall confirmation
      send(self(), {:mix_shell_input, :yes?, false})
      Mix.Tasks.Archive.Uninstall.run(["archive-0.2.0"])
      assert File.dir?(tmp_path("userhome/.mix/archives/archive-0.2.0/archive-0.2.0/ebin"))

      # Remove it!
      send(self(), {:mix_shell_input, :yes?, true})
      Mix.Tasks.Archive.Uninstall.run(["archive-0.2.0"])
      refute File.dir?(tmp_path("userhome/.mix/archives/archive-0.2.0/archive-0.2.0/ebin"))

      # Check old paths are unloaded
      paths = Enum.map(:code.get_path(), &List.to_string/1)
      refute tmp_path("userhome/.mix/archives/archive-0.1.0/archive-0.1.0/ebin") in paths
    end)
  end

  defp has_zip_file?(archive, name) do
    {:ok, files} = :zip.list_dir(archive)
    Enum.find(files, &match?({:zip_file, ^name, _, _, _, _}, &1))
  end

  test "archive checksum" do
    in_fixture("archive", fn ->
      Mix.Tasks.Archive.Build.run(["--no-elixir-version-check"])
      assert File.regular?("archive-0.1.0.ez")
      send(self(), {:mix_shell_input, :yes?, true})

      # Install with wrong checksum
      assert_raise Mix.Error, ~r"Data does not match the given SHA-512 checksum", fn ->
        send(self(), {:mix_shell_input, :yes?, true})
        Mix.Tasks.Archive.Install.run(["--sha512", "wrong"])
      end

      # Install with correct checksum
      send(self(), {:mix_shell_input, :yes?, true})
      Mix.Tasks.Archive.Install.run(["--sha512", sha512("archive-0.1.0.ez")])
      refute File.regular?(tmp_path("userhome/.mix/archives/archive-0.1.0.ez"))
      assert File.dir?(tmp_path("userhome/.mix/archives/archive-0.1.0/archive-0.1.0/ebin"))
    end)
  end

  test "archive check" do
    # Install the archive
    in_fixture("archive", fn ->
      Mix.Tasks.Archive.Build.run(["--no-elixir-version-check"])
      send(self(), {:mix_shell_input, :yes?, true})
      Mix.Tasks.Archive.Install.run([])
    end)

    assert_raise Mix.Error, ~r/Expected archive to be in the format/, fn ->
      archive_check([:archive])
    end

    assert_raise Mix.Error, ~r/Archive "archive" could not be found/, fn ->
      archive_check([{:archive, ">= 1.0.0"}])
    end

    # Load the archive
    Mix.Local.append_archives()

    message = ~r/Archive \"archive-0.1.0\" does not match requirement >= 1.0.0/

    assert_raise Mix.Error, message, fn ->
      archive_check([{:archive, ">= 1.0.0"}])
    end

    archive_check([{:archive, ">= 0.0.0"}])
  end

  defp archive_check(archives) do
    Mix.Project.pop()
    Mix.ProjectStack.post_config(archives: archives)
    Mix.Project.push(MixTest.Case.Sample)
    Mix.Tasks.Archive.Check.run([])
  end

  defp sha512(file) do
    Base.encode16(:crypto.hash(:sha512, File.read!(file)), case: :lower)
  end

  test "archive.install from Git" do
    in_fixture("git_repo", fn ->
      send(self(), {:mix_shell_input, :yes?, true})
      Mix.Tasks.Archive.Install.run(["git", File.cwd!()])

      message = "Generated archive \"git_repo-0.1.0.ez\" with MIX_ENV=prod"
      assert_received {:mix_shell, :info, [^message]}

      refute File.regular?(tmp_path("userhome/.mix/archives/git_repo-0.1.0.ez"))
      assert File.dir?(tmp_path("userhome/.mix/archives/git_repo-0.1.0/git_repo-0.1.0/ebin"))
    end)
  after
    purge([GitRepo, GitRepo.MixProject])
  end
end
