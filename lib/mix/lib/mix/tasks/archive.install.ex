defmodule Mix.Tasks.Archive.Install do
  use Mix.Task

  @shortdoc "Installs an archive locally"

  @moduledoc """
  Installs an archive locally.

  If no argument is supplied but there is an archive in the project's root directory
  (created with `mix archive.build`), then the archive will be installed
  locally. For example:

      mix do archive.build, archive.install

  The argument can be an archive located at some URL:

      mix archive.install http://example.com/foo.ez

  After installation, the tasks in the archive are available locally:

      mix some_task

  ## Command line options

    * `--sha512` - checks the archive matches the given sha512 checksum

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like `make`

  """
  @switches [force: :boolean, sha512: :string]
  @spec run(OptionParser.argv) :: boolean
  def run(argv) do
    {opts, argv, _} = OptionParser.parse(argv, switches: @switches)

    if src = List.first(argv) do
      case Path.extname(Mix.Local.Utils.basename(src)) do
        ".ez" -> install_archive(src, opts)
        _     -> Mix.raise "\"mix archive.install\" doesn't know how to install #{inspect src}"
      end
    else
      src = Mix.Archive.name(Mix.Project.config[:app], Mix.Project.config[:version])

      if File.exists?(src) do
        install_archive(src, opts)
      else
        Mix.raise "Expected local archive to exist or PATH to be given, " <>
                  "please use \"mix archive.install PATH\""
      end
    end
  end

  defp install_archive(src, opts) do
    previous = previous_versions(src)

    if opts[:force] || should_install?(src, previous) do
      dirname = Mix.Local.archives_path
      archive = Path.join(dirname, Mix.Local.Utils.basename(src))
      check_file_exists!(src, archive)

      case Mix.Utils.read_path(src, opts) do
        {:ok, binary} ->
          File.mkdir_p!(dirname)
          File.write!(archive, binary)
        :badpath ->
          Mix.raise "Expected #{inspect src} to be a URL or a local file path"
        {:local, message} ->
          Mix.raise message
        {kind, message} when kind in [:remote, :checksum] ->
          Mix.raise """
          #{message}

          Could not fetch archive at:

              #{src}

          Please download the archive above manually to your current directory and run:

              mix archive.install ./#{Path.basename(archive)}
          """
      end

      Mix.shell.info [:green, "* creating ", :reset, Path.relative_to_cwd(archive)]
      Mix.Local.check_archive_elixir_version archive
      unless archive in previous, do: remove_previous_versions(previous)
      true = Code.append_path(Mix.Archive.ebin(archive))
    else
      false
    end
  end

  defp should_install?(src, []) do
    Mix.shell.yes?("Are you sure you want to install archive #{inspect src}?")
  end

  defp should_install?(_src, previous_files) do
    files = Enum.map_join(previous_files, ", ", &Path.basename/1)

    Mix.shell.yes?("Found existing archive(s): #{files}.\n" <>
                   "Are you sure you want to replace them?")
  end

  defp check_file_exists!(src, path) do
    # OTP keeps loaded archives open, this leads to unfortunate behaviour on
    # Windows when trying overwrite loaded archives. remove_previous_versions
    # completes successfully even though the file will be first removed after
    # the BEAM process is dead. Because of this we ask the user rerun the
    # command, which should complete successfully at that time

    if File.exists?(path) and match?({:win32, _}, :os.type) do
      Mix.raise "Unable to overwrite open archives on Windows. Please manually remove " <>
                "the existing archive at #{inspect path} and run this command again. In " <>
                "case re-running the command still does not work, please fetch the archive " <>
                "at #{inspect src} and manually copy it to #{inspect path}."
    end
  end

  defp previous_versions(src) do
    app = src
          |> Mix.Archive.dir
          |> String.split("-")
          |> List.first

    if app do
      Mix.Local.archive_files(app)
    else
      []
    end
  end

  defp remove_previous_versions([]),
    do: :ok
  defp remove_previous_versions(previous),
    do: Enum.each(previous, &File.rm!/1)
end
