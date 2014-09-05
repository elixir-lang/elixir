defmodule Mix.Tasks.Archive.Install do
  use Mix.Task

  import Mix.Generator, only: [create_file: 2]

  @shortdoc "Install an archive locally"

  @moduledoc """
  Install an archive locally.

  If no argument is supplied but there is an archive in the root
  (created with mix archive), then the archive will be installed
  locally. For example:

      mix do archive.build, archive.install

  The argument can be an archive located at some URL:

      mix archive.install http://example.com/foo.ez

  After installed, the tasks in the archive are available locally:

      mix some_task

  ## Command line options

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like make

  """
  @spec run(OptionParser.argv) :: boolean
  def run(argv) do
    {opts, argv, _} = OptionParser.parse(argv, switches: [force: :boolean, shell: :boolean])

    if src = List.first(argv) do
      %URI{path: path} = URI.parse(src)

      case Path.extname(path) do
        ".ez" -> install_archive(src, opts)
        _     -> Mix.raise "mix archive.install doesn't know how to install #{path}"
      end
    else
      src = Mix.Archive.name(Mix.Project.config[:app], Mix.Project.config[:version])

      if File.exists?(src) do
        install_archive(src, opts)
      else
        Mix.raise "Expected PATH to be given, please use `mix archive.install PATH`"
      end
    end
  end

  defp install_archive(src, opts) do
    previous = previous_versions(src)

    if opts[:force] || should_install?(src, previous) do
      remove_previous_versions(previous)
      dest = Mix.Local.archives_path()
      File.mkdir_p!(dest)
      archive = Path.join(dest, basename(src))
      create_file archive, Mix.Utils.read_path!(src, opts)
      true = Code.append_path(Mix.Archive.ebin(archive))
    else
      false
    end
  end

  defp basename(path) do
    %URI{path: path} = URI.parse(path)
    Path.basename(path)
  end

  defp should_install?(src, []) do
    Mix.shell.yes?("Are you sure you want to install archive #{src}?")
  end

  defp should_install?(_src, previous_files) do
    files = Enum.map_join(previous_files, ", ", &Path.basename/1)

    Mix.shell.yes?("Found existing archives: #{files}.\n" <>
                   "Are you sure you want to replace them?")
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
