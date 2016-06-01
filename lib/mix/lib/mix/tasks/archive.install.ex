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

  @behaviour Mix.Local.Installer

  @switches [force: :boolean, sha512: :string]
  @spec run(OptionParser.argv) :: boolean
  def run(argv) do
    Mix.Local.Installer.install({__MODULE__, :archive}, argv, @switches)
  end

  ### Mix.Local.Installer callbacks

  def check_path_or_url(path_or_url) do
    if Path.extname(path_or_url) == ".ez" do
      :ok
    else
      {:error, "Expected a local file path or a file URL ending in .ez."}
    end
  end

  def find_previous_versions(src, _dst) do
    app =
      src
      |> Mix.Local.archive_name
      |> String.split("-")
      |> List.first

    if app do
      archives(app) ++ archives(app <> "-*")
    else
      []
    end
  end

  def install(ez_dst, contents, previous) do
    remove_previous_versions(previous)

    # Get the directory name and extract it there
    dir_dst = Path.rootname(ez_dst)
    File.mkdir_p!(dir_dst)
    {:ok, _} = :zip.extract(contents, [cwd: dir_dst])
    Mix.shell.info [:green, "* creating ", :reset, Path.relative_to_cwd(dir_dst)]

    ebin = Mix.Local.archive_ebin(dir_dst)
    Mix.Local.check_elixir_version_in_ebin(ebin)
    true = Code.append_path(ebin)
    :ok
  end

  ### Private helpers

  defp archives(name) do
    Mix.Local.path_for(:archive)
    |> Path.join(name)
    |> Path.wildcard
  end

  defp remove_previous_versions([]),
    do: :ok
  defp remove_previous_versions(previous),
    do: Enum.each(previous, &File.rm_rf!/1)
end
