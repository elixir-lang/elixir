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
      archives(app <> ".ez") ++ archives(app <> "-*.ez")
    else
      []
    end
  end

  def before_install(src, dst_path) do
    check_file_exists(src, dst_path)
  end

  def after_install(dst, previous) do
    ebin = Mix.Local.archive_ebin(dst)
    Mix.Local.check_elixir_version_in_ebin(ebin)
    unless dst in previous, do: remove_previous_versions(previous)
    true = Code.append_path(ebin)
  end

  ### Private helpers

  defp archives(name) do
    Mix.Local.path_for(:archive)
    |> Path.join(name)
    |> Path.wildcard
  end

  defp check_file_exists(src, path) do
    # OTP keeps loaded archives open, this leads to unfortunate behaviour on
    # Windows when trying overwrite loaded archives. remove_previous_versions
    # completes successfully even though the file will be first removed after
    # the BEAM process is dead. Because of this we ask the user rerun the
    # command, which should complete successfully at that time

    if File.exists?(path) and match?({:win32, _}, :os.type) do
      message = "Unable to overwrite open archives on Windows. Please manually remove " <>
                "the existing archive at #{inspect path} and run this command again. In " <>
                "case re-running the command still does not work, please fetch the archive " <>
                "at #{inspect src} and manually copy it to #{inspect path}."
      {:error, message}
    else
      :ok
    end
  end

  defp remove_previous_versions([]),
    do: :ok
  defp remove_previous_versions(previous),
    do: Enum.each(previous, &File.rm!/1)
end
