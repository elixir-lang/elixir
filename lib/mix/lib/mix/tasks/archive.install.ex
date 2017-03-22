defmodule Mix.Tasks.Archive.Install do
  use Mix.Task

  @shortdoc "Installs an archive locally"

  @moduledoc """
  Installs an archive locally.

  If no argument is supplied but there is an archive in the project's
  root directory (created with `mix archive.build`), then the archive
  will be installed locally. For example:

      mix do archive.build, archive.install

  If an argument is provided, it should be a local path or a URL to a
  prebuilt archive, a Git repository, a GitHub repository, or a Hex
  package.

      mix archive.install archive.ez
      mix archive.install path/to/archive.ez
      mix archive.install https://example.com/my_archive.ez
      mix archive.install git https://path/to/git/repo
      mix archive.install git https://path/to/git/repo branch git_branch
      mix archive.install git https://path/to/git/repo tag git_tag
      mix archive.install git https://path/to/git/repo ref git_ref
      mix archive.install github user/project
      mix archive.install github user/project branch git_branch
      mix archive.install github user/project tag git_tag
      mix archive.install github user/project ref git_ref
      mix archive.install hex hex_package
      mix archive.install hex hex_package 1.2.3

  After installation, the tasks in the archive are available locally:

      mix some_task

  ## Command line options

    * `--sha512` - checks the archive matches the given SHA-512 checksum

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like `make`

    * `--submodules` - fetches repository submodules before building archive from
      git or github

    * `--app` - specifies a custom app name to be used for building the archive
      from git, github, or hex
  """

  @behaviour Mix.Local.Installer

  @switches [force: :boolean, sha512: :string, submodules: :boolean, app: :string]
  @spec run(OptionParser.argv) :: boolean
  def run(argv) do
    Mix.Local.Installer.install({__MODULE__, :archive}, argv, @switches)
  end

  ### Mix.Local.Installer callbacks

  def check_install_spec({local_or_url, path_or_url}, _opts) when
      local_or_url in [:local, :url] do
    if Path.extname(path_or_url) == ".ez" do
      :ok
    else
      {:error, "Expected a local file path or a file URL ending in .ez."}
    end
  end

  def check_install_spec(_, _), do: :ok

  def find_previous_versions(src, _dest) do
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

  def install(ez_path, contents, previous) do
    # Get the directory name and extract it there
    dir_dest = resolve_destination(ez_path, contents)

    remove_previous_versions(previous)

    File.mkdir_p!(dir_dest)
    {:ok, _} = :zip.extract(contents, [cwd: dir_dest])
    Mix.shell.info [:green, "* creating ", :reset, Path.relative_to_cwd(dir_dest)]

    ebin = Mix.Local.archive_ebin(dir_dest)
    Mix.Local.check_elixir_version_in_ebin(ebin)
    true = Code.append_path(ebin)
    :ok
  end

  def build(_mixfile) do
    Mix.Task.run("archive.build", [])
  end

  ### Private helpers

  defp resolve_destination(ez_path, contents) do
    case :zip.list_dir(contents) do
      {:ok, [_comment, zip_first_file | _]} ->
        zip_root_dir =
          zip_first_file
          |> elem(1)
          |> Path.split()
          |> hd

        Path.join(Path.dirname(ez_path), zip_root_dir)
      {:error, _reason} ->
        Mix.raise "Installation failed: Invalid archive file"
    end
  end

  defp archives(name) do
    # TODO: We can remove the .ez extension on Elixir 2.0 since we always unzip since 1.3
    Mix.Local.path_for(:archive)
    |> Path.join(name <> "{,*.ez}")
    |> Path.wildcard
  end

  defp remove_previous_versions([]),
    do: :ok
  defp remove_previous_versions(previous),
    do: Enum.each(previous, &File.rm_rf!/1)
end
