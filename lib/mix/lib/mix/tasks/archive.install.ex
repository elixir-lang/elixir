defmodule Mix.Tasks.Archive.Install do
  use Mix.Task

  @shortdoc "Installs an archive locally"

  @moduledoc """
  Installs an archive locally.

  If no argument is supplied but there is an archive in the project's
  root directory (created with `mix archive.build`), then the archive
  will be installed locally. For example:

      mix do archive.build, archive.install

  If an argument is provided, it should be a local path to a
  prebuilt archive, a Git repository, a GitHub repository, or a Hex
  package.

      mix archive.install archive.ez
      mix archive.install path/to/archive.ez
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

  Note that installing via Git, GitHub, or Hex fetches the source
  of the archive and builds it, while using local path uses a pre-built archive.

  ## Command line options

    * `--sha512` - checks the archive matches the given SHA-512 checksum. Only
      applies to installations via a local path

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like Make

    * `--submodules` - fetches repository submodules before building archive from
      Git or GitHub

    * `--app` - specifies a custom app name to be used for building the archive
      from Git, GitHub, or Hex

    * `--organization` - set this for Hex private packages belonging to an
      organization

    * `--repo` - set this for self-hosted Hex instances, defaults to `hexpm`

  """

  @behaviour Mix.Local.Installer

  @switches [
    force: :boolean,
    sha512: :string,
    submodules: :boolean,
    app: :string,
    organization: :string,
    repo: :string,
    timeout: :integer
  ]

  @impl true
  def run(argv) do
    Mix.Local.Installer.install(__MODULE__, argv, @switches)
  end

  @impl true
  def check_install_spec({:local, path} = _install_spec, _opts) do
    check_extname(path)
  end

  def check_install_spec({:url, url} = _install_spec, _opts) do
    check_extname(url)
  end

  def check_install_spec(_, _), do: :ok

  defp check_extname(path_or_url) do
    if Path.extname(path_or_url) == ".ez" do
      :ok
    else
      {:error, "Expected a local file path ending in \".ez\"."}
    end
  end

  @impl true
  def find_previous_versions(src) do
    app =
      src
      |> Mix.Local.archive_name()
      |> String.split("-")
      |> List.first()

    if app do
      archives(app) ++ archives(app <> "-*")
    else
      []
    end
  end

  @impl true
  def install(basename, contents, previous) do
    ez_path = Path.join(Mix.path_for(:archives), basename)
    dir_dest = resolve_destination(ez_path, contents)

    remove_previous_versions(previous)

    File.mkdir_p!(dir_dest)
    {:ok, _} = :zip.extract(contents, cwd: to_charlist(dir_dest))
    Mix.shell().info([:green, "* creating ", :reset, Path.relative_to_cwd(dir_dest)])

    ebin = Mix.Local.archive_ebin(dir_dest)
    Mix.Local.check_elixir_version_in_ebin(ebin)
    true = Code.append_path(ebin)
    :ok
  end

  @impl true
  def build(_install_spec, _opts) do
    src = Mix.Local.name_for(:archives, Mix.Project.config())
    previous = find_previous_versions(src)

    Enum.each(previous, fn path ->
      Code.delete_path(Mix.Local.archive_ebin(path))
    end)

    Mix.Task.run("archive.build", [])
    src
  end

  ### Private helpers

  defp resolve_destination(ez_path, contents) do
    with {:ok, [_comment, zip_first_file | _]} <- :zip.list_dir(contents),
         {:zip_file, zip_first_path, _, _, _, _} = zip_first_file,
         [zip_root_dir | _] = Path.split(zip_first_path) do
      Path.join(Path.dirname(ez_path), zip_root_dir)
    else
      _ ->
        Mix.raise("Installation failed: invalid archive file")
    end
  end

  defp archives(name) do
    Mix.path_for(:archives)
    |> Path.join(name)
    |> Path.wildcard()
  end

  defp remove_previous_versions([]), do: :ok
  defp remove_previous_versions(previous), do: Enum.each(previous, &File.rm_rf!/1)
end
