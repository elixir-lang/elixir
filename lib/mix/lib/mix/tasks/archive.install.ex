# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Mix.Tasks.Archive.Install do
  use Mix.Task

  @shortdoc "Installs an archive locally"

  @moduledoc """
  Installs an archive locally.

  If no argument is supplied but there is an archive in the project's
  root directory (created with `mix archive.build`), then the archive
  will be installed locally. For example:

      $ mix do archive.build + archive.install

  If an argument is provided, it should be a local path to a
  prebuilt archive, a Git repository, a GitHub repository, or a Hex
  package.

      $ mix archive.install archive.ez
      $ mix archive.install path/to/archive.ez
      $ mix archive.install git https://path/to/git/repo
      $ mix archive.install git https://path/to/git/repo branch git_branch
      $ mix archive.install git https://path/to/git/repo tag git_tag
      $ mix archive.install git https://path/to/git/repo ref git_ref
      $ mix archive.install github user/project
      $ mix archive.install github user/project branch git_branch
      $ mix archive.install github user/project tag git_tag
      $ mix archive.install github user/project ref git_ref
      $ mix archive.install hex hex_package
      $ mix archive.install hex hex_package 1.2.3

  After installation, the tasks in the archive are available locally:

      $ mix some_task

  Note that installing via Git, GitHub, or Hex fetches the source
  of the archive and builds it, while using local path uses a pre-built archive.

  ## Security

  Archives must be installed only from sources you trust.

  Installing an archive from Git, GitHub, or Hex executes code from the source
  during installation, unless a pre-built archive is given. Once an archive is
  installed, Mix may load code from it as a plugin on any Mix command, even if
  no archive command is executed.

  ## Command line options

    * `--sha512` - checks the archive matches the given SHA-512 checksum. Only
      applies to installations via a local path

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like Make

    * `--submodules` - fetches repository submodules before building archive from
      Git or GitHub

    * `--sparse` - checkout a single directory inside the Git repository and use
      it as the archive root directory

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
    sparse: :string,
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
    archive_name = archive_name!(contents)
    dir_dest = Path.join(Path.dirname(ez_path), archive_name)

    remove_previous_versions(previous)

    File.mkdir_p!(dir_dest)
    {:ok, _} = :zip.extract(contents, cwd: to_charlist(dir_dest))
    Mix.shell().info([:green, "* creating ", :reset, Path.relative_to_cwd(dir_dest)])

    ebin = Mix.Local.archive_ebin(dir_dest)
    Mix.Local.check_elixir_version_in_ebin(ebin)
    true = Code.append_path(ebin, cache: true)
    :ok
  end

  @impl true
  def build(_install_spec, _opts) do
    src = Mix.Local.name_for(:archives, Mix.Project.config())
    previous = find_previous_versions(src)

    Enum.each(previous, fn path ->
      Code.delete_path(Mix.Local.archive_ebin(path))
    end)

    Mix.Task.run("loadconfig")
    Mix.Task.run("archive.build", [])
    src
  end

  ### Private helpers

  defp archive_name!(contents) do
    with {:ok, files} <- :zip.list_dir(contents),
         zip_files = Enum.filter(files, &match?({:zip_file, _, _, _, _, _}, &1)),
         true <- zip_files != [] do
      Enum.reduce(zip_files, nil, fn zip_file, root ->
        validate_archive_path!(zip_file, root)
      end)
    else
      _ ->
        Mix.raise("Installation failed: invalid archive file, no files found")
    end
  end

  defp validate_archive_path!({:zip_file, path, file_info, _, _, _}, root) do
    type = elem(file_info, 2)
    path = zip_path_to_string(path)

    unless type in [:regular, :directory] do
      Mix.raise(
        "Installation failed: invalid archive file, #{inspect(path)} is not a regular file or directory"
      )
    end

    cond do
      Path.type(path) != :relative ->
        Mix.raise(
          "Installation failed: invalid archive file, #{inspect(path)} is an absolute path"
        )

      String.contains?(path, ["..", "\\", <<0>>]) ->
        Mix.raise("Installation failed: invalid archive file, #{inspect(path)} is an unsafe path")

      true ->
        :ok
    end

    case String.split(path, "/", trim: true) do
      [new_root | _] ->
        cond do
          root && root != new_root ->
            Mix.raise(
              "Installation failed: invalid archive file, #{inspect(path)} is outside archive root #{inspect(root)}"
            )

          true ->
            new_root
        end

      [] ->
        Mix.raise("Installation failed: invalid archive file, #{inspect(path)} is empty")
    end
  end

  defp zip_path_to_string(path) when is_list(path), do: List.to_string(path)
  defp zip_path_to_string(path) when is_binary(path), do: path

  defp archives(name) do
    Mix.path_for(:archives)
    |> Path.join(name)
    |> Path.wildcard()
  end

  defp remove_previous_versions([]), do: :ok
  defp remove_previous_versions(previous), do: Enum.each(previous, &File.rm_rf!/1)
end
