# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Mix.Local do
  @moduledoc false

  @doc """
  Returns the name for an archive or an escript, based on the project config.

  ## Examples

      iex> Mix.Local.name_for(:archives, app: "foo", version: "0.1.0")
      "foo-0.1.0.ez"

      iex> Mix.Local.name_for(:escripts, escript: [name: "foo"])
      "foo"

  """
  @spec name_for(:archives | :escripts, keyword) :: String.t()
  def name_for(:archives, project) do
    version = if version = project[:version], do: "-#{version}"
    "#{project[:app]}#{version}.ez"
  end

  def name_for(:escripts, project) do
    case get_in(project, [:escript, :name]) do
      nil -> project[:app]
      name -> name
    end
    |> to_string()
  end

  @deprecated "Use Mix.path_for/1 instead"
  def path_for(:archive), do: Mix.path_for(:archives)
  def path_for(:escript), do: Mix.path_for(:escripts)

  @doc """
  Appends archive paths to the Erlang code path.
  """
  def append_archives do
    for archive <- archives_ebins() do
      check_elixir_version_in_ebin(archive)
      Code.append_path(archive, cache: true)
    end

    :ok
  end

  @doc """
  Removes archive paths from Erlang code path.
  """
  def remove_archives do
    for archive <- archives_ebins() do
      Code.delete_path(archive)
    end

    :ok
  end

  @doc """
  Appends Mix paths to the Erlang code path.

  We don't cache them as they are rarely used and
  they may be development paths.
  """
  def append_paths do
    Enum.each(mix_paths(), &Code.append_path/1)
  end

  @doc """
  Removes Mix paths from the Erlang code path.
  """
  def remove_paths do
    Enum.each(mix_paths(), &Code.delete_path/1)
  end

  defp mix_paths do
    if path = System.get_env("MIX_PATH") do
      String.split(path, path_separator())
    else
      []
    end
  end

  defp path_separator do
    case :os.type() do
      {:win32, _} -> ";"
      {:unix, _} -> ":"
    end
  end

  @doc """
  Returns all tasks in local archives.
  """
  def archives_tasks do
    Mix.Task.load_tasks(archives_ebins())
  end

  @doc """
  Returns the name of an archive given a path.
  """
  def archive_name(path) do
    path
    |> Path.basename()
    |> Path.rootname(".ez")
  end

  @doc """
  Returns the ebin path of an archive.
  """
  def archive_ebin(path) do
    Path.join([path, archive_name(path), "ebin"])
  end

  defp archives_ebins do
    path = Mix.path_for(:archives)

    case File.ls(path) do
      {:ok, entries} -> Enum.map(entries, &archive_ebin(Path.join(path, &1)))
      {:error, _} -> []
    end
  end

  @doc """
  Checks Elixir version requirement stored in the ebin directory
  and print a warning if it is not satisfied.
  """
  def check_elixir_version_in_ebin(ebin) do
    elixir = ebin |> Path.dirname() |> Path.join(".elixir") |> String.to_charlist()

    case File.read(elixir) do
      {:ok, req} ->
        if !Version.match?(System.version(), req) do
          archive = ebin |> Path.dirname() |> Path.basename()

          Mix.shell().error(
            "warning: the archive #{archive} requires Elixir #{inspect(req)} " <>
              "but you are running on v#{System.version()}"
          )
        end

        :ok

      {:error, _} ->
        :ok
    end
  end

  @doc """
  Fetches the given signed CSV files, verifies and returns the matching
  Elixir version, artifact version and artifact's checksum.

  Used to install both Rebar and Hex from S3.
  """
  def find_matching_versions!(name, version, path) do
    name
    |> read_path!(path)
    |> parse_csv()
    |> find_latest_eligible_version(version)
    |> Kernel.||(Mix.raise("Could not find a version of #{name} matching: #{version}"))
  end

  defp read_path!(name, path) do
    case Mix.Utils.read_path(path) do
      {:ok, contents} ->
        contents

      {:remote, message} ->
        Mix.raise(
          """
          #{message}

          Could not install #{name} because Mix could not download metadata at #{path}.
          """ <> suggestions(name)
        )
    end
  end

  defp suggestions("Hex") do
    """

    Alternatively, you can compile and install Hex directly with this command:

        $ mix archive.install github hexpm/hex branch latest
    """
  end

  defp suggestions(_) do
    ""
  end

  defp parse_csv(body) do
    body
    |> :binary.split("\n", [:global, :trim])
    |> Enum.map(&:binary.split(&1, ",", [:global, :trim]))
  end

  defp find_latest_eligible_version(entries, artifact_version) do
    elixir_version = Version.parse!(System.version())

    entries
    |> Enum.reverse()
    |> find_version(artifact_version, elixir_version)
  end

  defp find_version(entries, artifact_version, elixir_version) do
    entries =
      if artifact_version do
        Enum.filter(entries, &(hd(&1) == artifact_version))
      else
        entries
      end

    Enum.find_value(entries, &find_by_elixir_version(&1, elixir_version))
  end

  defp find_by_elixir_version([artifact_version, digest, hex_elixir_version | _], elixir_version) do
    if Version.compare(hex_elixir_version, elixir_version) != :gt do
      {hex_elixir_version, artifact_version, digest}
    end
  end
end
