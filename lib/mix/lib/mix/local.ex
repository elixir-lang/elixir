defmodule Mix.Local do
  @moduledoc false

  @public_keys_html "https://repo.hex.pm/installs/public_keys.html"

  @type item :: atom()

  @doc """
  Returns the name for an archive or an escript, based on the project config.

  The `target` parameter is a module implementing `Mix.Local.Target`

  ## Examples

      iex> Mix.Local.name_for(Mix.Local.Target.Archive, 
                              [app: "foo", version: "0.1.0"])
      "foo-0.1.0.ez"

      iex> Mix.Local.name_for(Mix.Local.Target.Escript, [escript: [name: "foo"]])
      "foo"

  """
  @spec name_for(item, Keyword.t) :: String.t
  def name_for(target, project) do
    target.name_for(project)
  end


  @doc """
  The path for local archives or escripts. The first parameter is the
  module implementing the Mix.Local.Target for the thing to be stored.
  """
  @spec path_for(item) :: String.t
  def path_for(target) when is_atom(target) do
    target.path_for()
  end

  @doc """
  Appends archives paths into Erlang code path.
  """
  def append_archives do
    archives = archives_ebins()
    Enum.each(archives, &check_elixir_version_in_ebin/1)
    Enum.each(archives, &Code.append_path/1)
  end

  @doc """
  Appends Mix paths into Erlang code path.
  """
  def append_paths do
    Enum.each(Mix.Utils.mix_paths, &Code.append_path(&1))
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
    path                    # "foo/bar/baz-0.1.0.ez"
    |> Path.basename        # "baz-0.1.0.ez"
    |> Path.rootname(".ez") # "baz-0.1.0"
  end

  @doc """
  Returns the ebin path of an archive.
  """
  def archive_ebin(path) do
    Path.join [path, archive_name(path), "ebin"]
  end

  defp archives_ebins do
    path = path_for(Mix.Local.Target.Archive)
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
    elixir = ebin |> Path.dirname |> Path.join(".elixir") |> String.to_charlist
    case :erl_prim_loader.get_file(elixir) do
      {:ok, req, _} ->
        unless Version.match?(System.version, req) do
          archive = ebin |> Path.dirname |> Path.basename
          Mix.shell.error "warning: the archive #{archive} requires Elixir #{inspect req} " <>
                          "but you are running on v#{System.version}"
        end
        :ok
      :error ->
        :ok
    end
  end

  @doc """
  Fetches the given signed CSV files, verifies and returns the matching
  Elixir version, artifact version and artifact's checksum.

  Used to install both Rebar and Hex from S3.
  """
  def find_matching_versions_from_signed_csv!(name, path) do
    csv = read_path!(name, path)

    signature =
      read_path!(name, path <> ".signed")
      |> String.replace("\n", "")
      |> Base.decode64!

    if Mix.PublicKey.verify csv, :sha512, signature do
      csv
      |> parse_csv
      |> find_latest_eligible_version
    else
      Mix.raise "Could not install #{name} because Mix could not verify authenticity " <>
                "of metadata file at #{path}. This may happen because a proxy or some " <>
                "entity is interfering with the download or because you don't have a " <>
                "public key to verify the download.\n\nYou may try again later or check " <>
                "if a new public key has been released in our public keys page: #{@public_keys_html}"
    end
  end

  defp read_path!(name, path) do
    case Mix.Utils.read_path(path) do
      {:ok, contents} -> contents
      {:remote, message} ->
        Mix.raise """
        #{message}

        Could not install #{name} because Mix could not download metadata at #{path}.
        """
    end
  end

  defp parse_csv(body) do
    body
    |> :binary.split("\n", [:global, :trim])
    |> Enum.map(&:binary.split(&1, ",", [:global, :trim]))
  end

  defp find_latest_eligible_version(entries) do
    {:ok, current_version} = Version.parse(System.version)
    entries
    |> Enum.reverse
    |> Enum.find_value(entries, &find_version(&1, current_version))
  end

  defp find_version([artifact_version, digest | versions], current_version) do
    if version = Enum.find(versions, &Version.compare(&1, current_version) != :gt) do
      {version, artifact_version, digest}
    end
  end
end
