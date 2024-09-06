defmodule Mix.Local do
  @moduledoc false

  @public_keys_html "https://builds.hex.pm/installs/public_keys.html"

  @in_memory_key """
  -----BEGIN PUBLIC KEY-----
  MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAslPz1mAfyAvRv8W8xOdv
  HQMbDJkDKfRhsL4JBGwGH7qw0xh+TbaUlNaM3pF+i8VUjS/4FeXjT/OAUEAHu5Y2
  rBVlx00QcH8Dpbyf+H73XiCs0MXnTSecqDgzx6i6NMi8knklHT7yHySHtuuPmPuN
  Po8QTKolCKftwPE/iNDeyZfwufd+hTCoCQdoTVcB01SElfNtvKRtoKbx35q80IPr
  rOcGsALmr58+bWqCTY/51kFeRxzrPJ5LdcLU/AebyWddD4IUfPDxk16jTiCagMWA
  JPSwo8NUrWDIBbD+rEUp06y0ek276rG5Tzm/3Bma56RN/u6nAqBTBE8F2Hu2QBKj
  lQIDAQAB
  -----END PUBLIC KEY-----
  """

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
  def find_matching_versions_from_signed_csv!(name, version, path) do
    ensure_applications!()
    csv = read_unsafe_path!(name, path)

    signature =
      read_unsafe_path!(name, path <> ".signed")
      |> String.replace("\n", "")
      |> Base.decode64!()

    verified? =
      Enum.any?(public_keys(), fn {id, key} ->
        :public_key.verify(csv, :sha512, signature, decode_pk!(id, key))
      end)

    if verified? do
      result =
        csv
        |> parse_csv()
        |> find_latest_eligible_version(version)

      result || Mix.raise("Could not find a version of #{name} matching: #{version}")
    else
      Mix.raise(
        "Could not install #{name} because Mix could not verify authenticity " <>
          "of metadata file at #{inspect(path)}. This may happen because a proxy or some " <>
          "entity is interfering with the download or because you don't have a " <>
          "public key to verify the download.\n\nYou may try again later or check " <>
          "if a new public key has been released in our public keys page: #{@public_keys_html}"
      )
    end
  end

  defp read_unsafe_path!(name, path) do
    case Mix.Utils.read_path(path, unsafe_uri: true) do
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

  defp find_version(entries, _artifact_version = nil, elixir_version) do
    Enum.find_value(entries, &find_by_elixir_version(&1, elixir_version))
  end

  defp find_version(entries, artifact_version, elixir_version) do
    Enum.find_value(entries, &find_by_artifact_version(&1, artifact_version, elixir_version))
  end

  defp find_by_elixir_version([artifact_version, digest | versions], elixir_version) do
    if version = Enum.find(versions, &(Version.compare(&1, elixir_version) != :gt)) do
      {version, artifact_version, digest}
    end
  end

  defp find_by_artifact_version(
         [artifact_version, digest | versions],
         artifact_version,
         elixir_version
       ) do
    if version = Enum.find(versions, &(Version.compare(&1, elixir_version) != :gt)) do
      {version, artifact_version, digest}
    end
  end

  defp find_by_artifact_version(_entry, _artifact_version, _elixir_version) do
    nil
  end

  ## Public keys

  @doc """
  Returns the file system path for public keys.
  """
  def public_keys_path, do: Path.join(Mix.Utils.mix_home(), "public_keys")

  @doc """
  Returns all public keys as a list.
  """
  def public_keys do
    path = public_keys_path()

    [{"in-memory public key for Elixir v#{System.version()}", @in_memory_key}] ++
      case File.ls(path) do
        {:ok, keys} -> Enum.map(keys, &{&1, File.read!(Path.join(path, &1))})
        {:error, _} -> []
      end
  end

  @doc """
  Decodes a public key and raises if the key is invalid.
  """
  def decode_pk!(id, key) do
    ensure_applications!()

    try do
      [rsa_public_key] = :public_key.pem_decode(key)
      :public_key.pem_entry_decode(rsa_public_key)
    rescue
      _ ->
        Mix.raise("""
        Could not decode public key: #{id}. The public key contents are shown below.

        #{key}

        Public keys must be valid and be in the PEM format
        """)
    end
  end

  defp ensure_applications! do
    Mix.ensure_application!(:crypto)
    Mix.ensure_application!(:asn1)
    Mix.ensure_application!(:public_key)
  end
end
