defmodule Mix.Local do
  @moduledoc false

  @public_keys_html "https://s3.amazonaws.com/s3.hex.pm/installs/public_keys.html"

  @doc """
  The path for local archives.

  Check `mix archive` for info.
  """
  def archives_path do
    System.get_env("MIX_ARCHIVES") ||
      Path.join(Mix.Utils.mix_home, "archives")
  end

  @doc """
  Appends archives paths into Erlang code path.
  """
  def append_archives do
    archives = archives_ebin()
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
  def all_tasks do
    Mix.Task.load_tasks(archives_ebin())
  end

  @doc """
  Returns paths of all archive files matching given
  application name.
  """
  def archive_files(name) do
    archives(name, ".ez") ++ archives(name, "-*.ez")
  end

  @doc """
  Checks Elixir version requirement stored in the archive and print a warning if it is not satisfied.
  """
  def check_archive_elixir_version(path) do
    path |> Mix.Archive.ebin |> check_elixir_version_in_ebin()
  end

  defp archives(name, suffix) do
    archives_path()
    |> Path.join(name <> suffix)
    |> Path.wildcard
  end

  defp archives_ebin do
    Path.join(archives_path(), "*.ez") |> Path.wildcard |> Enum.map(&Mix.Archive.ebin/1)
  end

  defp check_elixir_version_in_ebin(ebin) do
    elixir = ebin |> Path.dirname |> Path.join(".elixir") |> String.to_char_list
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
  Fetches the given signed CSV files, verify and return the matching
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
      |> find_latest_eligibile_version
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

  defp find_latest_eligibile_version(entries) do
    {:ok, current_version} = Version.parse(System.version)
    entries
    |> Enum.reverse
    |> Enum.find_value(entries, &find_version(&1, current_version))
  end

  defp find_version([artifact_version, digest|versions], current_version) do
    if version = Enum.find(versions, &Version.compare(&1, current_version) != :gt) do
      {version, artifact_version, digest}
    end
  end
end
