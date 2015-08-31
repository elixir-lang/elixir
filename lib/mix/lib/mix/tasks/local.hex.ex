defmodule Mix.Tasks.Local.Hex do
  use Mix.Task

  @hex_s3           "https://s3.amazonaws.com/s3.hex.pm"
  @hex_list_url     @hex_s3 <> "/installs/hex-1.x.csv"
  @hex_archive_url  @hex_s3 <> "/installs/[VERSION]/hex.ez"

  @shortdoc "Install hex locally"

  @moduledoc """
  Install Hex locally.

      mix local.hex

  ## Command line options

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like make
  """
  @spec run(OptionParser.argv) :: boolean
  def run(args) do
    {version, sha512} = get_matching_version()
    url = String.replace(@hex_archive_url, "[VERSION]", version)
    Mix.Tasks.Archive.Install.run [url, "--sha512", sha512 | args]
  end

  defp get_matching_version do
    csv = read_path!(@hex_list_url)

    signature =
      read_path!(@hex_list_url <> ".signed")
      |> String.replace("\n", "")
      |> Base.decode64!

    if Mix.PublicKey.verify csv, :sha512, signature do
      csv
      |> parse_csv
      |> find_latest_eligibile_version
    else
      Mix.raise "Could not install Hex because Mix could not verify authenticity " <>
                "of metadata file at #{@hex_list_url}. This may happen because a " <>
                "proxy or some entity is interfering with the download or because " <>
                "you don't have a public key to verify the downloaded package"
    end
  end

  defp read_path!(path) do
    case Mix.Utils.read_path(path) do
      {:ok, contents} -> contents
      {:remote, message} ->
        Mix.raise """
        #{message}

        Could not install Hex because Mix could not download metadata at #{path}.
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

  defp find_version([_hex, digest|versions], current_version) do
    if version = Enum.find(versions, &Version.compare(&1, current_version) != :gt) do
      {version, digest}
    end
  end
end
