defmodule Mix.Tasks.Local.Hex do
  use Mix.Task

  @hex_s3           "http://s3.amazonaws.com/s3.hex.pm"
  @hex_list_url     @hex_s3 <> "/installs/hex-1.x.csv"
  @hex_archive_url  @hex_s3 <> "/installs/[ELIXIR_VERSION]/hex-[HEX_VERSION].ez"

  @shortdoc "Installs Hex locally"

  @moduledoc """
  Installs Hex locally.

      mix local.hex

  ## Command line options

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like `make`
  """
  @spec run(OptionParser.argv) :: boolean
  def run(args) do
    {elixir_version, hex_version, sha512} =
      Mix.Local.find_matching_versions_from_signed_csv!("Hex", @hex_list_url)

    url =
      @hex_archive_url
      |> String.replace("[ELIXIR_VERSION]", elixir_version)
      |> String.replace("[HEX_VERSION]", hex_version)

    Mix.Tasks.Archive.Install.run [url, "--sha512", sha512 | args]
  end
end
