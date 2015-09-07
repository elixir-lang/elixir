defmodule Mix.Tasks.Local.Hex do
  use Mix.Task

  @hex_s3           "https://s3.amazonaws.com/s3.hex.pm"
  @hex_list_url     @hex_s3 <> "/installs/hex-1.x.csv"
  @hex_archive_url  @hex_s3 <> "/installs/[VERSION]/hex.ez"

  @shortdoc "Installs hex locally"

  @moduledoc """
  Installs Hex locally.

      mix local.hex

  ## Command line options

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like make
  """
  @spec run(OptionParser.argv) :: boolean
  def run(args) do
    {version, sha512} = Mix.Local.find_matching_elixir_version_from_signed_csv!("Hex", @hex_list_url)
    url = String.replace(@hex_archive_url, "[VERSION]", version)
    Mix.Tasks.Archive.Install.run [url, "--sha512", sha512 | args]
  end
end
