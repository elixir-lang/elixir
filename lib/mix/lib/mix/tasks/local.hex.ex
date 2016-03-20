defmodule Mix.Tasks.Local.Hex do
  use Mix.Task

  @hex_s3           "https://hexpmrepo.global.ssl.fastly.net"
  @hex_list_path    "/installs/hex-1.x.csv"
  @hex_archive_path "/installs/[ELIXIR_VERSION]/hex-[HEX_VERSION].ez"

  @shortdoc "Installs Hex locally"

  @moduledoc """
  Installs Hex locally.

      mix local.hex

  ## Command line options

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like `make`

  ## Mirrors

  If you want to change the [default mirror](https://hexpmrepo.global.ssl.fastly.net)
  to use for fetching Hex please set the `HEX_CDN` environment variable.
  """
  @spec run(OptionParser.argv) :: boolean
  def run(args) do
    hex_mirror = System.get_env("HEX_CDN") || @hex_s3

    {elixir_version, hex_version, sha512} =
      Mix.Local.find_matching_versions_from_signed_csv!("Hex", hex_mirror <> @hex_list_path)

    url =
      (hex_mirror <> @hex_archive_path)
      |> String.replace("[ELIXIR_VERSION]", elixir_version)
      |> String.replace("[HEX_VERSION]", hex_version)

    Mix.Tasks.Archive.Install.run [url, "--sha512", sha512 | args]
  end
end
