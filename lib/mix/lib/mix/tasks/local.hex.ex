defmodule Mix.Tasks.Local.Hex do
  use Mix.Task

  @hex_list_path    "/installs/hex-1.x.csv"
  @hex_archive_path "/installs/[ELIXIR_VERSION]/hex-[HEX_VERSION].ez"

  @shortdoc "Installs Hex locally"

  @moduledoc """
  Installs Hex locally.

      mix local.hex

  ## Command line options

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like `make`

    * `--if-missing` - performs installation only if Hex is not installed yet;
      intended for automation as a replacement of `--force` to not reinstall
      Hex every time scripts are run.
      If both options are set `--force` takes precedence

  ## Mirrors

  If you want to change the [default mirror](https://repo.hex.pm)
  used for fetching Hex, set the `HEX_MIRROR` environment variable.
  """
  @spec run(OptionParser.argv) :: boolean
  def run(argv) do
    {opts, _} = OptionParser.parse!(argv, switches: [if_missing: :boolean,
                                                     force: :boolean])
    should_install = case opts do
      %{force: true}      -> true;
      %{if_missing: true} -> Code.ensure_loaded?(Hex);
      _                   -> true
    end

    if should_install do
      do_install(argv)
    else
      true
    end
  end

  @spec do_install(OptionParser.argv) :: boolean
  def do_install(argv) do
    hex_mirror = Mix.Hex.mirror

    {elixir_version, hex_version, sha512} =
      Mix.Local.find_matching_versions_from_signed_csv!("Hex", hex_mirror <> @hex_list_path)

    url =
      (hex_mirror <> @hex_archive_path)
      |> String.replace("[ELIXIR_VERSION]", elixir_version)
      |> String.replace("[HEX_VERSION]", hex_version)

    Mix.Tasks.Archive.Install.run [url, "--sha512", sha512 | argv]
  end
end
