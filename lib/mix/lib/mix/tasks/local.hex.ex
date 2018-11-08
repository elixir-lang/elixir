defmodule Mix.Tasks.Local.Hex do
  use Mix.Task

  @hex_list_path "/installs/hex-1.x.csv"
  @hex_archive_path "/installs/[ELIXIR_VERSION]/hex-[HEX_VERSION].ez"

  @shortdoc "Installs Hex locally"

  @moduledoc """
  Installs Hex locally.

      mix local.hex

  ## Command line options

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like `make`

    * `--if-missing` - performs installation only if Hex is not installed yet;
      intended to avoid repeatedly reinstalling Hex in automation when a script
      may be run multiple times

  If both options are set, `--force` takes precedence.

  ## Mirrors

  If you want to change the [default mirror](https://repo.hex.pm)
  used for fetching Hex, set the `HEX_MIRROR` environment variable.
  """
  @switches [if_missing: :boolean, force: :boolean]

  @impl true
  def run(argv) do
    {opts, _} = OptionParser.parse!(argv, switches: @switches)

    should_install? =
      if Keyword.get(opts, :if_missing, false) do
        not Code.ensure_loaded?(Hex)
      else
        true
      end

    argv =
      if Keyword.get(opts, :force, false) do
        ["--force"]
      else
        []
      end

    should_install? && run_install(argv)
  end

  defp run_install(argv) do
    hex_mirror = Mix.Hex.mirror()

    {elixir_version, hex_version, sha512} =
      Mix.Local.find_matching_versions_from_signed_csv!("Hex", hex_mirror <> @hex_list_path)

    url =
      (hex_mirror <> @hex_archive_path)
      |> String.replace("[ELIXIR_VERSION]", elixir_version)
      |> String.replace("[HEX_VERSION]", hex_version)

    # Unload the Hex module we loaded earlier to avoid conflicts when Hex is updated
    # and then used without restarting the VM
    :code.purge(Hex)

    Mix.Tasks.Archive.Install.run([url, "--sha512", sha512 | argv])
  end
end
