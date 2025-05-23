# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Mix.Tasks.Local.Hex do
  use Mix.Task

  @hex_list_path "/installs/hex.csv"
  @hex_archive_path "/installs/[ELIXIR_VERSION]/hex-[HEX_VERSION]-otp-[OTP_RELEASE].ez"

  @shortdoc "Installs Hex locally"

  @moduledoc """
  Installs Hex locally.

      $ mix local.hex [version]

  By default the latest compatible version of Hex will be installed, unless
  `version` is specified.

  If installing a precompiled Hex does not work, you can compile and install
  Hex directly with this command:

      $ mix archive.install github hexpm/hex branch latest

  ## Command line options

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like `make`

    * `--if-missing` - performs installation only if Hex is not installed yet;
      intended to avoid repeatedly reinstalling Hex in automation when a script
      may be run multiple times

  If both options are set, the shell prompt is skipped and Hex is not
  re-installed if it was already installed.

  ## Mirrors

  If you want to change the [default mirror](https://builds.hex.pm)
  used for fetching Hex, set the `HEX_BUILDS_URL` environment variable.
  """
  @switches [if_missing: :boolean, force: :boolean]
  @compile {:no_warn_undefined, {Hex, :version, 0}}

  @impl true
  def run(argv) do
    {opts, args} = OptionParser.parse!(argv, switches: @switches)
    version = List.first(args)

    should_install? =
      if Keyword.get(opts, :if_missing, false) do
        if version do
          not Code.ensure_loaded?(Hex) or Version.compare(Hex.version(), version) == :gt
        else
          not Code.ensure_loaded?(Hex)
        end
      else
        true
      end

    argv =
      if Keyword.get(opts, :force, false) do
        ["--force"]
      else
        []
      end

    should_install? && run_install(version, argv)
  end

  defp run_install(version, argv) do
    hex_url = Mix.Hex.url()

    {elixir_version, hex_version, sha512, otp_release} =
      Mix.Local.find_matching_versions!("Hex", version, hex_url <> @hex_list_path)

    url =
      (hex_url <> @hex_archive_path)
      |> String.replace("[ELIXIR_VERSION]", elixir_version)
      |> String.replace("[OTP_RELEASE]", otp_release)
      |> String.replace("[HEX_VERSION]", hex_version)

    # Unload the Hex module we loaded earlier to avoid conflicts when Hex is updated
    # and then used without restarting the VM
    :code.purge(Hex)

    Mix.Tasks.Archive.Install.run([url, "--sha512", sha512 | argv])
  end
end
