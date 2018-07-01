defmodule Mix.Tasks.Archive.Uninstall do
  use Mix.Task

  @shortdoc "Uninstalls archives"

  @moduledoc """
  Uninstalls local archives.

      mix archive.uninstall archive.ez

  ## Command line options

    * `--sha512` - checks the archive matches the given SHA-512 checksum. Only
      applies to installations via URL or local path

    * `--force` - forces uninstallation without a shell prompt; primarily
      intended for automation
  """
  @switches [
    force: :boolean
  ]
  def run(argv) do
    Mix.Local.Installer.uninstall(Mix.Local.path_for(:archive), "archive", argv, @switches)
  end
end
