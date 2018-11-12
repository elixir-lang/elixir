defmodule Mix.Tasks.Archive.Uninstall do
  use Mix.Task

  @shortdoc "Uninstalls archives"

  @moduledoc """
  Uninstalls local archives.

      mix archive.uninstall archive.ez

  ## Command line options
    * `--force` - forces uninstallation without a shell prompt; primarily
      intended for automation
  """

  @switches [
    force: :boolean
  ]

  @impl true
  def run(argv) do
    Mix.Local.Installer.uninstall(Mix.Local.path_for(:archive), "archive", argv, @switches)
  end
end
