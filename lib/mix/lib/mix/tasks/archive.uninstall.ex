defmodule Mix.Tasks.Archive.Uninstall do
  use   Mix.Task
  alias Mix.Local.Target.Archive, as: TargetArchive

  @shortdoc "Uninstalls archives"

  @moduledoc """
  Uninstalls local archives.

      mix archive.uninstall archive.ez

  """
  @spec run(OptionParser.argv) :: :ok
  def run(argv) do
    Mix.Local.Installer.uninstall(TargetArchive, argv)
  end
end
