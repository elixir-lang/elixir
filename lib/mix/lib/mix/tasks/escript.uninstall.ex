defmodule Mix.Tasks.Escript.Uninstall do
  use Mix.Task

  @shortdoc "Uninstall escripts"

  @moduledoc """
  Uninstall local escripts:

      mix escript.uninstall escript_name

  """
  @spec run(OptionParser.argv) :: :ok
  def run(argv) do
    Mix.Local.Installer.uninstall(argv, Mix.Local.escripts_path,
      item_name: "escript",
      item_plural: "escripts")
  end
end
