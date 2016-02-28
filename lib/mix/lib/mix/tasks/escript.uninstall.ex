defmodule Mix.Tasks.Escript.Uninstall do
  use Mix.Task

  @shortdoc "Uninstall escripts"

  @moduledoc """
  Uninstall local escripts:

      mix escript.uninstall escript_name

  """
  @spec run(OptionParser.argv) :: :ok
  def run(argv) do
    if path = Mix.Local.Installer.uninstall(:escript, argv) do
      File.rm(path <> ".bat")
    end
  end
end
