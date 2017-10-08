defmodule Mix.Tasks.Escript.Uninstall do
  use Mix.Task

  @shortdoc "Uninstalls escripts"

  @moduledoc """
  Uninstalls local escripts:

      mix escript.uninstall escript_name

  """

  def run(argv) do
    if path = Mix.Local.Installer.uninstall(Mix.Local.path_for(:escript), "escript", argv) do
      File.rm(path <> ".bat")
    end
  end
end
