defmodule Mix.Tasks.Escript.Uninstall do
  use Mix.Task

  @shortdoc "Uninstalls escripts"

  @moduledoc """
  Uninstalls local escripts:

      mix escript.uninstall escript_name

  """

  @switches []

  def run(argv) do
    if path =
         Mix.Local.Installer.uninstall(Mix.Local.path_for(:escript), "escript", argv, @switches) do
      File.rm(path <> ".bat")
    end
  end
end
