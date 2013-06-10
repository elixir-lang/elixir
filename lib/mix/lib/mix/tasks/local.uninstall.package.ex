defmodule Mix.Tasks.Local.Uninstall.Package do
  use Mix.Task

  @shortdoc "Uninstall local task package"

  @moduledoc """
  Uninstall local task package:
  
      mix local.uninstall package_name

  """

  def run(argv) do
    { _, argv } = OptionParser.parse(argv)
    Enum.each argv, do_uninstall(&1)
  end

  defp do_uninstall(package) do
    File.rm! Path.join(Mix.Local.tasks_path, "#{package}.ez")
  end
end
