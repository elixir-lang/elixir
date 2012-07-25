defmodule Mix.Tasks.Local.Uninstall do
  use Mix.Task

  @shortdoc "Uninstall local tasks"

  @moduledoc """
  Uninstall local tasks:

      mix local.uninstall task_name

  """

  def run(argv) do
    { _, argv } = OptionParser.Simple.parse(argv)
    Enum.each argv, do_uninstall(&1)
  end

  defp do_uninstall(task) do
    task = Mix.Task.get(task)
    File.rm! File.join(Mix.Utils.home, ".mix/tasks/#{task}.beam")
  end
end