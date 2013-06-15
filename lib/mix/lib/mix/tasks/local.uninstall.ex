defmodule Mix.Tasks.Local.Uninstall do
  use Mix.Task

  @shortdoc "Uninstall local tasks"

  @moduledoc """
  Uninstall local tasks:

      mix local.uninstall task_name | package_name

  """

  def run(argv) do
    { _, argv } = OptionParser.parse(argv)
    Enum.each argv, do_uninstall(&1)
  end

  defp do_uninstall(task) do
    try do
      task_module = Mix.Task.get(task)
      if (package = in_package('#{task_module}.beam') |> Enum.first) do
        package = String.split(package, ".") |> Enum.first
        Mix.shell.info """
          The task #{task} is part of package #{package}.
          To uninstall this task, you will need to:
          
             mix local.uninstall #{package}
        """
      else
        File.rm! Path.join(Mix.Local.tasks_path, "#{task_module}.beam")
      end
    rescue
      Mix.NoTaskError -> 
        File.rm! Path.join(Mix.Local.tasks_path, "#{task}.ez")
    end
  end

  defp in_package(beam) do
    case :code.where_is_file(beam) do
      :non_existing -> []
      found ->
        list_to_binary(found) 
          |> Path.split |> Enum.filter(String.ends_with?(&1, ".ez"))
    end
  end
end
