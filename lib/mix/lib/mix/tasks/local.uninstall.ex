defmodule Mix.Tasks.Local.Uninstall do
  use Mix.Task

  @shortdoc "Uninstall local tasks or archives"

  @moduledoc """
  Uninstall local tasks:

      mix local.uninstall task_name | archive.ez

  """

  def run(argv) do
    { _, argv } = OptionParser.parse(argv)
    Enum.each argv, do_uninstall(&1)
  end

  defp do_uninstall(task) do
    case Path.extname(task) do
      ".ez" -> File.rm! Path.join(Mix.Local.archives_path, task)
      _ ->
        task_module = Mix.Task.get(task)
        archive = in_archive('#{task_module}.beam')

        if archive && Mix.shell.yes?("The task #{task} is part of archive #{archive}. " <>
             "Do you want to remove this archive?") do
          do_uninstall(archive)
        else
          raise Mix.Error, message: "Cannot uninstall task #{task}"
        end
    end
  end

  defp in_archive(beam) do
    case :code.where_is_file(beam) do
      :non_existing -> []
      found ->
        :unicode.characters_to_binary(found)
          |> Path.split
          |> Enum.find(String.ends_with?(&1, ".ez"))
    end
  end
end
