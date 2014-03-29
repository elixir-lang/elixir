defmodule Mix.Tasks.Local.Uninstall do
  use Mix.Task

  @shortdoc "Uninstall local tasks or archives"

  @moduledoc """
  Uninstall local tasks:

      mix local.uninstall archive

  """

  def run(argv) do
    { _, argv, _ } = OptionParser.parse(argv)
    Enum.each argv, &do_uninstall(&1)
  end

  defp do_uninstall(name) do
    archives = Mix.Local.archive_files(name)

    if archives == [] do
      raise Mix.Error, message: "Could not find a local archive named #{inspect name} "<>
        "at #{inspect Mix.Local.archives_path}"
    end

    Enum.each(archives, &File.rm!(&1))
  end
end
