defmodule Mix.Tasks.Archive.Uninstall do
  use Mix.Task

  @shortdoc "Uninstalls archives"

  @moduledoc """
  Uninstalls local archives.

      mix archive.uninstall archive.ez

  """
  @spec run(OptionParser.argv) :: :ok
  def run(argv) do
    {_, argv, _} = OptionParser.parse(argv)

    if name = List.first(argv) do
      path = Path.join(Mix.Local.archives_path, name)
      if File.regular?(path) do
        File.rm!(path)
      else
        Mix.shell.error "Could not find a local archive named #{inspect name}. "<>
                        "Existing archives are:"
        Mix.Task.run "archive"
      end
    else
      Mix.raise "No archive was given to archive.uninstall"
    end
  end
end
