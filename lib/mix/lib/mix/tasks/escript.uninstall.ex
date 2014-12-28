defmodule Mix.Tasks.Escript.Uninstall do
  use Mix.Task

  @shortdoc "Uninstall escripts"

  @moduledoc """
  Uninstall local escripts:

      mix escript.uninstall escript_name

  """
  @spec run(OptionParser.argv) :: :ok
  def run(argv) do
    {_, argv, _} = OptionParser.parse(argv)

    if name = List.first(argv) do
      path = Path.join(Mix.Local.escripts_path, name)
      if File.regular?(path) do
        File.rm!(path)
      else
        Mix.shell.error "Could not find a local escript named #{inspect name}. "<>
                        "Existing escripts are:"
        Mix.Task.run "escript"
      end
    else
      Mix.raise "No escript was given to escript.uninstall"
    end
  end
end
