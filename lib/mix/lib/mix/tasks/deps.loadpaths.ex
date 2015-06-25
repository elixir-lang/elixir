defmodule Mix.Tasks.Deps.Loadpaths do
  use Mix.Task

  @moduledoc """
  Loads the available dependencies paths.
  """

  @spec run(OptionParser.argv) :: :ok
  def run(_) do
    config = Mix.Project.config
    Mix.Project.build_path(config)
    |> Path.join("lib/*/ebin")
    |> Path.wildcard
    |> List.delete(config[:app] && Mix.Project.compile_path(config))
    |> Enum.each(&Code.prepend_path/1)
  end
end
