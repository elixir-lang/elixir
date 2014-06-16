defmodule Mix.Tasks.Deps.Loadpaths do
  use Mix.Task

  @moduledoc """
  Loads all dependencies for the current build.
  This is invoked directly by `loadpaths` when
  the CLI boots.

  ## Command line options

    * `--no-deps-check` - skip dependency check
  """
  def run(args) do
    unless "--no-deps-check" in args do
      Mix.Task.run "deps.check", args
    end

    config = Mix.Project.config

    Mix.Project.build_path(config)
    |> Path.join("lib/*/ebin")
    |> Path.wildcard
    |> List.delete(not Mix.Project.umbrella? && Mix.Project.compile_path(config))
    |> Enum.each(&Code.prepend_path/1)
  end
end
