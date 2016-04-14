defmodule Mix.Tasks.Deps.Precompile do
  use Mix.Task

  @moduledoc """
  Extension point for precompiling dependencies.

  This is a task that can be aliased by projects
  that need to execute certain tasks to before
  compiling dependencies:

      aliases: ["deps.precompile": ["nerves.precompile", "deps.precompile"]]

  By default, this task has a single responsibility
  of loading all dependencies paths. The dependency
  loading is ad-hoc on purpose, to load as much as
  it can without validating it.
  """
  def run(_) do
    config = Mix.Project.config
    Mix.Project.build_path(config)
    |> Path.join("lib/*/ebin")
    |> Path.wildcard
    |> List.delete(config[:app] && Mix.Project.compile_path(config))
    |> Enum.each(&Code.prepend_path/1)
  end
end
