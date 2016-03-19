defmodule Mix.Tasks.Deps.Precompile do
  use Mix.Task

  @moduledoc """
  Extension point for precompiling dependencies.

  This is a task that can be aliased by projects
  that need to execute certain tasks to before
  compiling dependencies:

      aliases: ["deps.precompile": ["nerves.precompile", "deps.precompile"]]

  By default, this task has a single responsibility
  of loading all dependencies paths.
  """
  def run(_) do
    Mix.Task.run "deps.loadpaths"
  end
end
