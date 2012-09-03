defmodule Mix.Tasks.Deps.Loadpaths do
  use Mix.Task

  import Mix.Deps, only: [all: 1, deps_path: 1]

  @hidden true
  @shortdoc "Load all dependencies paths"

  @moduledoc """
  Loads all dependencies. Invokes "deps.check" before
  unless `--no-check` is given.

  This task is not shown in `mix help` but it is part
  of mix public API and can be depended on.
  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args)

    unless opts[:no_check] do
      Mix.Task.run "deps.check"
    end

    Enum.each all(:ok), fn(dep) ->
      Code.prepend_path File.join deps_path(dep), "ebin"
    end
  end
end
