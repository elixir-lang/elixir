defmodule Mix.Tasks.Deps.Loadpaths do
  use Mix.Task

  import Mix.Deps, only: [all: 0, load_paths: 1, ok?: 1]

  @hidden true
  @shortdoc "Load all dependencies paths"
  @recursive true

  @moduledoc """
  Loads all dependencies. This is invoked directly
  by "loadpaths" when the CLI boots.
  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args)

    unless opts[:no_check] do
      Mix.Task.run "deps.check"
    end

    lc dep inlist all, ok?(dep) do
      Enum.each load_paths(dep), Code.prepend_path(&1)
    end
  end
end
