defmodule Mix.Tasks.Deps.Loadpaths do
  use Mix.Task

  import Mix.Deps, only: [all: 0, ok?: 1]

  @hidden true
  @shortdoc "Load all dependencies paths"

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
      Code.prepend_path File.join dep.opts[:path], "ebin"
    end
  end
end
