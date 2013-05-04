defmodule Mix.Tasks.Loadpaths do
  use Mix.Task

  @hidden true
  @shortdoc "Load the application and its dependencies paths"
  @recursive true

  @moduledoc """
  Load the application and its dependencies paths.

  ## Configuration

  * `:load_paths` extra load paths to be added.
    They are added with lower priority than the app ones.

  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args)

    unless opts[:no_deps] do
      Mix.Task.run "deps.loadpaths", args
    end

    paths = Mix.project[:load_paths] || []
    Enum.each paths, Code.prepend_path(&1)

    ebin  = Mix.project[:compile_path] || "ebin"
    Code.prepend_path(ebin)
  end
end