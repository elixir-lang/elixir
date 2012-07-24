defmodule Mix.Tasks.Clean do
  use Mix.Task

  @shortdoc "Clean generated application files"

  @moduledoc """
  Clean generated application files.

  ## Command line options

  * `--all` - Clean everything, including dependencies

  """
  def run(args) do
    { opts, _ } = OptionParser.Simple.parse(args)
    File.rm_rf Mix.project[:compile_path]  || "ebin"
    if opts[:all], do: Mix.Task.run("deps.clean")
  end
end