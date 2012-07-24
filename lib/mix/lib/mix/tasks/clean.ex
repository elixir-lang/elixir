defmodule Mix.Tasks.Clean do
  use Mix.Task

  @shortdoc "Clean generated application files"

  @moduledoc """
  Clean generated application files.

  ## Command line options

  * `--deps` - Also remove all dependencies files.
  """
  def run(args) do
    { opts, _ } = OptionParser.Simple.parse(args)
    File.rm_rf Mix.project[:compile_path]  || "ebin"
    if opts[:deps], do: Mix.Task.run("deps.clean")
  end
end