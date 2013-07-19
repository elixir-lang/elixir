defmodule Mix.Tasks.Clean do
  use Mix.Task
  alias Mix.Tasks.Compile.Leex
  alias Mix.Tasks.Compile.Yecc

  @shortdoc "Clean generated application files"
  @recursive true

  @moduledoc """
  Clean generated application files.

  ## Command line options

  * `--all` - Clean everything, including dependencies

  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args)

    manifests = Mix.Tasks.Compile.manifests
    Enum.each(manifests, fn(manifest) ->
      Mix.Utils.read_manifest(manifest) |> Enum.each(File.rm(&1))
      File.rm(manifest)
    end)

    File.rm_rf(Mix.project[:compile_path])

    if opts[:all], do: Mix.Task.run("deps.clean")
  end
end
