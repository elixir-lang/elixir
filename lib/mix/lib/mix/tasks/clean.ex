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

    compile_path = Mix.project[:compile_path]

    manifests = [ Leex.manifest, Yecc.manifest ]
    Enum.each(manifests, fn(manifest) ->
      manifest_path = Path.join(compile_path, manifest)
      Mix.Utils.read_manifest(manifest_path) |> Enum.each(File.rm(&1))
    end)

    File.rm_rf(compile_path)

    if opts[:all], do: Mix.Task.run("deps.clean")
  end
end
