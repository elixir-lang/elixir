defmodule Mix.Tasks.Clean do
  use Mix.Task

  @shortdoc "Clean generated application files"
  @recursive true

  @moduledoc """
  Clean generated application files.

  ## Command line options

  * `--all` - Clean everything, including dependencies

  """
  def run(args) do
    { opts, _, _ } = OptionParser.parse(args)

    manifests = Mix.Tasks.Compile.manifests
    Enum.each(manifests, fn(manifest) ->
      Enum.each Mix.Utils.read_manifest(manifest),
                &1 |> String.split("\t") |> hd |> File.rm
      File.rm(manifest)
    end)

    File.rm_rf(Mix.project[:compile_path])

    if opts[:all], do: Mix.Task.run("deps.clean", args)
  end
end
