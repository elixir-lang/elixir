defmodule Mix.Tasks.Clean do
  use Mix.Task

  @shortdoc "Clean generated application files"
  @recursive true

  @moduledoc """
  Clean generated application files.

  ## Command line options

  * `--all` - Clean everything, including builds and dependencies

  """

  def run(args) do
    { opts, _, _ } = OptionParser.parse(args)

    manifests = Mix.Tasks.Compile.manifests
    Enum.each(manifests, fn(manifest) ->
      Enum.each Mix.Utils.read_manifest(manifest),
                &(&1 |> String.split("\t") |> hd |> File.rm)
      File.rm(manifest)
    end)

    if opts[:all] do
      Mix.Task.run("deps.clean", args)
      File.rm_rf(Path.dirname(Mix.Project.app_path))
    else
      File.rm_rf(Mix.Project.app_path)
    end
  end
end
