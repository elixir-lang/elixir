defmodule Mix.Tasks.Clean do
  use Mix.Task

  @shortdoc "Clean generated application files"

  @moduledoc """
  Clean generated application files.

  ## Command line options

  * `--all` - Clean everything, including dependencies

  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args)
    File.rm_rf Mix.project[:compile_path]  || "ebin"

    generated_source = lc source_path inlist Mix.project[:erlc_paths] do
                         Mix.Utils.check_files(source_path, [:yrl, :xrl], source_path, :erl)
                       end |> List.flatten
    lc {_, output} inlist generated_source, do: File.rm_rf output
    if opts[:all], do: Mix.Task.run("deps.clean")
  end
end
