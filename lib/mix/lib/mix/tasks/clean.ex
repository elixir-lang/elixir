defmodule Mix.Tasks.Clean do
  use Mix.Task
  alias Mix.Tasks.Compile.Erlang

  @shortdoc "Clean generated application files"

  @moduledoc """
  Clean generated application files.

  ## Command line options

  * `--all` - Clean everything, including dependencies

  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args)
    File.rm_rf Mix.project[:compile_path]  || "ebin"

    Enum.each Mix.project[:erlc_paths], fn(source_path) ->
      pairs = Erlang.extract_stale_pairs(source_path, [:yrl, :xrl], source_path, :erl, true)
      Enum.each pairs, fn { _, output } -> File.rm_rf output end
    end

    if opts[:all], do: Mix.Task.run("deps.clean")
  end
end
