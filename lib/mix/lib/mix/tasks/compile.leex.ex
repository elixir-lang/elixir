defmodule Mix.Tasks.Compile.Leex do
  alias :leex, as: Leex
  alias Mix.Tasks.Compile.Erlang

  use Mix.Task

  @hidden true
  @shortdoc "Compile Leex source files"
  @recursive true

  @moduledoc """
  A task to compile Leex source files.

  When this task runs, it will check the mod time of every file, and
  if it has changed, then file will be compiled. Files will be
  compiled in the same source directory with .erl extension.
  You can force compilation regardless of mod times by passing
  the `--force` option.

  ## Command line options

  * `--force` - forces compilation regardless of module times;

  ## Configuration

  * `:erlc_paths` - directories to find source files.
    Defaults to `["src"]`, can be configured as:

        [erlc_paths: ["src", "other"]]

  * `:leex_options` - compilation options that applies
     to Leex's compiler. There are many available options
     here: http://www.erlang.org/doc/man/leex.html#file-2

  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args, switches: [force: :boolean])

    project      = Mix.project
    source_paths = project[:erlc_paths]

    files = lc source_path inlist source_paths do
              Erlang.extract_stale_pairs(source_path, :xrl, source_path, :erl, opts[:force])
            end |> List.flatten

    if files == [] do
      :noop
    else
      compile_files(files, project[:leex_options] || [])
      :ok
    end
  end

  defp compile_files(files, options) do
    lc {input, output} inlist files do
      options = options ++ [scannerfile: Erlang.to_erl_file(output), report: true]
      Erlang.interpret_result(input,
        Leex.file(Erlang.to_erl_file(input), options))
    end
  end
end

