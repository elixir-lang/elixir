defmodule Mix.Tasks.Compile.Yecc do

  alias :compile, as: Compiler
  alias :yecc, as: Yecc
  alias Mix.Utils
  alias Mix.Tasks.Compile.Erlang
  use Mix.Task

  @hidden true
  @shortdoc "Compile Yecc source files"
  @recursive true

  @moduledoc """
  A task to compile Yecc source files.

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

  * `:yecc_options` - compilation options that applies
     to Yecc's compiler. There are many other available
     options here: http://www.erlang.org/doc/man/yecc.html#file-1

  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args, switches: [force: :boolean])

    project = Mix.project
    source_paths = project[:erlc_paths]

    files = lc source_path inlist source_paths do
              Erlang.extract_stale_pairs(source_path, :yrl, source_path, :erl, opts[:force])
            end |> List.flatten

    if files == [] do
      :noop
    else
      compile_files(files, project[:yecc_options] || [])
      :ok
    end
  end

  defp compile_files(files, options) do
    lc { input, output } inlist files do
      options = options ++ [parserfile: Erlang.to_erl_file(output), report: true]
      Erlang.interpret_result(input,
        Yecc.file(Erlang.to_erl_file(input), options))
    end
  end
end
