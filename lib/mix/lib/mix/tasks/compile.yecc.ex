defmodule Mix.Tasks.Compile.Yecc do
  alias Mix.Tasks.Compile.Erlang

  use Mix.Task

  @hidden true
  @shortdoc "Compile Yecc source files"
  @recursive true
  @manifest ".compile.yecc"

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

    project      = Mix.project
    source_paths = project[:erlc_paths]
    compile_path = project[:compile_path]

    files = lc source_path inlist source_paths do
              Erlang.extract_stale_pairs(source_path, :yrl, source_path, :erl, opts[:force])
            end |> List.flatten

    if files == [] and not opts[:force] do
      :noop
    else
      compile_files(files, compile_path, project[:yecc_options] || [])
      :ok
    end
  end

  def manifest do
    @manifest
  end

  defp compile_files(files, compile_path, options) do
    manifest_path = Path.join(compile_path, @manifest)
    Mix.Utils.read_manifest(manifest_path) |> Enum.each(File.rm(&1))

    lc { input, output } inlist files do
      options = options ++ [parserfile: Erlang.to_erl_file(output), report: true]
      Erlang.interpret_result(input,
        :yecc.file(Erlang.to_erl_file(input), options))
    end

    outputs = Enum.map(files, elem(&1, 1))
    Mix.Utils.update_manifest(manifest_path, outputs)
  end
end
