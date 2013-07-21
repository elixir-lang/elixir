defmodule Mix.Tasks.Compile.Yecc do
  alias Mix.Tasks.Compile.Erlang

  use Mix.Task

  @hidden true
  @shortdoc "Compile Yecc source files"
  @recursive true
  @manifest ".compile.yecc"

  @moduledoc """
  A task to compile Yecc source files.

  When this task runs, it will check the modification time of every file, and
  if it has changed, the file will be compiled. Files will be
  compiled in the same source directory with a .erl extension.
  You can force compilation regardless of modification times by passing
  the `--force` option.

  ## Command line options

  * `--force` - forces compilation regardless of modification times;

  ## Configuration

  * `:erlc_paths` - directories to find source files.
    Defaults to `["src"]`, can be configured as:

    ```
    [erlc_paths: ["src", "other"]]
    ```

  * `:yecc_options` - compilation options that apply
     to Yecc's compiler. There are many other available
     options here: http://www.erlang.org/doc/man/yecc.html#file-1

  """

  @doc """
  Runs this task.
  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args, switches: [force: :boolean])

    project      = Mix.project
    source_paths = project[:erlc_paths]
    mappings     = Enum.zip(source_paths, source_paths)
    options      = project[:yecc_options] || []

    Erlang.compile_mappings(manifest(), mappings, :yrl, :erl, opts[:force], fn
      input, output ->
        options = options ++ [parserfile: Erlang.to_erl_file(output), report: true]
        :yecc.file(Erlang.to_erl_file(input), options)
    end)
  end

  @doc """
  Returns Yecc manifest.
  """
  def manifest do
    Path.join(Mix.project[:compile_path], @manifest)
  end
end
