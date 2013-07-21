defmodule Mix.Tasks.Compile.Leex do
  alias Mix.Tasks.Compile.Erlang

  use Mix.Task

  @hidden true
  @shortdoc "Compile Leex source files"
  @recursive true
  @manifest ".compile.leex"

  @moduledoc """
  A task to compile Leex source files.

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

  * `:leex_options` - compilation options that apply
     to Leex's compiler. There are many available options
     here: http://www.erlang.org/doc/man/leex.html#file-2

  """

  @doc """
  Runs this task.
  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args, switches: [force: :boolean])

    project      = Mix.project
    source_paths = project[:erlc_paths]
    mappings     = Enum.zip(source_paths, source_paths)
    options      = project[:leex_options] || []

    Erlang.compile_mappings(manifest(), mappings, :xrl, :erl, opts[:force], fn
      input, output ->
        options = options ++ [scannerfile: Erlang.to_erl_file(output), report: true]
        :leex.file(Erlang.to_erl_file(input), options)
    end)
  end

  @doc """
  Returns Leex manifest.
  """
  def manifest do
    Path.join(Mix.project[:compile_path], @manifest)
  end
end
