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
    entries      = Mix.Utils.read_manifest(manifest())

    { stale, removed } =
      Enum.reduce(source_paths, { [], [] }, fn
        path, { acc1, acc2 } ->
          { stale, removed } = Erlang.extract_stale_pairs(entries, path, :xrl, path, :erl, opts[:force])
          { stale ++ acc1, removed ++ acc2 }
      end)

    if stale == [] && removed == [] do
      :noop
    else
      Enum.each(removed, File.rm(&1))
      compile_files(entries -- removed, stale, project[:leex_options] || [])
      :ok
    end
  end

  @doc """
  Returns Leex manifest.
  """
  def manifest do
    Path.join(Mix.project[:compile_path], @manifest)
  end

  defp compile_files(entries, files, options) do
    results = lc { input, output } inlist files do
      options = options ++ [scannerfile: Erlang.to_erl_file(output), report: true]
      Erlang.interpret_result(input, :leex.file(Erlang.to_erl_file(input), options))
    end

    outputs = entries ++ Enum.map(files, elem(&1, 1))
    Mix.Utils.write_manifest(manifest(), :lists.usort(outputs))

    if Enum.any?(results, &1 == :error), do: raise CompileError
  end
end

