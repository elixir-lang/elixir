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
    entries      = Mix.Utils.read_manifest(manifest())

    { stale, removed } =
      Enum.reduce(source_paths, { [], [] }, fn
        path, { acc1, acc2 } ->
          { stale, removed } = Erlang.extract_stale_pairs(entries, path, :yrl, path, :erl, opts[:force])
          { stale ++ acc1, removed ++ acc2 }
      end)

    if stale == [] && removed == [] do
      :noop
    else
      Enum.each(removed, File.rm(&1))
      compile_files(entries -- removed, stale, project[:yecc_options] || [])
      :ok
    end
  end

  @doc """
  Returns Yecc manifest.
  """
  def manifest do
    Path.join(Mix.project[:compile_path], @manifest)
  end

  defp compile_files(entries, files, options) do
    results = lc { input, output } inlist files do
      options = options ++ [parserfile: Erlang.to_erl_file(output), report: true]
      Erlang.interpret_result(input, :yecc.file(Erlang.to_erl_file(input), options))
    end

    outputs = entries ++ Enum.map(files, elem(&1, 1))
    Mix.Utils.write_manifest(manifest(), :lists.usort(outputs))

    if Enum.any?(results, &1 == :error), do: raise CompileError
  end
end
