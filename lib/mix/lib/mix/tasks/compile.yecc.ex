defmodule Mix.Tasks.Compile.Yecc do
  use Mix.Task
  alias Mix.Compilers.Erlang

  @recursive true
  @manifest ".compile.yecc"

  @moduledoc """
  Compiles Yecc source files.

  When this task runs, it will check the modification time of every file, and
  if it has changed, the file will be compiled. Files will be
  compiled in the same source directory with a .erl extension.
  You can force compilation regardless of modification times by passing
  the `--force` option.

  ## Command line options

    * `--force` - forces compilation regardless of modification times

  ## Configuration

    * `:erlc_paths` - directories to find source files. Defaults to `["src"]`.

    * `:yecc_options` - compilation options that apply
      to Yecc's compiler.

      For a list of the many more available options,
      see [`:yecc.file/1`](http://www.erlang.org/doc/man/yecc.html#file-1).
  """

  @doc """
  Runs this task.
  """
  @spec run(OptionParser.argv) :: :ok | :noop
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: [force: :boolean])

    project      = Mix.Project.config
    source_paths = project[:erlc_paths]
    mappings     = Enum.zip(source_paths, source_paths)
    options      = project[:yecc_options] || []

    Erlang.compile(manifest(), mappings, :yrl, :erl, opts[:force], fn
      input, output ->
        Erlang.ensure_application!(:parsetools, input)
        options = options ++ [parserfile: Erlang.to_erl_file(output), report: true]
        :yecc.file(Erlang.to_erl_file(input), options)
    end)
  end

  @doc """
  Returns Yecc manifests.
  """
  def manifests, do: [manifest]
  defp manifest, do: Path.join(Mix.Project.manifest_path, @manifest)

  @doc """
  Cleans up compilation artifacts.
  """
  def clean do
    Erlang.clean(manifest())
  end
end
