defmodule Mix.Tasks.Compile.Leex do
  use Mix.Task.Compiler
  alias Mix.Compilers.Erlang

  @recursive true
  @manifest "compile.leex"
  @switches [force: :boolean, verbose: :boolean, all_warnings: :boolean]

  # These options can't be controlled with :leex_options.
  @forced_opts [report: true, return: true]

  @moduledoc """
  Compiles Leex source files.

  When this task runs, it will check the modification time of every file, and
  if it has changed, the file will be compiled. Files will be
  compiled in the same source directory with a .erl extension.
  You can force compilation regardless of modification times by passing
  the `--force` option.

  ## Command line options

    * `--force` - forces compilation regardless of modification times

    * `--all-warnings` - prints warnings even from files that do not need to be
      recompiled

  ## Configuration

    * `:erlc_paths` - directories to find source files. Defaults to `["src"]`.

    * `:leex_options` - compilation options that apply
      to Leex's compiler.

      For a complete list of options, see `:leex.file/2`.
      Note that the `:report`, `:return_errors`, and `:return_warnings` options
      are overridden by this compiler, thus setting them has no effect.

  """

  @impl true
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: @switches)

    project = Mix.Project.config()

    source_paths = project[:erlc_paths]
    Mix.Compilers.Erlang.assert_valid_erlc_paths(source_paths)
    mappings = Enum.zip(source_paths, source_paths)

    options = project[:leex_options] || []

    unless is_list(options) do
      Mix.raise(":leex_options should be a list of options, got: #{inspect(options)}")
    end

    Erlang.compile(manifest(), mappings, :xrl, :erl, opts, fn input, output ->
      Erlang.ensure_application!(:parsetools, input)
      options = options ++ @forced_opts ++ [scannerfile: Erlang.to_erl_file(output)]
      :leex.file(Erlang.to_erl_file(input), options)
    end)
  end

  @impl true
  def manifests, do: [manifest()]

  defp manifest, do: Path.join(Mix.Project.manifest_path(), @manifest)

  @impl true
  def clean do
    Erlang.clean(manifest())
  end
end
