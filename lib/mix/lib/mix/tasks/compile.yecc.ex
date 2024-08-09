defmodule Mix.Tasks.Compile.Yecc do
  use Mix.Task.Compiler
  alias Mix.Compilers.Erlang

  @recursive true
  @manifest "compile.yecc"
  @switches [force: :boolean, all_warnings: :boolean]

  # These options can't be controlled with :yecc_options.
  @forced_opts [report: true, return: true]

  @moduledoc """
  Compiles Yecc source files.

  When this task runs, it will check the modification time of every file, and
  if it has changed, the file will be compiled. Files will be
  compiled in the same source directory with a .erl extension.
  You can force compilation regardless of modification times by passing
  the `--force` option.

  You must add `compilers: [:yecc] ++ Mix.compilers()` in the
  `def project` section of your `mix.exs` to run this compiler.

  ## Command line options

    * `--all-warnings` (`--no-all-warnings`) - prints all warnings, including previous compilations
      (default is true except on errors)
    * `--force` - forces compilation regardless of modification times

  ## Configuration

    * `:erlc_paths` - directories to find source files. Defaults to `["src"]`.

    * `:yecc_options` - compilation options that apply
      to Yecc's compiler.

      For a complete list of options, see `:yecc.file/1`.
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

    yecc_options = project[:yecc_options] || []

    if not is_list(yecc_options) do
      Mix.raise(":yecc_options should be a list of options, got: #{inspect(yecc_options)}")
    end

    opts = [parallel: true, preload: fn -> preload(project) end] ++ opts

    Erlang.compile(manifest(), mappings, :yrl, :erl, opts, fn input, output ->
      options = yecc_options ++ @forced_opts ++ [parserfile: Erlang.to_erl_file(output)]
      :yecc.file(Erlang.to_erl_file(input), options)
    end)
  end

  defp preload(project) do
    # TODO: Remove me in Elixir v2.0
    if :yecc not in List.wrap(project[:compilers]) do
      Mix.shell().error(
        "warning: in order to compile .yrl files, you must add \"compilers: [:yecc] ++ Mix.compilers()\" to the \"def project\" section of #{project[:app]}'s mix.exs"
      )
    end

    Mix.ensure_application!(:parsetools)
    {:ok, _} = Application.ensure_all_started(:parsetools)
  end

  @impl true
  def manifests, do: [manifest()]
  defp manifest, do: Path.join(Mix.Project.manifest_path(), @manifest)

  @impl true
  def clean do
    Erlang.clean(manifest())
  end
end
