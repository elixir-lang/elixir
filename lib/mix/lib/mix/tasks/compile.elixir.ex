defmodule Mix.Tasks.Compile.Elixir do
  use Mix.Task

  @recursive true
  @manifest ".compile.elixir"

  @moduledoc """
  Compiles Elixir source files.

  Elixir is smart enough to recompile only files that have changed
  and their dependencies. This means if `lib/a.ex` is invoking
  a function defined over `lib/b.ex`, whenever `lib/b.ex` changes,
  `lib/a.ex` is also recompiled.

  Note it is important to recompile a file's dependencies as
  there are often compile time dependencies between them.

  ## Command line options

    * `--force` - forces compilation regardless of modification times
    * `--docs` (`--no-docs`) - attaches (or not) documentation to compiled modules
    * `--debug-info` (`--no-debug-info`) - attaches (or not) debug info to compiled modules
    * `--ignore-module-conflict` - does not emit warnings if a module was previously defined
    * `--warnings-as-errors` - treats warnings as errors and return a non-zero exit code
    * `--long-compilation-threshold N` - sets the "long compilation" threshold
      (in seconds) to `N` (see the docs for `Kernel.ParallelCompiler.files/2`)

  ## Configuration

    * `:elixirc_paths` - directories to find source files.
      Defaults to `["lib"]`.

    * `:elixirc_options` - compilation options that apply
      to Elixir's compiler, they are: `:ignore_module_conflict`,
      `:docs` and `:debug_info`. By default, uses the same
      defaults as `elixirc` and they can always be overridden from
      the command line according to the options above.

  """

  @switches [force: :boolean, docs: :boolean, warnings_as_errors: :boolean,
             ignore_module_conflict: :boolean, debug_info: :boolean,
             elixirc_paths: :keep, verbose: :boolean,
             long_compilation_threshold: :integer]

  @doc """
  Runs this task.
  """
  @spec run(OptionParser.argv) :: :ok | :noop
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: @switches)

    project = Mix.Project.config
    dest = Mix.Project.compile_path(project)
    srcs = project[:elixirc_paths]

    assert_valid_elixirc_paths(srcs)

    manifest = manifest()
    configs  = Mix.Project.config_files ++ Mix.Tasks.Compile.Erlang.manifests
    force    = opts[:force] || Mix.Utils.stale?(configs, [manifest])

    Mix.Compilers.Elixir.compile(manifest, srcs, dest, force, opts)
  end

  @doc """
  Returns Elixir manifests.
  """
  def manifests, do: [manifest()]
  defp manifest, do: Path.join(Mix.Project.manifest_path, @manifest)

  @doc """
  Cleans up compilation artifacts.
  """
  def clean do
    dest = Mix.Project.compile_path
    Mix.Compilers.Elixir.clean(manifest(), dest)
  end

  defp assert_valid_elixirc_paths(paths) do
    unless is_list(paths) do
      Mix.raise ":elixirc_paths should be a list of paths, got: #{inspect(paths)}"
    end
  end
end
