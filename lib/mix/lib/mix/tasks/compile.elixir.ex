defmodule Mix.Tasks.Compile.Elixir do
  use Mix.Task

  @recursive true
  @manifest ".compile.elixir"

  @moduledoc """
  Compiles Elixir source files.

  Elixir is smart enough to recompile only files that changed
  and their dependencies. This means if `lib/a.ex` is invoking
  a function defined over `lib/b.ex`, whenever `lib/b.ex` changes,
  `lib/a.ex` is also recompiled.

  Note it is important to recompile a file dependencies because
  often there are compilation time dependencies between them.

  ## Command line options

    * `--force` - forces compilation regardless of modification times
    * `--docs` (`--no-docs`) - attach (or not) documentation to compiled modules
    * `--debug-info` (`--no-debug-info`) - attach (or not) debug info to compiled modules
    * `--ignore-module-conflict` - do not emit warnings if a module was previously defined
    * `--warnings-as-errors` - treat warnings as errors and return a non-zero exit code
    * `--elixirc-paths` - restrict the original `elixirc` paths to
      a subset of the ones specified. Can be given multiple times.

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
             elixirc_paths: :keep]

  @doc """
  Runs this task.
  """
  @spec run(OptionParser.argv) :: :ok | :noop
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: @switches)

    project = Mix.Project.config
    dest = Mix.Project.compile_path(project)
    srcs = project[:elixirc_paths]
    skip =
      case Keyword.get_values(opts, :elixirc_paths) do
        [] -> []
        ep -> srcs -- ep
      end

    manifest = manifest()
    configs  = Mix.Project.config_files ++ Mix.Tasks.Compile.Erlang.manifests
    force    = opts[:force] || Mix.Utils.stale?(configs, [manifest])

    Mix.Compilers.Elixir.compile(manifest, srcs, skip, [:ex], dest, force, fn ->
      set_compiler_opts(project, opts, [])
    end)
  end

  @doc """
  Returns Elixir manifests.
  """
  def manifests, do: [manifest]
  defp manifest, do: Path.join(Mix.Project.manifest_path, @manifest)

  @doc """
  Cleans up compilation artifacts.
  """
  def clean do
    Mix.Compilers.Elixir.clean(manifest())
  end

  @doc false
  def protocols_and_impls do
    Mix.Compilers.Elixir.protocols_and_impls(manifest())
  end

  defp set_compiler_opts(project, opts, extra) do
    opts = Keyword.take(opts, Code.available_compiler_options)
    opts = Keyword.merge(project[:elixirc_options] || [], opts)
    Code.compiler_options Keyword.merge(opts, extra)
  end
end
