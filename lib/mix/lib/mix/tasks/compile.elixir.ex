defmodule Mix.Tasks.Compile.Elixir do
  use Mix.Task.Compiler

  @recursive true
  @manifest "compile.elixir"

  @moduledoc """
  Compiles Elixir source files.

  Elixir is smart enough to recompile only files that have changed
  and their dependencies. This means if `lib/a.ex` is invoking
  a function defined over `lib/b.ex`, whenever `lib/b.ex` changes,
  `lib/a.ex` is also recompiled.

  Note it is important to recompile a file's dependencies as
  there are often compile time dependencies between them.

  ## `__mix_recompile__?/0`

  A module may export a `__mix_recompile__?/0` function that can
  cause the module to be recompiled using custom rules. For example,
  `@external_resource` already adds a compile-time dependency on an
  external file, however to depend on a _dynamic_ list of files we
  can do:

      defmodule MyModule do
        paths = Path.wildcard("*.txt")
        @paths_hash :erlang.md5(paths)

        for path <- paths do
          @external_resource path
        end

        def __mix_recompile__?() do
          Path.wildcard("*.txt") |> :erlang.md5() != @paths_hash
        end
      end

  Compiler calls `__mix_recompile__?/0` for every module being
  compiled (or previously compiled) and thus it is very important
  to do there as little work as possible to not slow down the
  compilation.

  If module has `@compile {:autoload, false}`, `__mix_recompile__?/0` will
  not be used.

  ## Command line options

    * `--verbose` - prints each file being compiled
    * `--force` - forces compilation regardless of modification times
    * `--docs` (`--no-docs`) - attaches (or not) documentation to compiled modules
    * `--debug-info` (`--no-debug-info`) - attaches (or not) debug info to compiled modules
    * `--ignore-module-conflict` - does not emit warnings if a module was previously defined
    * `--warnings-as-errors` - treats warnings in the current project as errors and
      return a non-zero exit status
    * `--long-compilation-threshold N` - sets the "long compilation" threshold
      (in seconds) to `N` (see the docs for `Kernel.ParallelCompiler.compile/2`)
    * `--profile` - if set to `time`, outputs timing information of compilation steps
    * `--all-warnings` - prints warnings even from files that do not need to be recompiled
    * `--tracer` - adds a compiler tracer in addition to any specified in the `mix.exs` file

  ## Configuration

    * `:elixirc_paths` - directories to find source files.
      Defaults to `["lib"]`.

    * `:elixirc_options` - compilation options that apply to Elixir's compiler.
      See `Code.put_compiler_option/2` for a complete list of options. These
      options are often overridable from the command line using the switches
      above.

    * `[xref: [exclude: ...]]` - a list of `module` or `{module, function, arity}`
      that should not be warned on in case on undefined modules or undefined
      application warnings.

  """

  @switches [
    force: :boolean,
    docs: :boolean,
    warnings_as_errors: :boolean,
    ignore_module_conflict: :boolean,
    debug_info: :boolean,
    verbose: :boolean,
    long_compilation_threshold: :integer,
    profile: :string,
    all_warnings: :boolean,
    tracer: :keep
  ]

  @impl true
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: @switches)

    project = Mix.Project.config()
    dest = Mix.Project.compile_path(project)
    srcs = project[:elixirc_paths]

    unless is_list(srcs) do
      Mix.raise(":elixirc_paths should be a list of paths, got: #{inspect(srcs)}")
    end

    manifest = manifest()
    configs = [Mix.Project.config_mtime() | Mix.Tasks.Compile.Erlang.manifests()]
    force = opts[:force] || Mix.Utils.stale?(configs, [manifest])
    {tracers, opts} = pop_tracers(opts)

    opts =
      (project[:elixirc_options] || [])
      |> Keyword.merge(opts)
      |> xref_exclude_opts(project)
      |> tracers_opts(tracers)
      |> profile_opts()

    Mix.Compilers.Elixir.compile(manifest, srcs, dest, [:ex], force, opts)
  end

  @impl true
  def manifests, do: [manifest()]
  defp manifest, do: Path.join(Mix.Project.manifest_path(), @manifest)

  @impl true
  def clean do
    dest = Mix.Project.compile_path()
    Mix.Compilers.Elixir.clean(manifest(), dest)
  end

  defp xref_exclude_opts(opts, project) do
    exclude = List.wrap(project[:xref][:exclude])

    if exclude == [] do
      opts
    else
      Keyword.update(opts, :no_warn_undefined, exclude, &(List.wrap(&1) ++ exclude))
    end
  end

  defp pop_tracers(opts) do
    case Keyword.pop_values(opts, :tracer) do
      {[], opts} ->
        {[], opts}

      {tracers, opts} ->
        {Enum.map(tracers, &Module.concat([&1])), opts}
    end
  end

  defp tracers_opts(opts, tracers) do
    tracers = tracers ++ Code.get_compiler_option(:tracers)
    Keyword.update(opts, :tracers, tracers, &(tracers ++ &1))
  end

  defp profile_opts(opts) do
    case Keyword.fetch(opts, :profile) do
      {:ok, "time"} -> Keyword.put(opts, :profile, :time)
      {:ok, _} -> Keyword.delete(opts, :profile)
      :error -> opts
    end
  end
end
