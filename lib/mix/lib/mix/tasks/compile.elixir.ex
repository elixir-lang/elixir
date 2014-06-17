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
  often there are compilation time dependencies in between them.

  ## Command line options

    * `--force`         - forces compilation regardless of modification times
    * `--no-docs`       - do not attach documentation to compiled modules
    * `--no-debug-info` - do not attach debug info to compiled modules
    * `--ignore-module-conflict`
                        - do not emit warnings if a module was previously defined
    * `--warnings-as-errors`
                        - treat warnings as errors and return a non-zero exit code

  ## Configuration

    * `:elixirc_paths` - directories to find source files.
      Defaults to `["lib"]`.

    * `:elixirc_options` - compilation options that apply
      to Elixir's compiler, they are: `:ignore_module_conflict`,
      `:docs` and `:debug_info`. By default, uses the same
      behaviour as Elixir.

  """

  @switches [force: :boolean, docs: :boolean, warnings_as_errors: :boolean,
             ignore_module_conflict: :boolean, debug_info: :boolean]

  @doc """
  Runs this task.
  """
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: @switches)

    project = Mix.Project.config
    srcs = project[:elixirc_paths]
    dest = Mix.Project.compile_path(project)

    manifest = manifest()
    configs  = Mix.Project.config_files ++ Mix.Tasks.Compile.Erlang.manifests

    force = opts[:force] || local_deps_changed?(manifest)
              || Mix.Utils.stale?(configs, [manifest])

    result = Mix.Compilers.Elixir.compile(manifest, srcs, [:ex], dest, force, fn ->
      Code.prepend_path(dest)
      set_compiler_opts(project, opts, [])
    end)

    # The Mix.Dep.Lock keeps all the project dependencies. Since Elixir
    # is a dependency itself, we need to touch the lock so the current
    # Elixir version, used to compile the files above, is properly stored.
    unless result == :noop, do: Mix.Dep.Lock.touch
    result
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

  defp set_compiler_opts(project, opts, extra) do
    opts = Dict.take(opts, Code.available_compiler_options)
    opts = Keyword.merge(project[:elixirc_options] || [], opts)
    Code.compiler_options Keyword.merge(opts, extra)
  end

  defp local_deps_changed?(manifest) do
    manifest = Path.absname(manifest)

    Enum.any?(Mix.Dep.children([]), fn(dep) ->
      not dep.scm.fetchable? and Mix.Dep.in_dependency(dep, fn(_) ->
        files = Mix.Project.config_files ++ Mix.Tasks.Compile.manifests
        Mix.Utils.stale?(files, [manifest])
      end)
    end)
  end
end
