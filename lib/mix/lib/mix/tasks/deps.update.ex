defmodule Mix.Tasks.Deps.Update do
  use Mix.Task

  @shortdoc "Update dependencies"
  @recursive true

  @moduledoc """
  Update dependencies.

  By default, updates all dependencies. A list of deps can
  be given to update specific ones. Recompiles the given
  projects after updating.

  ## Command line options

  * `--no-compile` - skip compilation of dependencies
  """

  import Mix.Deps, only: [ all: 0, all: 2, available?: 1, by_name: 2,
                           depending: 2, format_dep: 1 ]

  def run(args) do
    { opts, rest } = OptionParser.parse(args, switches: [no_compile: :boolean])

    if rest != [] do
      all_deps = all
      deps = Enum.map by_name(rest, all_deps), check_unavailable!(&1)
      deps = deps ++ depending(deps, all_deps)
      { _, acc } = Enum.map_reduce deps, init, deps_updater(&1, &2)
    else
      acc = all(init, deps_updater(&1, &2))
    end

    finalize_update acc, opts[:no_compile]
  end

  defp init do
    { [], Mix.Deps.Lock.read }
  end

  defp finalize_update({ apps, lock }, no_compile) do
    Mix.Deps.Lock.write(lock)
    unless no_compile, do: Mix.Task.run "deps.compile", apps
  end

  defp deps_updater(dep, { acc, lock }) do
    if available?(dep) do
      Mix.Dep[app: app, scm: scm, opts: opts] = dep
      Mix.shell.info "* Updating #{format_dep(dep)}"

      lock =
        if latest = scm.update(opts) do
          Keyword.put(lock, app, latest)
        else
          lock
        end

      { Mix.Deps.update(dep), { [app|acc], lock } }
    else
      { dep, { acc, lock } }
    end
  end

  defp check_unavailable!(dep) do
    unless available?(dep) do
      raise Mix.Error, message: "Cannot update dependency #{dep.app} because " <>
        "it isn't available, run `mix deps.get` first"
    end
    dep
  end
end
