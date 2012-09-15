defmodule Mix.Tasks.Deps.Update do
  use Mix.Task

  @shortdoc "Update dependencies"

  @moduledoc """
  Update dependencies.

  By default, updates all dependencies. A list of deps can
  be given to update specific ones. Recompiles the given
  projects after updating.
  """

  import Mix.Deps, only: [all: 2, available?: 1, by_name!: 1, format_dep: 1, deps_path: 1]

  def run([]) do
    finalize_update all(init, deps_updater(&1, &2))
  end

  def run(args) do
    deps = Enum.map by_name!(args), check_unavailable!(&1)
    finalize_update Enum.reduce deps, init, deps_updater(&1, &2)
  end

  defp init do
    { [], Mix.Deps.Lock.read }
  end

  defp finalize_update({ apps, lock }) do
    Mix.Deps.Lock.write(lock)
    Mix.Task.run "deps.compile", apps
  end

  defp deps_updater(dep, { acc, lock }) do
    if available?(dep) do
      Mix.Dep[app: app, scm: scm, opts: opts] = dep
      Mix.shell.info "* Updating #{format_dep(dep)}"

      lock = 
        if latest = scm.update(deps_path(dep), opts) do
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