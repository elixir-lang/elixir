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
    do_update all(&1, &2)
  end

  def run(args) do
    deps = Enum.map by_name!(args), check_unavailable!(&1)
    do_update Enum.reduce deps, &1, &2
  end

  defp do_update(fun) do
    lock = Mix.Deps.Lock.read
    { deps, new_lock } = fun.({ [], lock }, deps_getter(&1, &2))
    Mix.Deps.Lock.write(new_lock)
    to_compile = deps /> Enum.map(fn(dep) -> dep.app end) /> Enum.reverse
    Mix.Task.run "deps.compile", to_compile
  end

  defp deps_getter(dep, { acc, lock }) do
    if available?(dep) do
      Mix.Dep[app: app, scm: scm, opts: opts] = dep
      Mix.shell.info "* Updating #{format_dep(dep)}"

      lock = 
        if latest = scm.update(deps_path(dep), opts) do
          Keyword.put(lock, app, latest)
        else
          lock
        end
        
      { dep, { [dep|acc], lock } }
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