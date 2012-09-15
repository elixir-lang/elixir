defmodule Mix.Tasks.Deps.Get do
  use Mix.Task

  @shortdoc "Get all out of date dependencies"

  @moduledoc """
  Get all out of date dependencies, i.e. dependencies
  that are not available or have a wrong lock.
  """

  import Mix.Deps, only: [all: 2, by_name!: 1, format_dep: 1,
    deps_path: 1, check_lock: 2, out_of_date?: 1]

  def run([]) do
    finalize_get all(init, deps_getter(&1, &2))
  end

  def run(args) do
    finalize_get Enum.reduce by_name!(args), init, deps_getter(&1, &2)
  end

  defp init do
    { [], Mix.Deps.Lock.read }
  end

  defp finalize_get({ apps, lock }) do
    if apps == [] do
      Mix.shell.info "All dependencies up to date"
    else
      Mix.Deps.Lock.write(lock)
      Mix.Task.run "deps.compile", apps
    end
  end

  defp deps_getter(dep, { acc, lock }) do
    shell = Mix.shell
    dep = check_lock(dep, lock)

    if out_of_date?(dep) do
      Mix.Dep[app: app, scm: scm, opts: opts] = dep
      shell.info "* Getting #{format_dep(dep)}"

      old  = lock[app]
      opts = Keyword.put(opts, :lock, old)
      path = deps_path(dep)

      new = 
        if scm.available?(path, opts) do
          scm.update(path, opts)
        else
          File.mkdir_p!(path)
          scm.checkout(path, opts)
        end

      cond do
        !new ->
          { dep, { acc, lock } }
        new && old && new != old ->
          Mix.shell.error "  dependency is not set to lock #{inspect old}"
          { dep, { acc, lock } }
        true ->
          { dep, { [app|acc], Keyword.put(lock, app, new) } }
      end
    else
      { dep, { acc, lock } }
    end
  end
end