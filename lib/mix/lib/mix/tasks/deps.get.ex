defmodule Mix.Tasks.Deps.Get do
  use Mix.Task

  @shortdoc "Get all out of date dependencies"

  @moduledoc """
  Get all out of date dependencies, i.e. dependencies
  that are not available or have an invalid lock.

  ## Command line options

  * `--no-compile` - skip compilation of dependencies
  * `--no-deps-check` - skip dependency check
  * `--quiet` - do not output verbose messages

  """

  import Mix.Deps, only: [unloaded: 2, unloaded_by_name: 3, format_dep: 1,
                          check_lock: 2, out_of_date?: 1]

  def run(args) do
    Mix.Project.get! # Require the project to be available
    { opts, rest, _ } = OptionParser.parse(args, switches: [no_compile: :boolean, quiet: :boolean])

    acc =
      if rest != [] do
        unloaded_by_name(rest, init, &deps_getter/2)
      else
        unloaded(init, &deps_getter/2)
      end

    finalize_get(acc, opts)
  end

  defp init do
    { [], Mix.Deps.Lock.read }
  end

  defp finalize_get({ all_deps, { apps, lock } }, opts) do
    Mix.Deps.finalize(all_deps, apps, lock, opts)

    if apps == [] && !opts[:quiet] do
      Mix.shell.info "All dependencies up to date"
    end
  end

  defp deps_getter(dep, { acc, lock }) do
    shell = Mix.shell
    Mix.Dep[app: app, scm: scm] = dep = check_lock(dep, lock)

    cond do
      # Path dependencies are specially handled because they cannot
      # be fetched although they are always compiled afterwards
      scm == Mix.SCM.Path ->
        { dep, { [app|acc], lock } }

      # If the dependency is not available or we have a lock mismatch
      out_of_date?(dep) ->
        shell.info "* Getting #{format_dep(dep)}"

        # If the lock is outdated, don't dare including it in the opts
        opts =
          if dep.status == :lockoutdated do
            dep.opts
          else
            Keyword.put(dep.opts, :lock, lock[app])
          end

        new =
          if scm.checked_out?(opts) do
            scm.update(opts)
          else
            scm.checkout(opts)
          end

        if new do
          { dep, { [app|acc], Keyword.put(lock, app, new) } }
        else
          { dep, { acc, lock } }
        end

      # The dependency is ok or has some other error
      true ->
        { dep, { acc, lock } }
    end
  end
end
