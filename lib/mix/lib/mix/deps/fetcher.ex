# Module responsible for fetching (getting/updating)
# dependencies from their sources.
#
# The new_lock and old_lock mechanism exists to signal
# externally which dependencies need to be updated and
# which ones do not.
defmodule Mix.Deps.Fetcher do
  @moduledoc false

  import Mix.Deps, only: [format_dep: 1, check_lock: 2, out_of_date?: 1, available?: 1]

  @doc """
  Fetches all dependencies.
  """
  def all(old_lock, new_lock, opts) do
    { apps, _deps } = do_finalize Mix.Deps.unloaded({ [], new_lock }, &do_fetch/2), old_lock, opts
    apps
  end

  @doc """
  Fetches the dependencies with the given names and their children recursively.
  """
  def by_name(names, old_lock, new_lock, opts) do
    { apps, deps } = do_finalize Mix.Deps.unloaded_by_name(names, { [], new_lock }, &do_fetch/2), old_lock, opts
    Mix.Deps.loaded_by_name(names, deps) # Check all given dependencies are loaded or fail
    apps
  end

  defp do_fetch(dep, { acc, lock }) do
    Mix.Dep[app: app, scm: scm, opts: opts] = dep = check_lock(dep, lock)

    cond do
      # Dependencies that cannot be fetched are always compiled afterwards
      not scm.fetchable? ->
        { dep, { [app|acc], lock } }

      # If the dependency is not available or we have a lock mismatch
      out_of_date?(dep) ->
        new =
          if scm.checked_out?(opts) do
            Mix.shell.info "* Updating #{format_dep(dep)}"
            scm.update(opts)
          else
            Mix.shell.info "* Getting #{format_dep(dep)}"
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

  defp do_finalize({ all_deps, { apps, new_lock } }, old_lock, opts) do
    # Let's get the loaded versions of deps
    deps = Mix.Deps.loaded_by_name(apps, all_deps)

    # Do not attempt to compile dependencies that are not available.
    # mix deps.check at the end will emit proper status in case they failed.
    deps = Enum.filter(deps, &available?/1)

    # Note we only retrieve the parent dependencies of the updated
    # deps if all dependencies are available. This is because if a
    # dependency is missing, it could be a children of the parent
    # (aka a sibling) which would make parent compilation fail.
    #
    # If there is any other dependency that is not ok, we include
    # it for compilation too, this is our best to try to solve the
    # maximum we can at each deps.get and deps.update.
    if Enum.all?(all_deps, &available?/1) do
      deps = with_depending(deps, all_deps) ++
             Enum.filter(all_deps, fn dep -> not ok?(dep) end)
    end

    # Merge the new lock on top of the old to guarantee we don't
    # leave out things that could not be fetched and save it.
    lock = Dict.merge(old_lock, new_lock)
    Mix.Deps.Lock.write(lock)

    do_compile(deps, opts)
    { apps, all_deps }
  end

  defp do_compile(deps, opts) do
    apps = Enum.map(deps, &(&1.app)) |> Enum.uniq

    unless opts[:no_compile] do
      if apps != [] do
        args = if opts[:quiet], do: ["--quiet"|apps], else: apps
        Mix.Task.run "deps.compile", args
      end

      unless opts[:no_deps_check] do
        Mix.Task.run "deps.check", ["--no-compile"]
      end
    end
  end

  defp ok?(Mix.Dep[status: { :ok, _ }]), do: true
  defp ok?(Mix.Dep[]), do: false

  defp with_depending(deps, all_deps) do
    (deps ++ do_with_depending(deps, all_deps)) |> Enum.uniq(&(&1.app))
  end

  defp do_with_depending([], _all_deps) do
    []
  end

  defp do_with_depending(deps, all_deps) do
    dep_names = Enum.map(deps, fn dep -> dep.app end)

    parents = Enum.filter all_deps, fn dep ->
      Enum.any?(dep.deps, &(&1 in dep_names))
    end

    do_with_depending(parents, all_deps) ++ parents
  end
end
