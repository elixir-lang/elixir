# Module responsible for fetching (getting/updating)
# dependencies from their sources.
#
# The new_lock and old_lock mechanism exists to signal
# externally which dependencies need to be updated and
# which ones do not.
defmodule Mix.Dep.Fetcher do
  @moduledoc false

  import Mix.Dep, only: [format_dep: 1, check_lock: 1, available?: 1]

  @doc """
  Fetches all dependencies.
  """
  def all(old_lock, new_lock, opts) do
    result = Mix.Dep.Converger.converge([], new_lock, opts, &do_fetch/3)
    {apps, _deps} = do_finalize(result, old_lock, opts)
    apps
  end

  @doc """
  Fetches the dependencies with the given names and their children recursively.
  """
  def by_name(names, old_lock, new_lock, opts) do
    fetcher = fetch_by_name(names, new_lock)
    result = Mix.Dep.Converger.converge([], new_lock, opts, fetcher)
    {apps, deps} = do_finalize(result, old_lock, opts)

    # Check if all given dependencies are loaded or fail
    _ = Mix.Dep.filter_by_name(names, deps, opts)
    apps
  end

  defp fetch_by_name(given, lock) do
    names = to_app_names(given)

    fn %Mix.Dep{app: app} = dep, acc, new_lock ->
      # Only fetch if dependency is in given names or if lock has
      # been changed for dependency by remote converger or it is new
      if app in names or lock[app] != new_lock[app] or is_nil(lock[app]) do
        do_fetch(dep, acc, new_lock)
      else
        {dep, acc, new_lock}
      end
    end
  end

  defp do_fetch(dep, acc, lock) do
    %Mix.Dep{app: app, scm: scm, opts: opts} = dep = check_lock(dep)

    cond do
      # Dependencies that cannot be fetched are always compiled afterwards
      not scm.fetchable? ->
        {dep, [app | acc], lock}

      # If the dependency is not available or we have a lock mismatch
      out_of_date?(dep) ->
        new =
          if scm.checked_out?(opts) do
            Mix.shell().info("* Updating #{format_dep(dep)}")
            scm.update(opts)
          else
            Mix.shell().info("* Getting #{format_dep(dep)}")
            scm.checkout(opts)
          end

        if new do
          File.touch!(Path.join(opts[:dest], ".fetch"))
          dep = put_in(dep.opts[:lock], new)
          {dep, [app | acc], Map.put(lock, app, new)}
        else
          {dep, acc, lock}
        end

      # The dependency is ok or has some other error
      true ->
        {dep, acc, lock}
    end
  end

  defp out_of_date?(%Mix.Dep{status: {:lockmismatch, _}}), do: true
  defp out_of_date?(%Mix.Dep{status: :lockoutdated}), do: true
  defp out_of_date?(%Mix.Dep{status: :nolock}), do: true
  defp out_of_date?(%Mix.Dep{status: {:unavailable, _}}), do: true
  defp out_of_date?(%Mix.Dep{}), do: false

  defp do_finalize({all_deps, apps, new_lock}, old_lock, opts) do
    # Let's get the loaded versions of deps
    deps = Mix.Dep.filter_by_name(apps, all_deps, opts)

    # Note we only retrieve the parent dependencies of the updated
    # deps if all dependencies are available. This is because if a
    # dependency is missing, it could directly affect one of the
    # dependencies we are trying to compile, causing the whole thing
    # to fail.
    parent_deps =
      if Enum.all?(all_deps, &available?/1) do
        Enum.uniq_by(with_depending(deps, all_deps), & &1.app)
      else
        []
      end

    # Merge the new lock on top of the old to guarantee we don't
    # leave out things that could not be fetched and save it.
    lock = Map.merge(old_lock, new_lock)
    Mix.Dep.Lock.write(lock)
    mark_as_fetched(parent_deps)

    # See if any of the deps diverged and abort.
    show_diverged!(Enum.filter(all_deps, &Mix.Dep.diverged?/1))

    {apps, all_deps}
  end

  defp mark_as_fetched(deps) do
    # If the dependency is fetchable, we are going to write a .fetch
    # file to it. Each build, regardless of the environment and location,
    # will compared against this .fetch file to know if the dependency
    # needs recompiling.
    _ =
      for %Mix.Dep{scm: scm, opts: opts} <- deps, scm.fetchable?() do
        File.touch!(Path.join(opts[:dest], ".fetch"))
      end

    :ok
  end

  defp with_depending(deps, all_deps) do
    deps ++ do_with_depending(deps, all_deps)
  end

  defp do_with_depending([], _all_deps) do
    []
  end

  defp do_with_depending(deps, all_deps) do
    dep_names = Enum.map(deps, fn dep -> dep.app end)

    parents =
      Enum.filter(all_deps, fn dep ->
        Enum.any?(dep.deps, &(&1.app in dep_names))
      end)

    do_with_depending(parents, all_deps) ++ parents
  end

  defp to_app_names(given) do
    Enum.map(given, fn app ->
      if is_binary(app), do: String.to_atom(app), else: app
    end)
  end

  defp show_diverged!([]), do: :ok

  defp show_diverged!(deps) do
    shell = Mix.shell()
    shell.error("Dependencies have diverged:")

    Enum.each(deps, fn dep ->
      shell.error("* #{Mix.Dep.format_dep(dep)}")
      shell.error("  #{Mix.Dep.format_status(dep)}")
    end)

    Mix.raise("Can't continue due to errors on dependencies")
  end
end
