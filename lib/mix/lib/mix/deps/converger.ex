# This module is the one responsible for converging
# dependencies in a recursive fashion. This
# module and its functions are private to Mix.
defmodule Mix.Deps.Converger do
  @moduledoc false

  @doc """
  Clear up the mixfile cache.
  """
  def clear_cache do
    Mix.Server.cast(:clear_mixfile_cache)
  end

  @doc """
  Returns all dependencies from the current project,
  including nested dependencies. There is a callback
  that is invoked for each dependency and must return
  an updated depedency in case some processing is done.
  """
  def all(rest, callback) do
    config = [ deps_path: Path.expand(Mix.project[:deps_path]),
               root_lockfile: Path.expand(Mix.project[:lockfile]) ]
    main = Mix.Deps.Retriever.children |> Enum.reverse
    all(main, [], [], main, config, callback, rest)
  end

  # We traverse the tree of dependencies in a breadth-
  # first fashion. The reason for this is that we converge
  # dependencies, but allow the parent to override any
  # dependency in the child. Consider this tree with
  # dependencies `a`, `b`, etc and the order they are
  # converged:
  #
  #     * project
  #       1) a
  #       2) b
  #         4) d
  #       3) c
  #         5) e
  #         6) f
  #           7) d
  #
  # Notice that the `d` dependency exists as a child of `g`
  # and child of `f`. In case the dependency is the same,
  # we proceed. However, if there is a conflict, for instance
  # different git repositories is used as source in each, we
  # raise an exception.
  #
  # In order to solve such dependencies, we allow the project
  # to specify the same dependency, but it will be considered
  # to have higher priority:
  #
  #     * project
  #       1) a
  #       2) b
  #         5) d
  #       3) c
  #         6) e
  #         7) f
  #           8) d
  #       4) d
  #
  # Now, since `d` was specified in a parent project, no
  # exception is going to be raised since d is considered
  # to be the authorative source.
  defp all([dep|t], acc, upper_breadths, current_breadths, config, callback, rest) do
    cond do
      new_acc = overriden_deps(acc, upper_breadths, dep) ->
        all(t, new_acc, upper_breadths, current_breadths, config, callback, rest)
      ({ diverged_acc, diverged } = diverged_deps(acc, dep)) && diverged ->
        all(t, diverged_acc, upper_breadths, current_breadths, config, callback, rest)
      true ->
        { dep, rest } = callback.(dep, rest)
        deps = Mix.Deps.Retriever.children(dep, config)
        { acc, rest } = all(t, [dep.deps(deps)|acc], upper_breadths, current_breadths, config, callback, rest)
        all(deps, acc, current_breadths, deps ++ current_breadths, config, callback, rest)
    end
  end

  defp all([], acc, _upper, _current, _config, _callback, rest) do
    { acc, rest }
  end

  # Look for an overriding dep in the upper breadths, if
  # found return a new acc without the overriden dep and
  # with the proper status set on the overrider
  defp overriden_deps(acc, upper_breadths, dep) do
    overriden = Enum.any?(upper_breadths, fn(other) ->
      other.app == dep.app
    end)

    if overriden do
      Mix.Dep[app: app] = dep

      Enum.map(acc, fn other ->
        Mix.Dep[app: other_app, opts: other_opts] = other

        cond do
          app == other_app && (other_opts[:override] || converge?(dep, other)) ->
            other
          app == other_app ->
            other.status({ :overriden, dep })
          true ->
            other
        end
      end)
    end
  end

  # Check the list for matching dependencies.
  # In case dependencies are found, check if their
  # scm info match. If not, mark the dependencies
  # as diverged.
  defp diverged_deps(list, dep) do
    Mix.Dep[app: app] = dep

    Enum.map_reduce list, false, fn(other, diverged) ->
      Mix.Dep[app: other_app] = other

      if app != other_app || converge?(dep, other) do
        { other, diverged }
      else
        { other.status({ :diverged, dep }), true }
      end
    end
  end

  defp converge?(Mix.Dep[scm: scm, requirement: req, opts: opts1],
                 Mix.Dep[scm: scm, requirement: req, opts: opts2]) do
    scm.equal?(opts1, opts2)
  end

  defp converge?(_, _), do: false
end
