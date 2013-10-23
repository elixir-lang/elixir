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
  # Notice that the `d` dependency exists as a child of `b`
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
      new_acc = diverged_deps(acc, dep) ->
        all(t, new_acc, upper_breadths, current_breadths, config, callback, rest)
      true ->
        { dep, rest } = callback.(dep, rest)

        # After we invoke the callback (which may actually fetch the
        # dependency) we get all direct childrens for the next one.
        # Note that we need to reverse the deps, so the final order
        # is as expected.
        deps = Mix.Deps.Retriever.children(dep, config)
        { acc, rest } = all(t, [dep.deps(deps)|acc], upper_breadths, current_breadths, config, callback, rest)
        all(Enum.reverse(deps), acc, current_breadths, deps ++ current_breadths, config, callback, rest)
    end
  end

  defp all([], acc, _upper, _current, _config, _callback, rest) do
    { acc, rest }
  end

  # Look for an overriding dep in the upper breadths, if
  # found return a new acc without the overriden dep and
  # with the proper status set on the overrider. The
  # overrider is moved to the front of the accumulator to
  # preserve the position of the removed dep.
  defp overriden_deps(acc, upper_breadths, dep) do
    overriden = Enum.any?(upper_breadths, fn(other) ->
      other.app == dep.app
    end)

    if overriden do
      Mix.Dep[app: app] = dep

      { overrider, acc } =
        Enum.reduce(acc, { nil , [] }, fn other, { overrider, acc } ->
          Mix.Dep[app: other_app, opts: other_opts] = other

          cond do
            app == other_app && (other_opts[:override] || converge?(dep, other)) ->
              { other, acc }
            app == other_app ->
              { other.status({ :overriden, dep }), acc }
            true ->
              { overrider, [other|acc] }
          end
        end)

      if overrider.deps == [] do
        [overrider | Enum.reverse(acc)]
      else
        dep = hd(overrider.deps).app
        { before_dep, after_dep } = Enum.split_while(acc, &(&1.app != dep))
        Enum.reverse(before_dep ++ [overrider] ++ after_dep)
      end
    end
  end

  # Check the list for matching dependencies.
  # In case dependencies are found, check if their
  # scm info match. If not, mark the dependencies
  # as diverged.
  defp diverged_deps(list, dep) do
    Mix.Dep[app: app] = dep

    { acc, match } =
      Enum.map_reduce list, false, fn(other, match) ->
        Mix.Dep[app: other_app] = other

        cond do
          app != other_app ->
            { other, match }
          converge?(dep, other) ->
            { other, true }
          true ->
            { other.status({ :diverged, dep }), true }
        end
      end

    if match, do: acc
  end

  defp converge?(Mix.Dep[scm: scm, requirement: req, opts: opts1],
                 Mix.Dep[scm: scm, requirement: req, opts: opts2]) do
    scm.equal?(opts1, opts2)
  end

  defp converge?(_, _), do: false
end
