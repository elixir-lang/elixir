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
    { main, rest } = Mix.Deps.Retriever.all(rest, config, callback)
    { all(Enum.reverse(main), [], [], main), rest }
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
  defp all([dep|t], acc, upper_breadths, current_breadths) do
    cond do
      contains_dep?(upper_breadths, dep) ->
        all(t, acc, upper_breadths, current_breadths)
      match?({ diverged_acc, true }, diverged_dep?(acc, dep)) ->
        all(t, diverged_acc, upper_breadths, current_breadths)
      true ->
        deps = dep.deps
        if deps != [] do
          acc = all(t, [dep|acc], upper_breadths, current_breadths)
          all(deps, acc, current_breadths, deps ++ current_breadths)
        else
          all(t, [dep|acc], upper_breadths, current_breadths)
        end
    end
  end

  defp all([], acc, _upper, _current) do
    acc
  end

  # Does the list contain the given dependency?
  defp contains_dep?(list, Mix.Dep[app: app]) do
    Enum.any?(list, match?(Mix.Dep[app: ^app], &1))
  end

  # Check the list for matching dependencies.
  # In case dependencies are found, check if their
  # scm info match. If not, mark the dependencies
  # as diverged.
  def diverged_dep?(list, dep) do
    Mix.Dep[app: app, scm: scm, opts: opts] = dep

    Enum.map_reduce list, false, fn(other, diverged) ->
      Mix.Dep[app: other_app, scm: other_scm, opts: other_opts] = other

      if app != other_app || scm == other_scm && scm.equals?(opts, other_opts) do
        { other, diverged }
      else
        { other.status({ :diverged, dep }), true }
      end
    end
  end
end
