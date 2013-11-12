# This module is the one responsible for converging
# dependencies in a recursive fashion. This
# module and its functions are private to Mix.
defmodule Mix.Deps.Converger do
  @moduledoc false

  @doc """
  Topsorts the given dependencies.
  """
  def topsort(deps) do
    graph = :digraph.new

    try do
      Enum.each deps, fn Mix.Dep[app: app] ->
        :digraph.add_vertex(graph, app)
      end

      Enum.each deps, fn Mix.Dep[app: app, deps: other_apps] ->
        Enum.each other_apps, fn other_app ->
          :digraph.add_edge(graph, other_app, app)
        end
      end

      if apps = :digraph_utils.topsort(graph) do
        Enum.map apps, fn(app) ->
          Enum.find(deps, fn(Mix.Dep[app: other_app]) -> app == other_app end)
        end
      else
        raise Mix.Error, message: "Could not sort dependencies. There are cycles in the dependency graph."
      end
    after
      :digraph.delete(graph)
    end
  end

  @doc """
  Returns all dependencies from the current project,
  including nested dependencies. There is a callback
  that is invoked for each dependency and must return
  an updated depedency in case some processing is done.
  """
  def all(rest, callback) do
    conf = Mix.Project.deps_config
    main = Mix.Deps.Retriever.children
    apps = Enum.map(main, &(&1.app))
    all(main, [], [], apps, conf, callback, rest)
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

        # After we invoke the callback (which may actually check out the
        # dependency), we load the dependency including its latest info
        # and children information.
        { dep, children } = Mix.Deps.Retriever.load(dep, config)
        children = reject_non_fullfilled_optional(children, current_breadths)
        dep      = dep.deps(Enum.map(children, &(&1.app)))

        { acc, rest } = all(t, [dep|acc], upper_breadths, current_breadths, config, callback, rest)
        all(children, acc, current_breadths, dep.deps ++ current_breadths, config, callback, rest)
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
    if dep.app in upper_breadths do
      Mix.Dep[app: app] = dep

      { overrider, acc } =
        Enum.reduce(acc, { nil , [] }, fn other, { overrider, acc } ->
          Mix.Dep[app: other_app, opts: other_opts] = other

          cond do
            app == other_app && (other_opts[:override] || converge?(other, dep)) ->
              { with_matching_req(other, dep), acc }
            app == other_app ->
              { other.status({ :overriden, dep }), acc }
            true ->
              { overrider, [other|acc] }
          end
        end)

      [overrider | Enum.reverse(acc)]
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
          converge?(other, dep) ->
            { with_matching_req(other, dep), true }
          true ->
            { other.status({ :diverged, dep }), true }
        end
      end

    if match, do: acc
  end

  defp converge?(_, Mix.Dep[scm: Mix.SCM.Optional]) do
    true
  end

  defp converge?(Mix.Dep[scm: scm, opts: opts1], Mix.Dep[scm: scm, opts: opts2]) do
    scm.equal?(opts1, opts2)
  end

  defp converge?(_, _), do: false

  defp reject_non_fullfilled_optional(children, upper_breadths) do
    Enum.reject children, fn Mix.Dep[app: app, opts: opts] ->
      opts[:optional] && not(app in upper_breadths)
    end
  end

  defp with_matching_req(Mix.Dep[] = other, Mix.Dep[] = dep) do
    case other.status do
      { :ok, vsn } when not nil?(vsn) ->
        if Mix.Deps.Retriever.vsn_match?(dep.requirement, vsn) do
          other
        else
          other.status({ :divergedreq, dep })
        end
      _ ->
        other
    end
  end
end
