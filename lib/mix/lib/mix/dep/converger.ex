# This module is the one responsible for converging
# dependencies in a recursive fashion. This
# module and its functions are private to Mix.
defmodule Mix.Dep.Converger do
  @moduledoc false

  @doc """
  Topsorts the given dependencies.
  """
  def topsort(deps) do
    graph = :digraph.new

    try do
      Enum.each(deps, fn %Mix.Dep{app: app} ->
        :digraph.add_vertex(graph, app)
      end)

      Enum.each(deps, fn %Mix.Dep{app: app, deps: other_deps} ->
        Enum.each(other_deps, fn %Mix.Dep{app: other_app} ->
          :digraph.add_edge(graph, other_app, app)
        end)
      end)

      if apps = :digraph_utils.topsort(graph) do
        Enum.map(apps, fn(app) ->
          Enum.find(deps, fn(%Mix.Dep{app: other_app}) -> app == other_app end)
        end)
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
  an updated dependency in case some processing is done.

  See `Mix.Dep.Loader.children/1` for options.
  """
  def all(acc, lock, opts, callback) do
    main      = Mix.Dep.Loader.children(opts)
    apps      = Enum.map(main, &(&1.app))
    converger = Mix.RemoteConverger.get

    # Run converger for all dependencies not handled by remote
    # converger. If `rest` is not nil we know dependencies are
    # being updated or fetched for the first time (only then do
    # we want the remote converger to run)
    result =
      all(main, [], [], apps, callback, acc, lock, fn dep ->
        if not nil?(lock) && converger && converger.remote?(dep) do
          { :loaded, dep }
        else
          { :unloaded, dep }
        end
      end)

    # Run remote converger if one is available and rerun mix's
    # converger with the new information
    case converger && result do
      { deps, rest, lock } when not nil?(lock) ->
        new_lock = converger.converge(main, lock)

        deps = deps
               |> Enum.reject(&converger.remote?(&1))
               |> Enum.into(HashDict.new, &{ &1.app, &1 })

        all(main, [], [], apps, callback, rest, new_lock, fn dep ->
          if cached = deps[dep.app] do
            { :loaded, cached }
          else
            { :unloaded, dep }
          end
        end)
      _ ->
        result
    end
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
  defp all([dep|t], acc, upper_breadths, current_breadths, callback, rest, lock, cache) do
    cond do
      new_acc = diverged_deps(acc, upper_breadths, dep) ->
        all(t, new_acc, upper_breadths, current_breadths, callback, rest, lock, cache)
      true ->
        dep =
          case cache.(dep) do
            { :loaded, cached_dep } ->
              cached_dep
            { :unloaded, dep } ->
              { dep, rest, lock } = callback.(dep, rest, lock)

              # After we invoke the callback (which may actually check out the
              # dependency), we load the dependency including its latest info
              # and children information.
              Mix.Dep.Loader.load(dep)
          end

        dep = %{dep | deps: reject_non_fullfilled_optional(dep.deps, current_breadths)}
        { acc, rest, lock } = all(t, [dep|acc], upper_breadths, current_breadths, callback, rest, lock, cache)
        all(dep.deps, acc, current_breadths, Enum.map(dep.deps, &(&1.app)) ++ current_breadths, callback, rest, lock, cache)
    end
  end

  defp all([], acc, _upper, _current, _callback, rest, lock, _cache) do
    { acc, rest, lock }
  end

  # Look for divergence in dependencies.
  #
  # If the same dependency is specified more than once,
  # we need to guarantee they converge. If they don't
  # converge, we mark them as diverged.
  #
  # The only exception is when the dependency that
  # diverges is in the upper breadth, in those cases we
  # also check for the override option and mark the dependency
  # as overridden instead of diverged.
  defp diverged_deps(list, upper_breadths, dep) do
    %Mix.Dep{app: app} = dep
    in_upper? = app in upper_breadths

    { acc, match } =
      Enum.map_reduce list, false, fn(other, match) ->
        %Mix.Dep{app: other_app, opts: other_opts} = other

        cond do
          app != other_app ->
            { other, match }
          in_upper? && other_opts[:override] ->
            { other, true }
          converge?(other, dep) ->
            { with_matching_req(other, dep), true }
          true ->
            tag = if in_upper?, do: :overridden, else: :diverged
            { %{other | status: { tag, dep }}, true }
        end
      end

    if match, do: acc
  end

  defp converge?(%Mix.Dep{scm: scm1, opts: opts1}, %Mix.Dep{scm: scm2, opts: opts2}) do
    scm1 == scm2 and scm1.equal?(opts1, opts2)
  end

  defp reject_non_fullfilled_optional(children, upper_breadths) do
    Enum.reject children, fn %Mix.Dep{app: app, opts: opts} ->
      opts[:optional] && not(app in upper_breadths)
    end
  end

  defp with_matching_req(%Mix.Dep{} = other, %Mix.Dep{} = dep) do
    case other.status do
      { :ok, vsn } when not nil?(vsn) ->
        if Mix.Dep.Loader.vsn_match?(dep.requirement, vsn, dep.app) do
          other
        else
          %{other | status: { :divergedreq, dep }}
        end
      _ ->
        other
    end
  end
end
