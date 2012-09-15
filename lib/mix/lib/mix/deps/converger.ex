# This module is the one responsible for converging
# dependencies in a recursive fashion.
defmodule Mix.Deps.Converger do
  @moduledoc false

  import Mix.Deps, only: [deps_path: 1]

  @doc """
  Returns all dependencies from the current project,
  including nested dependencies. There is a callback
  that is invoked for each dependency and must return
  an updated depedency in case some processing is done.
  """
  def all(callback // fn(dep) -> dep end) do
    main   = Mix.Deps.all
    config = [ deps_path: File.expand_path(Mix.project[:deps_path]),
               lockfile: File.expand_path(Mix.project[:lockfile]) ]
    all(main, [], [], main, [], config, callback)
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
  defp all([dep|t], acc, upper_breadths, current_breadths, diverged, config, callback) do
    upper = contains_dep?(upper_breadths, dep)
    prev  = contains_dep?(acc, dep)

    cond do
      upper in [:converge, :diverge] or prev in [:converge] ->
        all(t, acc, upper_breadths, current_breadths, diverged, config, callback)
      prev in [:diverge] ->
        diverged = [{ find_dep(acc, dep), dep } | diverged]
        all(t, acc, upper_breadths, current_breadths, diverged, config, callback)
      true ->
        if unavailable?(dep) or not mixfile?(dep) do
          all(t, [dep|acc], upper_breadths, current_breadths, diverged, config, callback)
        else
          { project, deps } = nested_deps(dep, config)
          { acc, diverged } = all(t, [dep.project(project)|acc], upper_breadths, current_breadths, diverged, config, callback)
          all(deps, acc, current_breadths, deps ++ current_breadths, diverged, config, callback)
        end
    end
  end

  defp all([], acc, _upper, _current, diverged, _config, _callback) do
    { acc, diverged }
  end

  defp find_dep(list, Mix.Dep[app: app]) do
    Enum.find_value(list, match?(Mix.Dep[app: ^app], &1))
  end

  defp contains_dep?(list, dep) do
    if match = find_dep(list, dep) do
      Mix.Dep[scm: scm1, opts: opts1] = dep
      Mix.Dep[scm: scm2, opts: opts2] = match

      if scm1 == scm2 && scm1.match?(opts1, opts2) do
        :converge
      else
        :diverge
      end
    end
  end

  defp unavailable?(Mix.Dep[status: { :unavailable, _ }]), do: true
  defp unavailable?(_), do: false

  defp mixfile?(dep) do
    File.regular?(File.join(deps_path(dep), "mix.exs"))
  end

  defp nested_deps(Mix.Dep[opts: opts] = dep, config) do
    File.cd! deps_path(dep), fn ->
      env      = opts[:env] || :prod
      old_env  = Mix.env
      old_proj = Mix.Project.current

      try do
        Mix.env(env)

        if File.regular?("mix.exs") do
          Mix.Server.cast({ :post_config, config })
          Code.load_file "mix.exs"
        end

        if old_proj == Mix.Project.current do
          Mix.Project.push nil
        else
          project = Mix.Project.current
        end

        try do
          { project, Mix.Deps.all }
        after
          Mix.Project.pop
        end
      after
        Mix.env(old_env)
      end
    end
  end
end