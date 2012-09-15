# This module is the one responsible for converging
# dependencies in a recursive fashion. This
# module and its functions are private to Mix.
defmodule Mix.Deps.Converger do
  @moduledoc false

  import Mix.Deps, only: [deps_path: 1, available?: 1]

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
    main   = Mix.Deps.Project.all
    config = [ deps_path: File.expand_path(Mix.project[:deps_path]),
               lockfile: File.expand_path(Mix.project[:lockfile]) ]
    all(main, [], [], main, [], config, callback, rest)
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
  defp all([dep|t], acc, upper_breadths, current_breadths, diverged, config, callback, rest) do
    upper = contains_dep?(upper_breadths, dep)
    prev  = contains_dep?(acc, dep)

    cond do
      upper in [:converge, :diverge] or prev in [:converge] ->
        all(t, acc, upper_breadths, current_breadths, diverged, config, callback, rest)
      prev in [:diverge] ->
        diverged = [{ find_dep(acc, dep), dep } | diverged]
        all(t, acc, upper_breadths, current_breadths, diverged, config, callback, rest)
      true ->
        { dep, rest } = callback.(dep, rest)

        if available?(dep) and mixfile?(dep) do
          { project, deps } = nested_deps(dep, config)
          { acc, diverged, rest } = all(t, [dep.project(project)|acc], upper_breadths,
                                        current_breadths, diverged, config, callback, rest)
          all(deps, acc, current_breadths, deps ++ current_breadths, diverged, config, callback, rest)
        else
          all(t, [dep|acc], upper_breadths, current_breadths, diverged, config, callback, rest)
        end
    end
  end

  defp all([], acc, _upper, _current, diverged, _config, _callback, rest) do
    { acc, diverged, rest }
  end

  defp find_dep(list, Mix.Dep[app: app]) do
    Enum.find(list, match?(Mix.Dep[app: ^app], &1))
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

  defp mixfile?(dep) do
    File.regular?(File.join(deps_path(dep), "mix.exs"))
  end

  defp nested_deps(Mix.Dep[app: app, opts: opts] = dep, config) do
    File.cd! deps_path(dep), fn ->
      env      = opts[:env] || :prod
      old_env  = Mix.env

      try do
        Mix.env(env)
        project = load_project(app, config)

        try do
          { project, Mix.Deps.Project.all }
        after
          Mix.Project.pop
        end
      after
        Mix.env(old_env)
      end
    end
  end

  defp load_project(app, config) do
    if cached = Mix.Server.call({ :mixfile_cache, app }) do
      Mix.Project.post_config(config)
      Mix.Project.push(cached)
      cached
    else
      old_proj = Mix.Project.get

      if File.regular?("mix.exs") do
        Mix.Project.post_config(config)
        Code.load_file "mix.exs"
      end

      new_proj = Mix.Project.get

      if old_proj == new_proj do
        new_proj = nil
        Mix.Project.push new_proj
      end

      Mix.Server.cast({ :mixfile_cache, app, new_proj })
      new_proj
    end
  end
end