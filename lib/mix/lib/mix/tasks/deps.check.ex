defmodule Mix.Tasks.Deps.Check do
  use Mix.Task

  import Mix.Deps, only: [loaded: 0, format_dep: 1, format_status: 1, check_lock: 2]

  @moduledoc """
  Checks if all dependencies are valid and if not, abort.
  Prints the invalid dependencies' status before aborting.

  This task is not shown in `mix help` but it is part
  of the `mix` public API and can be depended on.

  ## Command line options

  * `--no-compile` - do not compile dependencies

  """
  def run(args) do
    lock = Mix.Deps.Lock.read
    all  = Enum.map loaded, &check_lock(&1, lock)

    prune_deps(all)
    { not_ok, recompile } = partition_deps(all, [], [])

    cond do
      not_ok != [] ->
        shell = Mix.shell
        shell.error "Unchecked dependencies for environment #{Mix.env}:"

        Enum.each not_ok, fn(dep) ->
          shell.error "* #{format_dep(dep)}"
          shell.error "  #{format_status dep}"
        end

        raise Mix.Error, message: "Can't continue due to errors on dependencies"
      recompile == [] or "--no-compile" in args ->
        :ok
      true ->
        Mix.Task.run "deps.compile", Enum.map(recompile, & &1.app)
    end
  end

  defp partition_deps([Mix.Dep[status: { :ok, _ }]|deps], not_ok, recompile) do
    partition_deps(deps, not_ok, recompile)
  end

  defp partition_deps([Mix.Dep[status: { :noappfile, _ }] = dep|deps], not_ok, recompile) do
    partition_deps(deps, not_ok, [dep|recompile])
  end

  defp partition_deps([dep|deps], not_ok, recompile) do
    partition_deps(deps, [dep|not_ok], recompile)
  end

  defp partition_deps([], not_ok, recompile) do
    { not_ok, recompile }
  end

  # If the build is per environment, we should be able to look
  # at all dependencies and remove the builds that no longer
  # has a dependnecy defined for them.
  #
  # Notice we require the build_path to be nil. If the build_path
  # is not nil, it means it was set by a parent application and
  # the parent application should be the one to do the pruning.
  defp prune_deps(all) do
    config = Mix.project

    if nil?(config[:build_path]) && config[:build_per_environment] do
      paths = Mix.Project.build_path(config)
              |> Path.join("lib/*/ebin")
              |> Path.wildcard
              |> List.delete(not Mix.Project.umbrella? && Mix.Project.compile_path(config))

      to_prune = Enum.reduce(all, paths, &(&2 -- Mix.Deps.load_paths(&1)))

      Enum.map(to_prune, fn path ->
        Code.delete_path(path)
        File.rm_rf!(path |> Path.dirname)
      end)
    end
  end
end
