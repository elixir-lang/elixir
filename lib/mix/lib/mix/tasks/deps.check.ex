defmodule Mix.Tasks.Deps.Check do
  use Mix.Task

  import Mix.Deps, only: [loaded: 0, format_dep: 1, format_status: 1, check_lock: 2, ok?: 1]

  @hidden true
  @shortdoc "Check if all dependencies are valid"

  @moduledoc """
  Checks if all dependencies are valid and if not, abort.
  Prints the invalid dependencies' status before aborting.

  This task is not shown in `mix help` but it is part
  of the `mix` public API and can be depended on.
  """
  def run(_) do
    lock = Mix.Deps.Lock.read
    all  = Enum.map loaded, &check_lock(&1, lock)

    prune_deps(all)

    case Enum.partition all, &ok?/1 do
      { _, [] }     -> :ok
      { _, not_ok } ->
        shell = Mix.shell
        shell.error "Unchecked dependencies for environment #{Mix.env}:"

        Enum.each not_ok, fn(dep) ->
          shell.error "* #{format_dep(dep)}"
          shell.error "  #{format_status dep}"
        end

        raise Mix.Error, message: "Can't continue due to errors on dependencies"
    end
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
