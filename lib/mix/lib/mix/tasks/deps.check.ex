defmodule Mix.Tasks.Deps.Check do
  use Mix.Task

  import Mix.Dep, only: [loaded: 1, loaded_by_name: 2, format_dep: 1, ok?: 1,
                         format_status: 1, check_lock: 2, partition: 1]

  @moduledoc """
  Checks if all dependencies are valid and if not, abort.
  Prints the invalid dependencies' status before aborting.

  This task is not shown in `mix help` but it is part
  of the `mix` public API and can be depended on.

  ## Command line options

    * `--no-compile` - do not compile dependencies

  """
  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    lock = Mix.Dep.Lock.read
    all  = Enum.map(loaded(env: Mix.env), &check_lock(&1, lock))

    _ = prune_deps(all)
    {not_ok, compile} = partition(all)

    cond do
      not_ok != [] ->
        show_not_ok!(not_ok)
      compile == [] or "--no-compile" in args ->
        :ok
      true ->
        Mix.Tasks.Deps.Compile.compile(compile)
        show_not_ok! compile
                     |> Enum.map(& &1.app)
                     |> loaded_by_name(env: Mix.env)
                     |> Enum.filter(&(not ok?(&1)))
    end
  end

  # If the build is per environment, we should be able to look
  # at all dependencies and remove the builds that no longer
  # have a dependency defined for them.
  #
  # Notice we require the build_path to be nil. If it is not nil,
  # it means the build_path is shared, so we leave it up to the owner.
  #
  # We also expect env_path to be nil. If it is not nil, it means
  # it was set by a parent application and the parent application
  # should be the one doing the pruning.
  defp prune_deps(all) do
    config = Mix.Project.config

    if is_nil(config[:env_path]) and
       is_nil(config[:build_path]) and
       config[:build_per_environment] do
      paths = Mix.Project.build_path(config)
              |> Path.join("lib/*/ebin")
              |> Path.wildcard
              |> List.delete(config[:app] && Mix.Project.compile_path(config))

      to_prune = Enum.reduce(all, paths, &(&2 -- Mix.Dep.load_paths(&1)))

      Enum.map(to_prune, fn path ->
        # Path cannot be in code path when deleting
        _ = Code.delete_path(path)
        File.rm_rf!(path |> Path.dirname)
      end)
    else
      []
    end
  end

  defp show_not_ok!([]) do
    :ok
  end

  defp show_not_ok!(deps) do
    shell = Mix.shell
    shell.error "Unchecked dependencies for environment #{Mix.env}:"

    Enum.each deps, fn(dep) ->
      shell.error "* #{format_dep dep}"
      shell.error "  #{format_status dep}"
    end

    Mix.raise "Can't continue due to errors on dependencies"
  end
end
