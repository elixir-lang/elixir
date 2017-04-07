defmodule Mix.Tasks.Deps.Loadpaths do
  use Mix.Task

  import Mix.Dep, only: [loaded_by_name: 2, format_dep: 1, ok?: 1,
                         format_status: 1, check_lock: 1]

  @moduledoc """
  Checks and loads all dependencies along the way.

  If there is an invalid dependency, its status is printed
  before aborting.

  Although this task does not show up in `mix help`, it is
  part of Mix public API and can be depended on.

  ## Command line options

    * `--no-deps-check` - does not check or compile deps, only load available ones
    * `--no-compile` - does not compile dependencies

  """
  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    all = Enum.map(Mix.Dep.cached(), &check_lock/1)

    unless "--no-deps-check" in args do
      deps_check(all, "--no-compile" in args)
    end

    load_paths =
      for dep <- all, path <- Mix.Dep.load_paths(dep) do
        _ = Code.prepend_path(path)
        path
      end

    unless System.get_env("MIX_NO_DEPS") in ~w(1 true) do
      prune_deps(load_paths, "--no-deps-check" in args)
    end
  end

  # If the build is per environment, we should be able to look
  # at all dependencies and remove the builds that no longer
  # have a dependency defined for them.
  #
  # Notice we require the build_path to be nil. If it is not nil,
  # it means the build_path is shared so we don't delete entries.
  #
  # We also expect env_path to be nil. If it is not nil, it means
  # it was set by a parent application and the parent application
  # should be the one doing the pruning.
  defp prune_deps(load_paths, no_check?) do
    config = Mix.Project.config

    shared_build? =
      no_check? or config[:build_path] != nil or config[:build_per_environment] == false

    config
    |> Mix.Project.build_path
    |> Path.join("lib/*/ebin")
    |> Path.wildcard
    |> List.delete(config[:app] && Mix.Project.compile_path(config))
    |> Kernel.--(load_paths)
    |> Enum.each(&prune_path(&1, shared_build?))
  end

  defp prune_path(path, shared_build?) do
    _ = Code.delete_path(path)

    unless shared_build? do
      path |> Path.dirname |> File.rm_rf!
    end
  end

  defp deps_check(all, no_compile?) do
    {not_ok, compile} = partition(all, [], [])

    cond do
      not_ok != [] ->
        show_not_ok!(not_ok)
      compile == [] or no_compile? ->
        :ok
      true ->
        Mix.Tasks.Deps.Compile.compile(compile)
        compile
        |> Enum.map(& &1.app)
        |> loaded_by_name(env: Mix.env)
        |> Enum.filter(&(not ok?(&1)))
        |> show_not_ok!
    end
  end

  defp partition([dep | deps], not_ok, compile) do
    cond do
      compilable?(dep) ->
        if from_umbrella?(dep) do
          partition(deps, not_ok, compile)
        else
          partition(deps, not_ok, [dep | compile])
        end
      ok?(dep) ->
        partition(deps, not_ok, compile)
      true ->
        partition(deps, [dep | not_ok], compile)
    end
  end

  defp partition([], not_ok, compile) do
    {Enum.reverse(not_ok), Enum.reverse(compile)}
  end

  # Those are compiled by umbrella.
  defp from_umbrella?(dep) do
    dep.opts[:from_umbrella]
  end

  # Every local dependency (i.e. that are not fetchable)
  # are automatically recompiled if they are ok.
  defp local?(dep) do
    not dep.scm.fetchable?
  end

  # Can the dependency be compiled automatically without user intervention?
  defp compilable?(%Mix.Dep{status: {:elixirlock, _}}), do: true
  defp compilable?(%Mix.Dep{status: {:noappfile, _}}), do: true
  defp compilable?(%Mix.Dep{status: {:scmlock, _}}), do: true
  defp compilable?(%Mix.Dep{status: :compile}), do: true
  defp compilable?(%Mix.Dep{} = dep), do: ok?(dep) and local?(dep)

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
