defmodule Mix.Tasks.Deps.Clean do
  use Mix.Task

  @shortdoc "Deletes the given dependencies' files"

  @moduledoc """
  Deletes the given dependencies' files, including build artifacts and fetched
  sources.

  Since this is a destructive action, cleaning of dependencies
  only occurs when passing arguments/options:

    * `dep1 dep2` - the names of dependencies to be deleted separated by a space
    * `--unlock` - also unlocks the deleted dependencies
    * `--build` - deletes only compiled files (keeps source files)
    * `--all` - deletes all dependencies
    * `--unused` - deletes only unused dependencies
      (i.e. dependencies no longer mentioned in `mix.exs`)

  By default this task works across all environments,
  unless `--only` is given which will clean all dependencies
  leaving only the ones for chosen environment.
  """

  @switches [unlock: :boolean, all: :boolean, only: :string, unused: :boolean, build: :boolean]

  @impl true
  def run(args) do
    Mix.Project.get!()
    {opts, apps, _} = OptionParser.parse(args, switches: @switches)

    build_path =
      Mix.Project.build_path()
      |> Path.dirname()
      |> Path.join("#{opts[:only] || :*}/lib")

    deps_path = Mix.Project.deps_path()

    loaded_opts = if only = opts[:only], do: [env: :"#{only}"], else: []
    loaded_deps = Mix.Dep.load_on_environment(loaded_opts)

    apps_to_clean =
      cond do
        opts[:all] ->
          checked_deps(build_path, deps_path)

        opts[:unused] ->
          checked_deps(build_path, deps_path) |> filter_loaded(loaded_deps)

        apps != [] ->
          apps

        true ->
          Mix.raise(
            "\"mix deps.clean\" expects dependencies as arguments or " <>
              "an option indicating which dependencies to clean. " <>
              "The --all option will clean all dependencies while " <>
              "the --unused option cleans unused dependencies"
          )
      end

    do_clean(apps_to_clean, loaded_deps, build_path, deps_path, opts[:build])

    if opts[:unlock] do
      Mix.Task.run("deps.unlock", args)
    else
      :ok
    end
  end

  defp checked_deps(build_path, deps_path) do
    deps_names =
      for root <- [deps_path, build_path],
          path <- Path.wildcard(Path.join(root, "*")),
          File.dir?(path),
          uniq: true,
          do: Path.basename(path)

    List.delete(deps_names, to_string(Mix.Project.config()[:app]))
  end

  defp filter_loaded(apps, deps) do
    apps -- Enum.map(deps, &Atom.to_string(&1.app))
  end

  defp maybe_warn_for_invalid_path([], dependency) do
    Mix.shell().error(
      "warning: the dependency #{dependency} is not present in the build directory"
    )

    []
  end

  defp maybe_warn_for_invalid_path(paths, _dependency) do
    paths
  end

  defp do_clean(apps, deps, build_path, deps_path, build_only?) do
    shell = Mix.shell()

    local = for %{scm: scm, app: app} <- deps, not scm.fetchable?, do: Atom.to_string(app)

    Enum.each(apps, fn app ->
      shell.info("* Cleaning #{app}")

      # Remove everything from the build directory of dependencies
      build_path
      |> Path.join(to_string(app))
      |> Path.wildcard()
      |> maybe_warn_for_invalid_path(app)
      |> Enum.each(&File.rm_rf!/1)

      # Remove everything from the source directory of dependencies.
      # Skip this step if --build option is specified or if
      # the dependency is local, i.e., referenced using :path.
      if build_only? || app in local do
        :do_not_delete_source
      else
        deps_path
        |> Path.join(to_string(app))
        |> File.rm_rf!()
      end
    end)
  end
end
