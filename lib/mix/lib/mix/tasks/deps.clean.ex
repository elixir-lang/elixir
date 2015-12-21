defmodule Mix.Tasks.Deps.Clean do
  use Mix.Task

  @shortdoc "Deletes the given dependencies' files"

  @moduledoc """
  Deletes the given dependencies' files, including build artifacts and fetched
  sources.

  Since this is a destructive action, cleaning of dependencies
  can only happen by passing arguments/options:

    * `dep1, dep2` - the name of dependencies to be deleted
    * `--all`      - deletes all dependencies
    * `--unused`   - deletes only unused dependencies (no longer mentioned
      in the `mix.exs` file)
    * `--unlock`   - also unlocks the deleted dependencies
    * `--build`    - deletes only dependencies compiled files (keep the source files)

  By default this task works across all environments, unless `--only`
  is given.
  """

  @switches [unlock: :boolean, all: :boolean, only: :string, unused: :boolean,
             build: :boolean]

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    Mix.Project.get!
    {opts, apps, _} = OptionParser.parse(args, switches: @switches)

    build = Mix.Project.build_path
            |> Path.dirname
            |> Path.join("#{opts[:only] || :*}/lib")
    deps = Mix.Project.deps_path

    apps_to_clean = cond do
      opts[:all] -> checked_deps(build, deps)
      opts[:unused] -> checked_deps(build, deps) |> filter_loaded(opts)
      apps != [] -> apps
      true ->
        Mix.raise "\"mix deps.clean\" expects dependencies as arguments or " <>
                  "a flag indicating which dependencies to clean. " <>
                  "The --all option will clean all dependencies while " <>
                  "the --unused option cleans unused dependencies"
    end

    do_clean(apps_to_clean, build, deps, opts[:build])

    if opts[:unlock] do
      Mix.Task.run "deps.unlock", args
    else
      :ok
    end
  end

  defp checked_deps(build, deps) do
    for root <- [deps, build],
        path <- Path.wildcard(Path.join(root, "*")),
        File.dir?(path) do
      Path.basename(path)
    end
    |> Enum.uniq()
    |> List.delete(to_string(Mix.Project.config[:app]))
  end

  defp filter_loaded(apps, opts) do
    load_opts = if only = opts[:only], do: [env: :"#{only}"], else: []
    load_deps = Mix.Dep.loaded(load_opts) |> Enum.map(&Atom.to_string(&1.app))
    Enum.reject(apps, &(&1 in load_deps))
  end

  defp do_clean(apps, build, deps, build_only?) do
    shell = Mix.shell

    Enum.each apps, fn(app) ->
      shell.info "* Cleaning #{app}"

      build
      |> Path.join(to_string(app))
      |> Path.wildcard
      |> Enum.each(&File.rm_rf!/1)

      unless build_only? do
        deps
        |> Path.join(to_string(app))
        |> File.rm_rf!
      end
    end
  end
end
