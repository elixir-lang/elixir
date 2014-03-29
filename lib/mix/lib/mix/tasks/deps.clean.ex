defmodule Mix.Tasks.Deps.Clean do
  use Mix.Task

  @shortdoc "Remove the given dependencies' files"

  @moduledoc """
  Remove the given dependencies' files.

  Since this is a destructive action, cleaning of all dependencies
  can only happen by passing the `--all` command line option.

  Clean does not unlock the dependencies, unless `--unlock` is given.
  """
  def run(args) do
    Mix.Project.get! # Require the project to be available

    { opts, args, _ } = OptionParser.parse(args, switches: [unlock: :boolean, all: :boolean])
    loaded = Mix.Dep.loaded(env: Mix.env)

    cond do
      opts[:all] ->
        do_clean Enum.map(loaded, &(&1.app)), loaded, opts
      args != [] ->
        do_clean args, loaded, opts
      true ->
        raise Mix.Error, message: "mix deps.clean expects dependencies as arguments or " <>
                                  "the --all option to clean all dependencies"
    end
  end

  defp do_clean(apps, loaded, opts) do
    shell = Mix.shell
    build = Mix.Project.build_path |> Path.join("lib")
    deps  = Mix.Project.deps_path

    Enum.each apps, fn(app) ->
      shell.info "* Cleaning #{app}"
      load_paths =
        if dep = Enum.find(loaded, &(&1.app == app)) do
          Mix.Dep.load_paths(dep)
        else
          [Path.join([build, app, "ebin"])]
        end

      Enum.each(load_paths, &(&1 |> Path.dirname |> File.rm_rf!))
      File.rm_rf!(Path.join(deps, app))
    end

    if opts[:unlock] do
      Mix.Task.run "deps.unlock", apps
    end
  end
end
