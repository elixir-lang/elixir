defmodule Mix.Tasks.Deps.Update do
  use Mix.Task

  @shortdoc "Update the given dependencies"

  @moduledoc """
  Update the given dependencies.

  Since this is a destructive action, update of all dependencies
  can only happen by passing the `--all` command line option.

  All dependencies are automatically recompiled after update.

  ## Command line options

  * `--all` - update all dependencies
  * `--no-compile` - skip compilation of dependencies
  * `--no-deps-check` - skip dependency check
  * `--quiet` - do not output verbose messages

  """

  import Mix.Deps, only: [ unfetched: 2, unfetched_by_name: 3, available?: 1, format_dep: 1 ]

  def run(args) do
    Mix.Project.get! # Require the project to be available
    { opts, rest, _ } = OptionParser.parse(args, switches: [no_compile: :boolean, all: :boolean])

    cond do
      opts[:all] ->
        acc = unfetched(init, &deps_updater/2)
      rest != [] ->
        acc = unfetched_by_name(rest, init, &deps_updater/2)
      true ->
        raise Mix.Error, message: "mix deps.update expects dependencies as arguments or " <>
                                  "the --all option to update all dependencies"
    end

    finalize_update(acc, opts)
  end

  defp init do
    { [], Mix.Deps.Lock.read }
  end

  defp finalize_update({ apps, lock }, opts) do
    apps = Enum.reverse(apps)
    Mix.Deps.Lock.write(lock)

    unless opts[:no_compile] do
      args = if opts[:quiet], do: ["--quiet"|apps], else: apps
      Mix.Task.run("deps.compile", args)
      unless opts[:no_deps_check] do
        Mix.Task.run("deps.check", [])
      end
    end
  end

  defp deps_updater(dep, { acc, lock }) do
    if available?(dep) do
      Mix.Dep[app: app, scm: scm, opts: opts] = dep
      Mix.shell.info "* Updating #{format_dep(dep)}"

      lock =
        if latest = scm.update(opts) do
          Keyword.put(lock, app, latest)
        else
          lock
        end

      { dep, { [app|acc], lock } }
    else
      { dep, { acc, lock } }
    end
  end
end
