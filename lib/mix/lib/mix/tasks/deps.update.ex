defmodule Mix.Tasks.Deps.Update do
  use Mix.Task

  @shortdoc "Update the given dependencies"
  @recursive :both

  @moduledoc """
  Update the given dependencies.

  Since this is a destructive action, update of all dependencies
  can only happen by passing the `--all` command line option.

  All dependencies are automatically recompiled after update.

  ## Command line options

  * `--all` - update all dependencies
  * `--no-compile` - skip compilation of dependencies
  * `--no-deps-check` - skip dependency check
  """

  import Mix.Deps, only: [ all: 0, all: 2, available?: 1, by_name: 2,
                           depending: 2, format_dep: 1 ]

  def run(args) do
    Mix.Project.get! # Require the project to be available
    { opts, rest } = OptionParser.parse(args, switches: [no_compile: :boolean, all: :boolean])

    cond do
      opts[:all] ->
        acc = all(init, deps_updater(&1, &2))
      rest != [] ->
        all_deps = all
        deps = Enum.map by_name(rest, all_deps), check_unavailable!(&1)
        deps = deps ++ depending(deps, all_deps)
        { _, acc } = Enum.map_reduce deps, init, deps_updater(&1, &2)
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
    Mix.Deps.Lock.write(lock)
    unless opts[:no_compile] do
      Mix.Task.run("deps.compile", apps)
      unless opts[:no_deps_check], do: Mix.Task.run("deps.check", [])
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

      { Mix.Deps.update(dep), { [app|acc], lock } }
    else
      { dep, { acc, lock } }
    end
  end

  defp check_unavailable!(dep) do
    unless available?(dep) do
      raise Mix.Error, message: "Cannot update dependency #{dep.app} because " <>
        "it isn't available, run `mix deps.get` first"
    end
    dep
  end
end
