defmodule Mix.Tasks.Deps.Update do
  use Mix.Task

  @shortdoc "Update dependencies"

  @moduledoc """
  Update dependencies.

  By default, updates all dependencies. A list of deps can
  be given to update specific ones. Recompiles the given
  projects after updating.
  """

  import Mix.Tasks.Deps, only: [all: 0, all: 1, by_name: 1, format_dep: 1, deps_path: 1]

  def run([]) do
    do_update all -- all(:unavailable)
  end

  def run(args) do
    do_update by_name(args)
  end

  defp do_update(deps) do
    shell = Mix.shell

    apps = Enum.map deps, fn({ scm, app, _req, status, opts } = dep) ->
      check_unavailable!(app, status)
      shell.info "* Updating #{format_dep(dep)}"
      scm.update(deps_path(app), opts)
      app
    end

    Mix.Task.run "deps.compile", apps
  end

  defp check_unavailable!(app, { :unavailable, _ }) do
    raise Mix.Error, message: "Cannot update dependency #{app} because " <>
      "it isn't available, run `mix deps.get` first"
  end

  defp check_unavailable!(_, _) do
    :ok
  end
end