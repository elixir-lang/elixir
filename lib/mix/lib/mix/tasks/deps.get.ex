defmodule Mix.Tasks.Deps.Get do
  use Mix.Task

  @shortdoc "Get all unavailable dependencies"

  @moduledoc """
  Get all unavailable dependencies.
  """

  import Mix.Deps, only: [all: 1, format_dep: 1, update_status: 1,deps_path: 1, deps_path: 0]

  def run(_) do
    shell = Mix.shell
    File.mkdir_p!(deps_path)

    apps = Enum.reduce all(:unavailable), [], fn(dep, acc) ->
      Mix.Dep[scm: scm, app: app, opts: opts] = dep

      shell.info "* Getting #{format_dep(dep)}"
      scm.get(deps_path(app), opts)

      case update_status(dep) do
        Mix.Dep[status: { :unavailable, _ }] -> acc
        _ -> [app|acc]
      end
    end

    Mix.Task.run "deps.compile", apps
  end
end