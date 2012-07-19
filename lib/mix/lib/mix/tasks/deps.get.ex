defmodule Mix.Tasks.Deps.Get do
  use Mix.Task

  @shortdoc "Get all unavailable dependencies"

  @moduledoc """
  Get all unavailable dependencies.
  """

  import Mix.Tasks.Deps, only: [all: 1, format_dep: 1, deps_path: 1, deps_path: 0]

  def run(_) do
    shell = Mix.shell
    File.mkdir_p!(deps_path)

    Enum.map all(:unavailable), fn({ scm, app, _req, _status, opts } = dep) ->
      shell.info "* Getting #{format_dep(dep)}"
      scm.get(deps_path(app), opts)
      app
    end
  end
end