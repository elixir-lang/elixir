defmodule Mix.Tasks.Deps.Get do
  use Mix.Task

  @shortdoc "Get all unavailable dependencies"

  @moduledoc """
  Get all unavailable dependencies.
  """

  import Mix.Deps, only: [all: 1, format_dep: 1, update_status: 1,deps_path: 1]

  def run(_) do
    shell = Mix.shell

    apps = Mix.Deps.Lock.update_lock all(:unavailable), fn(dep, lock) ->
      Mix.Dep[scm: scm, opts: opts] = dep
      shell.info "* Getting #{format_dep(dep)}"
      path = deps_path(dep)
      File.mkdir_p!(path)

      opts = Keyword.put(opts, :lock, lock)
      scm.get(path, opts)
    end

    Mix.Task.run "deps.compile", apps
  end
end