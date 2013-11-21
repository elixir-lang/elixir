defmodule Mix.Tasks.Deps do
  use Mix.Task

  import Mix.Deps, only: [loaded: 0, format_dep: 1, format_status: 1, check_lock: 2]

  @shortdoc "List dependencies and their status"

  @moduledoc """
  List all dependencies and their status.
  The output is given as follows:

    * APP VERSION (SCM)
      locked at REF* <optional list of refs>
      STATUS

  The star after `REF` indicates that the working dir for the dep is dirty.

  Examples of the optional list of refs:

    (origin/master)
    (tag: v0.1)
    (tags: 1.0, 2.0)

  """
  def run(_) do
    Mix.Project.get! # Require the project to be available

    shell = Mix.shell
    lock  = Mix.Deps.Lock.read

    Enum.each loaded, fn(Mix.Dep[app: app, scm: scm] = dep) ->
      dep = check_lock(dep, lock)
      shell.info "* #{format_dep(dep)}"
      if (lock = lock[app]) && (formatted = scm.format_lock(dep, lock)) do
        shell.info "  locked at #{formatted}"
      end
      shell.info "  #{format_status dep}"
    end
  end
end
