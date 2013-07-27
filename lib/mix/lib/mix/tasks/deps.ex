defmodule Mix.Tasks.Deps do
  use Mix.Task

  import Mix.Deps, only: [all: 0, format_dep: 1, format_status: 1, check_lock: 2]

  @shortdoc "List dependencies and their status"
  @recursive :both

  @moduledoc """
  List all dependencies and their status.
  The output is given as follows:

    * APP [VERSION] SCM: LOCATION
      [locked at REF]
      STATUS

  """
  def run(_) do
    Mix.Project.get! # Require the project to be available

    shell = Mix.shell
    lock  = Mix.Deps.Lock.read

    Enum.each all, fn(Mix.Dep[app: app, scm: scm] = dep) ->
      dep = check_lock(dep, lock)
      shell.info "* #{format_dep(dep)}"
      if (lock = lock[app]) && (formatted = scm.format_lock(lock)) do
        shell.info "  locked at #{inspect formatted}"
      end
      shell.info "  #{format_status dep}"
    end
  end
end
