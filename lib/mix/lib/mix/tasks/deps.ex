defmodule Mix.Tasks.Deps do
  use Mix.Task

  import Mix.Deps, only: [all: 0, format_dep: 1, format_status: 1]

  @shortdoc "List dependencies and their status"

  @moduledoc """
  List all dependencies and their status.
  The output is given as follow:

    * APP [VERSION] SCM: LOCATION
      [locked at REF]
      STATUS

  """
  def run(_) do
    shell = Mix.shell
    lock  = Mix.Deps.Lock.read

    Enum.each all, fn(dep) ->
      shell.info "* #{format_dep(dep)}"
      if rev = lock[dep.app] do
        shell.info "  locked at #{rev}"
      end
      shell.info "  #{format_status dep.status}"
    end
  end
end