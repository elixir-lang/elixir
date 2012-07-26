defmodule Mix.Tasks.Deps do
  use Mix.Task

  import Mix.Deps, only: [all: 0, format_dep: 1, format_status: 1]

  @shortdoc "List dependencies and their status"

  @moduledoc """
  List all dependencies and their status.
  """

  def run(_) do
    shell = Mix.shell

    Enum.each all, fn(dep) ->
      shell.info "* #{format_dep(dep)}"
      shell.info "  #{format_status dep.status}"
    end
  end
end