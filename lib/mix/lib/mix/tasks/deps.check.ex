defmodule Mix.Tasks.Deps.Check do
  use Mix.Task

  import Mix.Tasks.Deps, only: [all: 0, format_dep: 1, format_status: 1]

  @hidden true
  @shortdoc "Check if all dependencies are ok"

  @moduledoc """
  Checks if all dependencies are valid and if not, abort.
  Prints the invalid dependencies status before aborting.

  This task is not shown in `mix help` but it is part
  of mix public API and can be depended on.
  """
  def run(_) do
    case Enum.partition all, match?({ _, _, _, { :ok, _ }, _ }, &1) do
      { _, [] }     -> :ok
      { _, not_ok } ->
        if Enum.all? not_ok, match?({ _, _, _, { :unavailable, _ }, _ }, &1) do
          raise Mix.NotMetDepsError, message:
            "Dependencies are not available, run `mix deps.get` before proceeding"
        else
          shell = Mix.shell

          Enum.each not_ok, fn(dep) ->
            shell.error "* #{format_dep(dep)}"
            shell.error "  #{format_status elem(dep, 4)}"
          end

          raise Mix.NotMetDepsError
        end
    end
  end
end
