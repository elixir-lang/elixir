defmodule Mix.Tasks.Deps.Check do
  use Mix.Task

  import Mix.Deps, only: [all: 0, format_dep: 1, format_status: 1, check_lock: 2, out_of_date?: 1]

  @hidden true
  @shortdoc "Check if all dependencies are valid"

  @moduledoc """
  Checks if all dependencies are valid and if not, abort.
  Prints the invalid dependencies' status before aborting.

  This task is not shown in `mix help` but it is part
  of the `mix` public API and can be depended on.
  """
  def run(_) do
    lock = Mix.Deps.Lock.read
    all  = Enum.map all, &check_lock(&1, lock)

    case Enum.partition all, &ok?/1 do
      { _, [] }     -> :ok
      { _, not_ok } ->
        shell = Mix.shell
        shell.error "Unchecked dependencies for environment #{Mix.env}:"

        Enum.each not_ok, fn(dep) ->
          shell.error "* #{format_dep(dep)}"
          shell.error "  #{format_status dep}"
        end

        raise Mix.Error, message: "Can't continue due to errors on dependencies"
    end
  end

  defp ok?(Mix.Dep[status: { :ok, _ }]), do: true
  defp ok?(_),                           do: false
end
