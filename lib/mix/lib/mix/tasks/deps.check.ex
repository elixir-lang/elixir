defmodule Mix.Tasks.Deps.Check do
  use Mix.Task

  import Mix.Deps, only: [all: 0, format_dep: 1, format_status: 1]

  @hidden true
  @shortdoc "Check if all dependencies are ok"

  @moduledoc """
  Checks if all dependencies are valid and if not, abort.
  Prints the invalid dependencies status before aborting.

  This task is not shown in `mix help` but it is part
  of mix public API and can be depended on.
  """
  def run(_) do
    lock = Mix.Deps.Lock.read

    case Enum.partition all, check?(&1, lock) do
      { _, [] }     -> :ok
      { _, not_ok } ->
        if Enum.all? not_ok, unavailable?(&1) do
          raise Mix.NotMetDepsError, message:
            "Dependencies are not available, run `mix deps.get` before proceeding"
        else
          shell = Mix.shell

          Enum.each not_ok, fn(dep) ->
            shell.error "* #{format_dep(dep)}"
            if rev = lock[dep.app] do
              shell.error "  locked at #{rev}"
            else
              shell.error "  error: not locked"
            end
            shell.error "  #{format_status dep.status}"
          end

          raise Mix.NotMetDepsError
        end
    end
  end

  defp unavailable?(Mix.Dep[status: { :unavailable, _ }]), do: true
  defp unavailable?(_dep), do: false

  defp check?(Mix.Dep[app: app, status: { :ok, _ }], lock), do: lock[app]
  defp check?(_dep, _lock), do: false
end
