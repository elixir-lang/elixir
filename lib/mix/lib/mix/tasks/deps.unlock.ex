defmodule Mix.Tasks.Deps.Unlock do
  use Mix.Task

  @shortdoc "Unlock the given dependencies"

  @moduledoc """
  Unlock the given dependencies. If no dependencies
  are given, unlock all.
  """

  import Mix.Deps, only: [all: 0, by_name: 1]

  def run([]) do
    do_unlock all
  end

  def run(args) do
    do_unlock by_name(args)
  end

  defp do_unlock(deps) do
    lock =
      Enum.reduce deps, Mix.Deps.Lock.read, fn(dep, lock) ->
        Keyword.delete(lock, dep.app)
      end

    Mix.Deps.Lock.write(lock)
  end
end