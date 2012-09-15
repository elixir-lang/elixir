defmodule Mix.Tasks.Deps.Unlock do
  use Mix.Task

  @shortdoc "Unlock the given dependencies"

  @moduledoc """
  Unlock the given dependencies. If no dependencies
  are given, unlock all.
  """

  import Mix.Deps, only: [all: 0, by_name!: 1]

  def run([]) do
    Mix.Deps.Lock.write([])
  end

  def run(args) do
    lock =
      Enum.reduce args, Mix.Deps.Lock.read, fn(arg, lock) ->
        if is_binary(arg), do: arg = binary_to_atom(arg)
        Keyword.delete(lock, arg)
      end

    Mix.Deps.Lock.write(lock)
  end
end