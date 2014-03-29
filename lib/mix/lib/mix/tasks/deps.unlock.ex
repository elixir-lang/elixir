defmodule Mix.Tasks.Deps.Unlock do
  use Mix.Task

  @shortdoc "Unlock the given dependencies"

  @moduledoc """
  Unlock the given dependencies.

  Since this is a destructive action, unlocking of all dependencies
  can only happen by passing the `--all` command line option.
  """

  def run(args) do
    Mix.Project.get! # Require the project to be available
    { opts, args, _ } = OptionParser.parse(args, switches: [unlock: :boolean, all: :boolean])

    cond do
      opts[:all] ->
        Mix.Dep.Lock.write([])
      args != [] ->
        lock =
          Enum.reduce args, Mix.Dep.Lock.read, fn(arg, lock) ->
            if is_binary(arg), do: arg = binary_to_atom(arg)
            Map.delete(lock, arg)
          end

        Mix.Dep.Lock.write(lock)
      true ->
        raise Mix.Error, message: "mix deps.unlock expects dependencies as arguments or " <>
                                  "the --all option to unlock all dependencies"
    end
  end
end
