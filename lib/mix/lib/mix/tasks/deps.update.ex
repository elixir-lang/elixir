defmodule Mix.Tasks.Deps.Update do
  use Mix.Task

  @shortdoc "Update the given dependencies"

  @moduledoc """
  Update the given dependencies.

  Since this is a destructive action, update of all dependencies
  can only happen by passing the `--all` command line option.

  All dependencies are automatically recompiled after update.

  ## Command line options

  * `--all` - update all dependencies
  """
  def run(args) do
    Mix.Project.get! # Require the project to be available
    { opts, rest, _ } = OptionParser.parse(args, switches: [no_compile: :boolean, all: :boolean])

    cond do
      opts[:all] ->
        Mix.Dep.Fetcher.all(Mix.Dep.Lock.read, %{}, [])
      rest != [] ->
        { old, new } = Dict.split(Mix.Dep.Lock.read, to_app_names(rest))
        Mix.Dep.Fetcher.by_name(rest, old, new, [])
      true ->
        raise Mix.Error, message: "mix deps.update expects dependencies as arguments or " <>
                                  "the --all option to update all dependencies"
    end
  end

  defp to_app_names(given) do
    Enum.map given, fn(app) ->
      if is_binary(app), do: binary_to_atom(app), else: app
    end
  end
end
