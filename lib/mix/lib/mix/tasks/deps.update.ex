defmodule Mix.Tasks.Deps.Update do
  use Mix.Task

  @shortdoc "Updates the given dependencies"

  @moduledoc """
  Updates the given dependencies.

  Since this is a destructive action, updating all dependencies
  only occurs when the `--all` command line option is passed.

  All dependencies are automatically recompiled after update.

  ## Command line options

    * `--all` - updates all dependencies
    * `--only` - only fetchs dependencies for given environment
    * `--no-archives-check` - do not check archives before fetching deps
  """
  @spec run(OptionParser.argv) :: [atom]
  def run(args) do
    unless "--no-archives-check" in args do
      Mix.Task.run "archive.check", args
    end
    Mix.Project.get!
    {opts, rest, _} = OptionParser.parse(args, switches: [all: :boolean, only: :string])

    # Fetch all deps by default unless --only is given
    fetch_opts = if only = opts[:only], do: [env: :"#{only}"], else: []

    cond do
      opts[:all] ->
        Mix.Dep.Fetcher.all(Mix.Dep.Lock.read, %{}, fetch_opts)
      rest != [] ->
        {old, new} = Map.split(Mix.Dep.Lock.read, to_app_names(rest))
        Mix.Dep.Fetcher.by_name(rest, old, new, fetch_opts)
      true ->
        Mix.raise "\"mix deps.update\" expects dependencies as arguments or " <>
                                  "the --all option to update all dependencies"
    end
  end

  defp to_app_names(given) do
    Enum.map given, fn(app) ->
      if is_binary(app), do: String.to_atom(app), else: app
    end
  end
end
