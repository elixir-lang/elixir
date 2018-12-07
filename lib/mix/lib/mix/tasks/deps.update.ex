defmodule Mix.Tasks.Deps.Update do
  use Mix.Task

  @shortdoc "Updates the given dependencies"

  @moduledoc """
  Updates the given dependencies.

  The given dependencies and the projects they depend on will
  be unlocked and updated to the latest version according to their
  version requirements.

  Since this is a destructive action, updating all dependencies
  only occurs when the `--all` command line option is passed.

  All dependencies are automatically recompiled after update.

  ## mix deps.unlock + mix deps.get

  Upgrading a dependency often requires the projects it depends on
  to upgrade too. If you would rather update a single dependency and
  not touch its children, you can explicitly unlock the single dependency
  and run `mix deps.get`:

      $ mix deps.unlock some_dep
      $ mix deps.get

  ## Command line options

    * `--all` - updates all dependencies
    * `--only` - only fetches dependencies for given environment
    * `--target` - only fetches dependencies for given target
    * `--no-archives-check` - does not check archives before fetching deps

  """

  @impl true
  def run(args) do
    unless "--no-archives-check" in args do
      Mix.Task.run("archive.check", args)
    end

    Mix.Project.get!()

    {opts, rest, _} =
      OptionParser.parse(args, switches: [all: :boolean, only: :string, target: :string])

    fetch_opts =
      for {switch, key} <- [only: :env, target: :target],
          value = opts[switch],
          do: {key, :"#{value}"}

    cond do
      opts[:all] ->
        Mix.Dep.Fetcher.all(Mix.Dep.Lock.read(), %{}, fetch_opts)

      rest != [] ->
        {old, new} = Map.split(Mix.Dep.Lock.read(), to_app_names(rest))
        Mix.Dep.Fetcher.by_name(rest, old, new, fetch_opts)

      true ->
        Mix.raise(
          "\"mix deps.update\" expects dependencies as arguments or " <>
            "the --all option to update all dependencies"
        )
    end
  end

  defp to_app_names(given) do
    Enum.map(given, fn app ->
      if is_binary(app), do: String.to_atom(app), else: app
    end)
  end
end
