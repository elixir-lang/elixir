defmodule Mix.Tasks.Deps.Unlock do
  use Mix.Task

  @shortdoc "Unlocks the given dependencies"

  @moduledoc """
  Unlocks the given dependencies.

  Since this is a destructive action, unlocking dependencies
  only occurs when passing arguments/options:

    * `dep1 dep2` - the name of dependencies to be unlocked
    * `--all` - unlocks all dependencies
    * `--filter` - unlocks only deps matching the given name
    * `--unused` - unlocks only unused dependencies (no longer mentioned
      in the `mix.exs` file)
    * `--check-unused` - checks that the `mix.lock` file has no unused
      dependencies. This is useful in pre-commit hooks and CI scripts
      if you want to reject contributions with extra dependencies

  """

  @switches [all: :boolean, check_unused: :boolean, unused: :boolean, filter: :string]

  @impl true
  def run(args) do
    Mix.Project.get!()
    {opts, apps, _} = OptionParser.parse(args, switches: @switches)

    cond do
      opts[:all] ->
        Mix.Dep.Lock.write(%{})

      opts[:check_unused] ->
        lock = Mix.Dep.Lock.read()
        unused_apps = unused_apps(lock)

        unless unused_apps == [] do
          Mix.raise("""
          Unused dependencies in mix.lock file:

          #{Enum.map_join(unused_apps, "\n", fn app -> "  * #{inspect(app)}" end)}
          """)
        end

      opts[:unused] ->
        lock = Mix.Dep.Lock.read()
        unused_apps = unused_apps(lock)

        unless unused_apps == [] do
          unlock(lock, unused_apps)
        end

      filter = opts[:filter] ->
        lock = Mix.Dep.Lock.read()
        apps = lock |> Map.keys() |> Enum.filter(&(Atom.to_string(&1) =~ filter))

        if apps == [] do
          Mix.shell().error("warning: no dependencies were matched")
        else
          unlock(lock, apps)
        end

      apps != [] ->
        lock = Mix.Dep.Lock.read()
        apps = Enum.map(apps, &String.to_atom/1)
        unlocked = apps -- Map.keys(lock)

        for app <- unlocked do
          Mix.shell().error("warning: #{app} dependency is not locked")
        end

        unlock(lock, apps -- unlocked)

      true ->
        Mix.raise(
          "\"mix deps.unlock\" expects dependencies as arguments or " <>
            "an option indicating which dependencies to unlock. " <>
            "The --all option will unlock all dependencies while " <>
            "the --unused option unlocks unused dependencies"
        )
    end
  end

  defp unused_apps(lock) do
    apps = Mix.Dep.load_on_environment([]) |> Enum.map(& &1.app)

    lock
    |> Map.drop(apps)
    |> Map.keys()
    |> Enum.sort()
  end

  defp unlock(lock, apps) do
    lock |> Map.drop(apps) |> Mix.Dep.Lock.write()

    Mix.shell().info("""
    Unlocked deps:
    * #{Enum.join(apps, "\n* ")}
    """)
  end
end
