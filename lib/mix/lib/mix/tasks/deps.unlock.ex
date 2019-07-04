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
    * `--check-unused` - checks that the mix.lock file has no unused
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
        apps = Mix.Dep.load_on_environment([]) |> Enum.map(& &1.app)

        unused_apps =
          Mix.Dep.Lock.read()
          |> Map.drop(apps)
          |> Map.keys()
          |> Enum.sort()

        unless unused_apps == [] do
          Mix.raise(
            "Unused dependencies in mix.lock file: #{inspect(unused_apps, limit: :infinity)}"
          )
        end

      opts[:unused] ->
        apps = Mix.Dep.load_on_environment([]) |> Enum.map(& &1.app)
        Mix.Dep.Lock.read() |> Map.take(apps) |> Mix.Dep.Lock.write()

      filter = opts[:filter] ->
        lock = Mix.Dep.Lock.read()
        apps = Map.keys(lock)

        unlock = Enum.filter(apps, &(Atom.to_string(&1) =~ filter))

        if unlock == [] do
          Mix.shell().error("warning: no dependencies were matched")
        else
          lock = Enum.reject(lock, fn {app, _} -> app in unlock end)
          Mix.Dep.Lock.write(lock)

          Mix.shell().info("""
          Unlocked deps:
          * #{Enum.join(unlock, "\n* ")}
          """)
        end

      apps != [] ->
        lock =
          Enum.reduce(apps, Mix.Dep.Lock.read(), fn app_str, lock ->
            app = String.to_atom(app_str)

            if Map.has_key?(lock, app) do
              Map.delete(lock, app)
            else
              Mix.shell().error("warning: #{app} dependency is not locked")
              lock
            end
          end)

        Mix.Dep.Lock.write(lock)

      true ->
        Mix.raise(
          "\"mix deps.unlock\" expects dependencies as arguments or " <>
            "an option indicating which dependencies to unlock. " <>
            "The --all option will unlock all dependencies while " <>
            "the --unused option unlocks unused dependencies"
        )
    end
  end
end
