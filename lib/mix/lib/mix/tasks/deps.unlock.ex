defmodule Mix.Tasks.Deps.Unlock do
  use Mix.Task

  @shortdoc "Unlocks the given dependencies"

  @moduledoc """
  Unlocks the given dependencies.

  Since this is a destructive action, unlocking of dependencies
  can only happen by passing arguments/options:

    * `dep1 dep2` - the name of dependencies to be unlocked
    * `--all` - unlocks all dependencies
    * `--unused` - unlocks only unused dependencies (no longer mentioned
      in the `mix.exs` file)

  """

  @switches [all: :boolean, unused: :boolean, filter: :string]

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    Mix.Project.get!
    {opts, apps, _} = OptionParser.parse(args, switches: @switches)

    cond do
      opts[:all] ->
        Mix.Dep.Lock.write(%{})
      opts[:unused] ->
        apps = Mix.Dep.loaded([]) |> Enum.map(& &1.app)
        Mix.Dep.Lock.read() |> Map.take(apps) |> Mix.Dep.Lock.write()
      filter = opts[:filter] ->
        lock = Mix.Dep.Lock.read
        apps = Map.keys(lock)

        unlock =
          apps
          |> Enum.filter(&String.contains?("#{&1}", filter))

        if unlock == [] do
          Mix.shell.error "warning: no dependencies were matched"
        else
          lock =
            Enum.reject(lock, fn({app, _}) ->
              app in unlock
            end)
          Mix.Dep.Lock.write(lock)
          Mix.shell.info """
          Unlocked deps:
          * #{Enum.join(unlock, "\n* ")}
          """
        end

      apps != [] ->
        lock =
          Enum.reduce apps, Mix.Dep.Lock.read, fn(app_str, lock) ->
            app = String.to_atom(app_str)
            if Map.has_key?(lock, app) do
              Map.delete(lock, app)
            else
              Mix.shell.error "warning: #{app} dependency is not locked"
              lock
            end
          end
        Mix.Dep.Lock.write(lock)

      true ->
        Mix.raise "\"mix deps.unlock\" expects dependencies as arguments or " <>
                  "the --all option to unlock all dependencies"
    end
  end
end
