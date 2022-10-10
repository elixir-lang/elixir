defmodule Mix.Tasks.Deps.Get do
  use Mix.Task

  @shortdoc "Gets all out of date dependencies"

  @moduledoc """
  Gets all out of date dependencies, i.e. dependencies
  that are not available or have an invalid lock.

  ## Command line options

    * `--only` - only fetches dependencies for given environment
    * `--no-archives-check` - does not check archives before fetching deps
    * `--no-lockfile-update` - does not update lockfile. Exits non-zero if there are going to be changes in the lockfile

  """

  @impl true
  def run(args) do
    unless "--no-archives-check" in args do
      Mix.Task.run("archive.check", args)
    end

    Mix.Project.get!()
    {opts, _, _} = OptionParser.parse(args, switches: [only: :string, target: :string, lockfile_update: :boolean])

    fetch_opts =
      for {switch, key} <- [only: :env, target: :target, lockfile_update: :allow_updates],
          value = opts[switch],
          do: {key, :"#{value}"}

    apps = Mix.Dep.Fetcher.all(%{}, Mix.Dep.Lock.read(), fetch_opts)

    if apps == [] do
      Mix.shell().info("All dependencies are up to date")
    else
      :ok
    end
  end
end
