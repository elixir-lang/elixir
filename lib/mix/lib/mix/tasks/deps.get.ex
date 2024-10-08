defmodule Mix.Tasks.Deps.Get do
  use Mix.Task

  @shortdoc "Gets all out of date dependencies"

  @moduledoc """
  Gets all out of date dependencies, i.e. dependencies
  that are not available or have an invalid lock.

  ## Command line options

    * `--check-locked` - raises if there are pending changes to the lockfile
    * `--no-archives-check` - does not check archives before fetching deps
    * `--only` - only fetches dependencies for given environment

  """

  @impl true
  def run(args) do
    if "--no-archives-check" not in args do
      Mix.Task.run("archive.check", args)
    end

    Mix.Project.get!()

    {opts, _, _} =
      OptionParser.parse(args, switches: [only: :string, target: :string, check_locked: :boolean])

    Mix.Project.with_deps_lock(fn ->
      do_run(opts)
    end)
  end

  defp do_run(opts) do
    fetch_opts =
      for {switch, key} <- [only: :env, target: :target, check_locked: :check_locked],
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
