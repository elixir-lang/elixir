# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Mix.Tasks.Deps.Get do
  use Mix.Task

  @shortdoc "Fetches unavailable and out of date dependencies"

  @moduledoc """
  Fetches unavailable and out of date dependencies.

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
      Mix.shell().info("All dependencies have been fetched")
    else
      :ok
    end
  end
end
