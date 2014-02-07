defmodule Mix.Tasks.Deps.Get do
  use Mix.Task

  @shortdoc "Get all out of date dependencies"

  @moduledoc """
  Get all out of date dependencies, i.e. dependencies
  that are not available or have an invalid lock.

  ## Command line options

  * `--quiet` - do not output verbose messages
  """
  def run(args) do
    Mix.Project.get! # Require the project to be available
    { opts, rest, _ } = OptionParser.parse(args, switches: [no_compile: :boolean, quiet: :boolean])

    apps =
      if rest != [] do
        Mix.Deps.Fetcher.by_name(rest, [], Mix.Deps.Lock.read)
      else
        Mix.Deps.Fetcher.all([], Mix.Deps.Lock.read)
      end

    if apps == [] && !opts[:quiet] do
      Mix.shell.info "All dependencies up to date"
    end
  end
end
