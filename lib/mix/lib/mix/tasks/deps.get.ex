defmodule Mix.Tasks.Deps.Get do
  use Mix.Task

  @shortdoc "Get all out of date dependencies"

  @moduledoc """
  Get all out of date dependencies, i.e. dependencies
  that are not available or have an invalid lock.

  ## Command line options

  * `--quiet` - do not output verbose messages
  * `--only` - only fetch dependencies for given environment
  """
  def run(args) do
    Mix.Project.get! # Require the project to be available
    {opts, rest, _} = OptionParser.parse(args, switches: [quiet: :boolean, only: :string])

    # Fetch all deps by default unless --only is given
    fetch_opts = if only = opts[:only], do: [env: :"#{only}"], else: []

    apps =
      if rest != [] do
        Mix.Dep.Fetcher.by_name(rest, %{}, Mix.Dep.Lock.read, fetch_opts)
      else
        Mix.Dep.Fetcher.all(%{}, Mix.Dep.Lock.read, fetch_opts)
      end

    if apps == [] && !opts[:quiet] do
      Mix.shell.info "All dependencies up to date"
    end
  end
end
