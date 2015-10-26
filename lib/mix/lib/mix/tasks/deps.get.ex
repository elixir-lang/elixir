defmodule Mix.Tasks.Deps.Get do
  use Mix.Task

  @shortdoc "Gets all out of date dependencies"

  @moduledoc """
  Gets all out of date dependencies, i.e. dependencies
  that are not available or have an invalid lock.

  ## Command line options

    * `--only`  - only fetch dependencies for given environment
  """
  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    Mix.Project.get!
    {opts, _, _} = OptionParser.parse(args, switches: [only: :string])

    # Fetch all deps by default unless --only is given
    fetch_opts = if only = opts[:only], do: [env: :"#{only}"], else: []

    apps = Mix.Dep.Fetcher.all(%{}, Mix.Dep.Lock.read, fetch_opts)

    if apps == [] do
      Mix.shell.info "All dependencies up to date"
    else
      :ok
    end
  end
end
