defmodule Mix.Tasks.Local.Rebar do
  use Mix.Task

  @rebar_url "http://s3.hex.pm/rebar"
  @shortdoc  "Install rebar locally"

  @moduledoc """
  Fetch a copy of rebar from the given path or url. It defaults to a
  rebar copy that ships with Elixir source if available or fetches it
  from #{@rebar_url}.

  The local copy is stored in your MIX_HOME (defaults to ~/.mix).
  This version of rebar will be used as required by `mix deps.compile`.

  ## Command line options

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like make
  """
  @spec run(OptionParser.argv) :: true
  def run(argv) do
    {opts, argv, _} = OptionParser.parse(argv, switches: [force: :boolean])

    path = case argv do
      []       -> @rebar_url
      [path|_] -> path
    end

    local = Mix.Rebar.local_rebar_path

    if opts[:force] || Mix.Utils.can_write?(path) do
      case Mix.Utils.read_path(path, opts) do
        {:ok, binary} ->
          File.mkdir_p!(Path.dirname(local))
          File.write!(local, binary)
          File.chmod!(local, 0o755)
          Mix.shell.info [:green, "* creating ", :reset, Path.relative_to_cwd(local)]
        :badname ->
          Mix.raise "Expected #{inspect path} to be a url or a local file path"
        {:local, message} ->
          Mix.raise message
        {:remote, message} ->
          Mix.raise """
          #{message}

          Could not fetch rebar at:

              #{path}

          Please download the file above manually to your current directory and run:

              mix local.rebar ./#{Path.basename(local)}
          """
      end
    end

    :ok
  end
end
