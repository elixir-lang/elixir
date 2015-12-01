defmodule Mix.Tasks.Local.Rebar do
  use Mix.Task

  @rebar_s3           "http://s3.amazonaws.com/s3.hex.pm"
  @rebar_list_path    "/installs/rebar-1.x.csv"
  @rebar_escript_path "/installs/[ELIXIR_VERSION]/rebar-[REBAR_VERSION]"

  @shortdoc  "Installs rebar locally"

  @moduledoc """
  Fetches a copy of `rebar` from the given path or url.

  It defaults to safely download a `rebar` copy from
  [Amazon S3](https://aws.amazon.com/s3/). However, a URL
  can be given as argument, usually from an existing local copy of `rebar`.

  The local copy is stored in your `MIX_HOME` (defaults to `~/.mix`).
  This version of `rebar` will be used as required by `mix deps.compile`.

  ## Command line options

    * `--sha512` - checks the archive matches the given sha512 checksum

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like `make`

  ## Mirrors

  If you want to change the [default mirror](http://s3.amazonaws.com/s3.hex.pm)
  to use for fetching `rebar` please set the `HEX_CDN` environment variable.
  """
  @switches [force: :boolean, sha512: :string]
  @spec run(OptionParser.argv) :: true
  def run(argv) do
    {opts, argv, _} = OptionParser.parse(argv, switches: @switches)

    case argv do
      [path|_] -> install_from_path(path, opts)
      []       -> install_from_s3(opts)
    end
  end

  defp install_from_path(path, opts) do
    local = Mix.Rebar.local_rebar_path

    if opts[:force] || Mix.Utils.can_write?(path) do
      case Mix.Utils.read_path(path, opts) do
        {:ok, binary} ->
          File.mkdir_p!(Path.dirname(local))
          File.write!(local, binary)
          File.chmod!(local, 0o755)
          Mix.shell.info [:green, "* creating ", :reset, Path.relative_to_cwd(local)]
        :badpath ->
          Mix.raise "Expected #{inspect path} to be a url or a local file path"
        {:local, message} ->
          Mix.raise message
        {kind, message} when kind in [:remote, :checksum] ->
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

  defp install_from_s3(opts) do
    rebar_mirror = System.get_env("HEX_CDN") || @rebar_s3

    {elixir_version, rebar_version, sha512} =
      Mix.Local.find_matching_versions_from_signed_csv!("Rebar", rebar_mirror <> @rebar_list_path)

    url =
      (rebar_mirror <> @rebar_escript_path)
      |> String.replace("[ELIXIR_VERSION]", elixir_version)
      |> String.replace("[REBAR_VERSION]", rebar_version)

    install_from_path(url, Keyword.put(opts, :sha512, sha512))
  end
end
