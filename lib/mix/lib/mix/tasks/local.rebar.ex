defmodule Mix.Tasks.Local.Rebar do
  use Mix.Task

  @s3_url             "https://hexpmrepo.global.ssl.fastly.net"
  @rebar2_list_url     "/installs/rebar-1.x.csv"
  @rebar2_escript_url  "/installs/[ELIXIR_VERSION]/rebar-[REBAR_VERSION]"
  @rebar3_list_url     "/installs/rebar3-1.x.csv"
  @rebar3_escript_url  "/installs/[ELIXIR_VERSION]/rebar3-[REBAR_VERSION]"

  @shortdoc  "Installs rebar locally"

  @moduledoc """
  Fetches a copy of `rebar` or `rebar3` from the given path or url.

  It defaults to safely download a rebar copy from  Hex's CDN.
  However, a URL can be given as argument, usually from an existing
  local copy of rebar:

      mix local.rebar rebar path/to/rebar
      mix local.rebar rebar3 path/to/rebar

  If not specified both `rebar` and `rebar3` will be fetched.

  The local copy is stored in your `MIX_HOME` (defaults to `~/.mix`).
  This version of rebar will be used as required by `mix deps.compile`.

  ## Command line options

    * `rebar PATH` - specify a path or url for `rebar`

    * `rebar3 PATH` - specify a path or url for `rebar3`

    * `--sha512` - checks the archive matches the given sha512 checksum

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like `make`

  ## Mirrors

  If you want to change the [default mirror](https://hexpmrepo.global.ssl.fastly.net)
  to use for fetching `rebar` please set the `HEX_CDN` environment variable.
  """
  @switches [force: :boolean, sha512: :string]
  @spec run(OptionParser.argv) :: true
  def run(argv) do
    {opts, argv, _} = OptionParser.parse(argv, switches: @switches)

    case argv do
      ["rebar", path | _] ->
       install_from_path(:rebar, path, opts)
      ["rebar3", path | _] ->
       install_from_path(:rebar3, path, opts)
      [] ->
        install_from_s3(:rebar, @rebar2_list_url, @rebar2_escript_url, opts)
        install_from_s3(:rebar3, @rebar3_list_url, @rebar3_escript_url, opts)
      _ ->
        Mix.raise "Invalid arguments given to mix local.rebar. " <>
                  "Check the proper call syntax with: mix help local.rebar"
    end
  end

  defp install_from_path(manager, path, opts) do
    local = Mix.Rebar.local_rebar_path(manager)

    if opts[:force] || Mix.Utils.can_write?(local) do
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

          Could not fetch #{manager} at:

              #{path}

          Please download the file above manually to your current directory and run:

              mix local.rebar #{manager} ./#{Path.basename(local)}
          """
      end
    end

    true
  end

  defp install_from_s3(manager, list_url, escript_url, opts) do
    rebar_mirror = System.get_env("HEX_CDN") || @s3_url
    list_url = rebar_mirror <> list_url

    {elixir_version, rebar_version, sha512} =
      Mix.Local.find_matching_versions_from_signed_csv!("Rebar", list_url)

    url =
      (rebar_mirror <> escript_url)
      |> String.replace("[ELIXIR_VERSION]", elixir_version)
      |> String.replace("[REBAR_VERSION]", rebar_version)

    install_from_path(manager, url, Keyword.put(opts, :sha512, sha512))
  end
end
