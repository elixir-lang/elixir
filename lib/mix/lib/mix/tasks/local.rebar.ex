defmodule Mix.Tasks.Local.Rebar do
  use Mix.Task

  @rebar2_list_url "/installs/rebar-1.x.csv"
  @rebar2_escript_url "/installs/[ELIXIR_VERSION]/rebar-[REBAR_VERSION]"
  @rebar3_list_url "/installs/rebar3-1.x.csv"
  @rebar3_escript_url "/installs/[ELIXIR_VERSION]/rebar3-[REBAR_VERSION]"

  @shortdoc "Installs Rebar locally"

  @moduledoc """
  Fetches a copy of `rebar` or `rebar3` from the given path or URL.

  It defaults to safely download a Rebar copy from  Hex's CDN.
  However, a URL can be given as argument, usually for an existing
  local copy of Rebar:

      mix local.rebar rebar path/to/rebar
      mix local.rebar rebar3 path/to/rebar

  If neither `rebar` or `rebar3` are specified, both versions will be fetched.

  The local copy is stored in your `MIX_HOME` (defaults to `~/.mix`).
  This version of Rebar will be used as required by `mix deps.compile`.

  ## Command line options

    * `rebar PATH` - specifies a path or URL for `rebar`

    * `rebar3 PATH` - specifies a path or URL for `rebar3`

    * `--sha512` - checks the archive matches the given SHA-512 checksum

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like `make`

  ## Mirrors

  If you want to change the [default mirror](https://repo.hex.pm)
  to use for fetching `rebar` please set the `HEX_MIRROR` environment variable.
  """

  @switches [force: :boolean, sha512: :string]

  @impl true
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
        Mix.raise(
          "Invalid arguments given to mix local.rebar. " <>
            "To find out the proper call syntax run \"mix help local.rebar\""
        )
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
          Mix.shell().info([:green, "* creating ", :reset, Path.relative_to_cwd(local)])

        :badpath ->
          Mix.raise("Expected #{inspect(path)} to be a URL or a local file path")

        {:local, message} ->
          Mix.raise(message)

        {kind, message} when kind in [:remote, :checksum] ->
          Mix.raise("""
          #{message}

          Could not fetch #{manager} at:

              #{path}

          Please download the file above manually to your current directory and run:

              mix local.rebar #{manager} ./#{Path.basename(local)}
          """)
      end
    end

    true
  end

  defp install_from_s3(manager, list_url, escript_url, opts) do
    hex_mirror = Mix.Hex.mirror()
    list_url = hex_mirror <> list_url

    {elixir_version, rebar_version, sha512} =
      Mix.Local.find_matching_versions_from_signed_csv!("Rebar", list_url)

    url =
      (hex_mirror <> escript_url)
      |> String.replace("[ELIXIR_VERSION]", elixir_version)
      |> String.replace("[REBAR_VERSION]", rebar_version)

    install_from_path(manager, url, Keyword.put(opts, :sha512, sha512))
  end
end
