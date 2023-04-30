defmodule Mix.Tasks.Local.Rebar do
  use Mix.Task

  @rebar3_list_url "/installs/rebar3-1.x.csv"
  @rebar3_escript_url "/installs/[ELIXIR_VERSION]/rebar3-[REBAR_VERSION]"

  @shortdoc "Installs Rebar locally"

  @moduledoc """
  Fetches a copy of `rebar3` from the given path or URL.

  It defaults to safely download a Rebar copy from Hex's CDN.
  However, a URL can be given as an argument, usually for an existing
  local copy of Rebar:

      $ mix local.rebar rebar3 path/to/rebar

  The local copy is stored in your `MIX_HOME` (defaults to `~/.mix`)
  according to the current Elixir. The installed version of Rebar will
  be used whenever required by `mix deps.compile`.

  ## Command line options

    * `rebar3 PATH` - specifies a path for `rebar3`

    * `--sha512` - checks the Rebar script matches the given SHA-512 checksum

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like `make`

    * `--if-missing` - performs installation only if not installed yet;
      intended to avoid repeatedly reinstalling in automation when a script
      may be run multiple times

  ## Mirrors

  If you want to change the [default mirror](https://builds.hex.pm)
  to use for fetching `rebar` please set the `HEX_BUILDS_URL` environment variable.
  """

  @switches [force: :boolean, sha512: :string, if_missing: :boolean]

  @impl true
  def run(argv) do
    {opts, args, _} = OptionParser.parse(argv, switches: @switches)

    case args do
      ["rebar3", path | _] ->
        maybe_install_from_path(:rebar3, path, opts)

      [] ->
        maybe_install_from_s3(:rebar3, @rebar3_list_url, @rebar3_escript_url, opts)

      _ ->
        Mix.raise(
          "Invalid arguments given to mix local.rebar: #{inspect(args)}. " <>
            "To find out the proper call syntax run \"mix help local.rebar\""
        )
    end
  end

  defp maybe_install_from_path(manager, path, opts) do
    if not skip_install?(manager, opts) do
      install_from_path(manager, path, opts)
    end
  end

  defp maybe_install_from_s3(manager, list_url, escript_url, opts) do
    if not skip_install?(manager, opts) do
      install_from_s3(manager, list_url, escript_url, opts)
    end
  end

  defp install_from_path(manager, path, opts) do
    local = Mix.Rebar.local_rebar_path(manager)

    if opts[:force] || Mix.Generator.overwrite?(local) do
      case Mix.Utils.read_path(path, opts) do
        {:ok, binary} ->
          File.mkdir_p!(Path.dirname(local))
          File.write!(local, binary)
          File.chmod!(local, 0o755)
          Mix.shell().info([:green, "* creating ", :reset, Path.relative_to_cwd(local)])

        :badpath ->
          Mix.raise("Expected #{inspect(path)} to be a local file path")

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
    hex_url = Mix.Hex.url()
    list_url = hex_url <> list_url

    {elixir_version, rebar_version, sha512} =
      Mix.Local.find_matching_versions_from_signed_csv!("Rebar", _version = nil, list_url)

    url =
      (hex_url <> escript_url)
      |> String.replace("[ELIXIR_VERSION]", elixir_version)
      |> String.replace("[REBAR_VERSION]", rebar_version)

    install_from_path(manager, url, Keyword.put(opts, :sha512, sha512))
  end

  defp skip_install?(manager, opts) do
    Keyword.get(opts, :if_missing, false) and
      manager
      |> Mix.Rebar.local_rebar_path()
      |> File.exists?()
  end
end
