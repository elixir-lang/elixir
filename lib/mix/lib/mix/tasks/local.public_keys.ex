defmodule Mix.Tasks.Local.PublicKeys do
  use Mix.Task

  @shortdoc "Manages public keys"

  @moduledoc """
  Public keys are used by Mix to install packages like Rebar and Hex.

  Mix by default ships with a public key but new ones can be added
  on demand.

  To list all available keys:

      $ mix local.public_keys

  To list all available keys showing the keys themselves:

      $ mix local.public_keys --detailed

  To add a new key:

      $ mix local.public_keys local/path/to/key

  Be careful when adding new keys. Only add keys from sources you
  trust.

  Public keys are by default stored in your MIX_HOME under the
  public_keys directory.

  ## Command line options

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like `make`

  """

  @impl true
  def run(argv) do
    {opts, argv} = OptionParser.parse!(argv, switches: [force: :boolean, detailed: :boolean])

    case argv do
      [] -> show(opts)
      [path | _] -> install(path, opts)
    end
  end

  defp show(opts) do
    for {id, key} <- Mix.PublicKey.public_keys() do
      Mix.shell().info("* #{id}")

      if opts[:detailed] do
        Mix.shell().info("\n#{key}")
      end
    end

    Mix.shell().info(
      "Public keys (except in-memory ones) installed at: #{Mix.PublicKey.public_keys_path()}"
    )
  end

  defp install(source, opts) do
    key = File.read!(source)
    base = Path.basename(source)
    dest = Path.join(Mix.PublicKey.public_keys_path(), base)

    # Validate the key is good
    _ = Mix.PublicKey.decode!(source, key)

    if opts[:force] || should_install?(source, dest) do
      File.mkdir_p!(Mix.PublicKey.public_keys_path())
      File.write!(dest, key)
      Mix.shell().info([:green, "* creating ", :reset, Path.relative_to_cwd(dest)])
    end
  end

  defp should_install?(source, dest) do
    if File.exists?(dest) do
      Mix.shell().yes?(
        "There is already a public key named #{Path.basename(dest)}.\n" <>
          "Are you sure you want to replace it?"
      )
    else
      Mix.shell().yes?("Are you sure you want to install public key #{source}?")
    end
  end
end
