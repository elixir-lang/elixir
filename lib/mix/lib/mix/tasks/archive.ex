defmodule Mix.Tasks.Archive do
  use Mix.Task

  @shortdoc "Lists installed archives"

  @moduledoc """
  Lists all installed archives.

  Archives are typically installed at `~/.mix/archives`
  although the installation path can be customized by
  setting the `MIX_ARCHIVES` environment variable.

  Since archives are specific to Elixir versions, it is
  expected from build tools to swap the `MIX_ARCHIVES`
  variable to different locations based on a particular
  Elixir installation.
  """
  @spec run(OptionParser.argv) :: :ok
  def run(_) do
    archives =
      Mix.Local.archives_path
      |> Path.join("*.ez")
      |> Path.wildcard()
      |> Enum.map(&Path.basename/1)

    Mix.Local.Installer.print_list(archives,
      empty_message: "No archives currently installed.",
      footnote: "Archives installed at: #{Mix.Local.archives_path}")
  end
end
