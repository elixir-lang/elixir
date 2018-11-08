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

  @impl true
  def run(_) do
    Mix.Local.path_for(:archive)
    |> Path.join("*")
    |> Path.wildcard()
    |> Enum.map(&Path.basename/1)
    |> print()
  end

  defp print([]) do
    Mix.shell().info("No archives currently installed.")
  end

  defp print(items) do
    Enum.each(items, fn item -> Mix.shell().info(["* ", item]) end)
    Mix.shell().info("Archives installed at: #{Mix.Local.path_for(:archive)}")
  end
end
