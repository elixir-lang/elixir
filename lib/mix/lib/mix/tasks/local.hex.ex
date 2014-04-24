defmodule Mix.Tasks.Local.Hex do
  use Mix.Task

  @hex_url "https://hex.pm/installs/hex.ez"

  @shortdoc "Install hex locally"

  @moduledoc """
  Install hex locally from #{@hex_url}.

      mix local.hex

  ## Command line options

  * `--force` - forces installation without a shell prompt. Primarily
    intended for automation in build systems like make.
  """

  def run(args) do
    Mix.Tasks.Local.Install.run [@hex_url|args]
  end
end
