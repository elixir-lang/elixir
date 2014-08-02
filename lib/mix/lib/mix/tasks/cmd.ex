defmodule Mix.Tasks.Cmd do
  use Mix.Task

  @shortdoc "Executes the given command"
  @recursive true

  @moduledoc """
  Executes the given command.

  Useful in umbrella applications to execute a command
  on each child app:

      mix cmd echo pwd

  Aborts when the first command exits with status different
  than zero.
  """
  def run(args) do
    case Mix.shell.cmd(Enum.join(args, " ")) do
      0 -> :ok
      s -> exit(s)
    end
  end
end
