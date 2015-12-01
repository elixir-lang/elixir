defmodule Mix.Shell.Quiet do
  @moduledoc """
  This is Mix's default shell when the `MIX_QUIET` environment 
  variable is set.

  It's just like `Mix.Shell.IO' but print far less.
  """

  @behaviour Mix.Shell

  @doc """
  Prints the currently running application if it
  was not printed yet.
  """
  defdelegate print_app, to: Mix.Shell.IO

  @doc """
  Executes the given command quietly without outputting anything.
  """
  def cmd(command, opts \\ []) do
    Mix.Shell.cmd(command, opts, fn data -> data end)
  end

  @doc """
  Writes nothing to the shell.
  """
  def info(_message), do: nil

  @doc """
  Writes an error message to the shell followed by new line.
  """
  defdelegate error(message), to: Mix.Shell.IO

  @doc """
  Writes a message shell followed by prompting the user for
  input. Input will be consumed until enter is pressed.
  """
  defdelegate prompt(message), to: Mix.Shell.IO

  @doc """
  Receives a message and asks the user if they want to proceed.
  The user must press enter or type anything that matches the "yes"
  regex `~r/^Y(es)?$/i`.
  """
  defdelegate yes?(message), to: Mix.Shell.IO
end
