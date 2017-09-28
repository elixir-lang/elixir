defmodule Mix.Shell.Quiet do
  @moduledoc """
  This is Mix's default shell when the `MIX_QUIET` environment
  variable is set.

  It's just like `Mix.Shell.IO`, but prints far less.
  """

  @behaviour Mix.Shell

  @doc """
  Prints the current application if it
  was not printed yet.
  """
  defdelegate print_app, to: Mix.Shell.IO

  @doc """
  Writes nothing to the shell.
  """
  def write(_data), do: :ok

  @doc """
  Prints nothing to the shell.
  """
  def info(_message), do: :ok

  @doc """
  Prints the error to the shell followed by a newline.
  """
  defdelegate error(message), to: Mix.Shell.IO

  @doc """
  Prints a message and prompts the user for input.

  Input will be consumed until Enter is pressed.
  """
  defdelegate prompt(message), to: Mix.Shell.IO

  @doc """
  Prints a message and asks the user if they want to proceed.

  The user must press Enter or type one of "y", "yes", "Y", "YES" or
  "Yes".
  """
  defdelegate yes?(message), to: Mix.Shell.IO

  @doc false
  # TODO: Deprecate on Elixir v1.8
  def cmd(command, opts \\ []) do
    Mix.Shell.cmd(command, opts, fn data -> data end)
  end
end
