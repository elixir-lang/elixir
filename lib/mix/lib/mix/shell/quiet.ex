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
  Prints a message and asks the user to confirm if they
  want to proceed. The user must type and submit one of
  "y", "yes", "Y", "YES" or "Yes".

  The user may also press Enter; this can be configured
  to either accept or reject the prompt. The latter case
  may be useful for a potentially dangerous operation that
  should require explicit confirmation from the user.

  ## Options

    * `:default` - (:yes or :no) if `:yes` pressing Enter
      accepts the prompt; if `:no` pressing Enter rejects
      the prompt instead. Defaults to `:yes`.

  """
  defdelegate yes?(message, options \\ []), to: Mix.Shell.IO

  @doc """
  Executes the given command quietly without outputting anything.
  """
  def cmd(command, opts \\ []) do
    Mix.Shell.cmd(command, opts, fn data -> data end)
  end
end
