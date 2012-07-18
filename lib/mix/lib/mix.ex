defmodule Mix do
  @doc """
  Starts the mix application and its dependencies.
  """
  def start do
    Enum.each [:elixir, :mix], :application.start(&1)
    Mix.Server.start_link
  end

  @doc """
  The shell is a wrapper for doing IO.

  It contains conveniences for asking the user information,
  printing status and so forth. The fact it is also swappable
  allow developers to use a test shell, that simply sends the
  messages to the current process.
  """
  def shell do
    Mix.Server.call(:shell)
  end

  @doc """
  Sets the current shell.
  """
  def shell(shell) do
    Mix.Server.cast({ :shell, shell })
  end
end