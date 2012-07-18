defmodule Mix do
  @doc """
  Starts the mix application and its dependencies.
  """
  def start do
    Enum.each [:elixir, :mix], :application.start(&1)
    :application.set_env(:mix, :shell, Mix.Shell)
    Mix.Task.start
    Mix.Project.start
  end

  @doc """
  The shell is a wrapper for doing IO.

  It contains conveniences for asking the user information,
  printing status and so forth. The fact it is also swappable
  allow developers to use a test shell, that simply sends the
  messages to the current process.
  """
  def shell do
    case :application.get_env(:mix, :shell) do
      { :ok, shell } -> shell
      _ -> raise "No shell was set, are you sure you invoked Mix.start() ?"
    end
  end

  @doc """
  Sets the current shell.
  """
  def shell(shell) do
    :application.set_env(:mix, :shell, shell)
  end
end