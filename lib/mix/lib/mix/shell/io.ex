defmodule Mix.Shell.IO do
  @moduledoc """
  This is Mix's default shell.
  It simply prints messages to stdio and stderr.
  """

  @behaviour Mix.Shell

  @doc """
  Executes the given command and prints its output
  to stdout as it comes.
  """
  def cmd(command) do
    Mix.Shell.cmd(command, IO.write(&1))
  end

  @doc """
  Writes a message to the shell followed by new line.
  """
  def info(message) do
    IO.puts IO.ANSI.escape(message)
  end

  @doc """
  Writes an error message to the shell followed by new line.
  """
  def error(message) do
    IO.puts :stderr, IO.ANSI.escape "%{red,bright}#{message}"
  end

  @doc """
  Receives a message and asks the user if he wants to proceed.
  He must press enter or type anything that matches the a "yes"
  regex `%r/^Y(es)?$/i`.
  """
  def yes?(message) do
    IO.gets(message <> IO.ANSI.escape(" [Yn] ")) =~ %r/^(Y(es)?)?$/i
  end
end