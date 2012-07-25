defmodule Mix.Shell do
  @moduledoc """
  This is Mix's default shell.
  It simply prints messages to stdio and stderr.
  """

  @doc """
  Writes a message to the shell followed by new line.
  """
  def info(message) do
    IO.puts message
  end

  @doc """
  Writes an error message to the shell followed by new line.
  """
  def error(message) do
    IO.puts :stderr, message
  end

  @doc """
  Receives a message and asks the user if he wants to proceed.
  He must press enter or type anything that matches the a "yes"
  regex `%r/^Y(es)?$/i`.
  """
  def yes?(message) do
    IO.gets(message <> " [Yn] ") =~ %r/^(Y(es)?)?$/i
  end

  @doc """
  Define Mix.Shell callbacks.
  """
  def behaviour_info(:callbacks) do
    [info: 1, error: 1, yes?: 1]
  end
end