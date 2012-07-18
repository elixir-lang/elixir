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
  Define Mix.Shell callbacks.
  """
  def behaviour_info(:callbacks) do
    [info: 1, error: 1]
  end
end