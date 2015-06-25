defmodule Mix.Shell.IO do
  @moduledoc """
  This is Mix's default shell.

  It simply prints messages to stdio and stderr.
  """

  @behaviour Mix.Shell

  @doc """
  Prints the currently running application if it
  was not printed yet.
  """
  def print_app do
    if name = Mix.Shell.printable_app_name do
      IO.puts "==> #{name}"
    end
  end

  @doc """
  Executes the given command and prints its output
  to stdout as it comes.
  """
  def cmd(command, opts \\ []) do
    print_app? = Keyword.get(opts, :print_app, true)
    Mix.Shell.cmd(command, opts, fn data ->
      if print_app?, do: print_app()
      IO.write(data)
    end)
  end

  @doc """
  Writes a message to the shell followed by new line.
  """
  def info(message) do
    print_app
    IO.puts IO.ANSI.format message
  end

  @doc """
  Writes an error message to the shell followed by new line.
  """
  def error(message) do
    print_app
    IO.puts :stderr, IO.ANSI.format(red(message))
  end

  @doc """
  Writes a message shell followed by prompting the user for
  input. Input will be consumed until enter is pressed.
  """
  def prompt(message) do
    print_app
    IO.gets message <> " "
  end

  @doc """
  Receives a message and asks the user if they want to proceed.
  The user must press enter or type anything that matches the "yes"
  regex `~r/^Y(es)?$/i`.
  """
  def yes?(message) do
    print_app
    got_yes? IO.gets(message <> " [Yn] ")
  end

  defp got_yes?(answer) when is_binary(answer) do
    answer =~ ~r/^(Y(es)?)?$/i
  end

  # The io server may return :eof or :error
  defp got_yes?(_), do: false

  defp red(message) do
    [:red, :bright, message]
  end
end
