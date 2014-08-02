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
    if Mix.Shell.print_app? do
      IO.puts "==> #{Mix.Project.config[:app]}"
    end
  end

  @doc """
  Executes the given command and prints its output
  to stdout as it comes.
  """
  def cmd(command) do
    Mix.Shell.cmd(command, &IO.write(&1))
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
    IO.puts :stderr, red(IO.ANSI.format(message))
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
  Receives a message and asks the user if he wants to proceed.
  He must press enter or type anything that matches the a "yes"
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

  @doc """
  The same as `yes/1`, ask the user with prompt.
  The difference is that the dafult option is "No".
  """
  def no?(message) do
    print_app
    got_no? IO.gets(message <> " [yN] ")
  end

  defp got_no?(answer) when is_binary(answer) do
    answer =~ ~r/^(No?)?$/i
  end

  defp got_no?(_), do: true

  defp red(message) do
    [IO.ANSI.red, IO.ANSI.bright, message, IO.ANSI.reset]
  end
end
