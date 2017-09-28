defmodule Mix.Shell.IO do
  @moduledoc """
  This is Mix's default shell.

  It simply prints messages to stdio and stderr.
  """

  @behaviour Mix.Shell

  @doc """
  Prints the current application to the shell if it
  was not printed yet.
  """
  def print_app do
    if name = Mix.Shell.printable_app_name do
      IO.puts "==> #{name}"
    end
  end

  @doc """
  Returns a collectable to be used along side System.cmd.
  """
  def into do
    fun = fn
      :ok, {:cont, data} ->
        print_app()
        IO.write(data)
      :ok, _ ->
        :ok
    end
    {:ok, fun}
  end

  @doc false
  # TODO: Deprecate on Elixir v1.8
  def cmd(command, opts \\ []) do
    print_app? = Keyword.get(opts, :print_app, true)
    Mix.Shell.cmd(command, opts, fn data ->
      if print_app?, do: print_app()
      IO.write(data)
    end)
  end

  @doc """
  Prints the given message to the shell followed by a newline.
  """
  def info(message) do
    print_app()
    IO.puts IO.ANSI.format message
  end

  @doc """
  Prints the given error to the shell followed by a newline.
  """
  def error(message) do
    print_app()
    IO.puts :stderr, IO.ANSI.format(red(message))
  end

  @doc """
  Prints a message and prompts the user for input.

  Input will be consumed until Enter is pressed.
  """
  def prompt(message) do
    print_app()
    IO.gets(message <> " ")
  end

  @doc """
  Prints a message and asks the user if they want to proceed.

  The user must press Enter or type one of "y", "yes", "Y", "YES" or
  "Yes".
  """
  def yes?(message) do
    print_app()
    answer = IO.gets(message <> " [Yn] ")
    is_binary(answer) and String.trim(answer) in ["", "y", "Y", "yes", "YES", "Yes"]
  end

  defp red(message) do
    [:red, :bright, message]
  end
end
