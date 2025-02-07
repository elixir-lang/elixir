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
    if name = Mix.Shell.printable_app_name() do
      IO.puts("==> #{name}")
    end

    :ok
  end

  @doc """
  Prints the given ANSI message to the shell followed by a newline.
  """
  def info(message) do
    print_app()
    IO.puts(IO.ANSI.format(message))
  end

  @doc """
  Prints the given ANSI error to the shell followed by a newline.
  """
  def error(message) do
    print_app()
    IO.puts(:stderr, IO.ANSI.format(red(message)))
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

  ## Examples

      if Mix.shell().yes?("Are you sure?") do
        # do something...
      end

  """
  def yes?(message, options \\ []) do
    default = Keyword.get(options, :default, :yes)

    if default not in [:yes, :no] do
      raise ArgumentError,
            "expected :default to be either :yes or :no, got: #{inspect(default)}"
    end

    answers = ["y", "Y", "yes", "YES", "Yes"]

    {prompt, accepted_answers} =
      case default do
        :yes -> {" [Yn] ", ["" | answers]}
        :no -> {" [yN] ", answers}
      end

    print_app()

    answer = IO.gets(message <> prompt)
    is_binary(answer) and String.trim(answer) in accepted_answers
  end

  defp red(message) do
    [:red, :bright, message]
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
end
