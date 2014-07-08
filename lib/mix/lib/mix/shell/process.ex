defmodule Mix.Shell.Process do
  @moduledoc """
  This is a Mix shell that uses the current process mailbox
  for communication instead of IO.

  When a developer calls `info("hello")`, the following
  message will be sent to the current process:

      {:mix_shell, :info, ["hello"]}

  This is mainly useful in tests, allowing us to assert
  if given messages were received or not. Since we need
  to guarantee a clean slate between tests, there
  is also a `flush/1` function responsible for flushing all
  `:mix_shell` related messages from the process inbox.
  """

  @behaviour Mix.Shell

  @doc """
  Flush all `:mix_shell` and `:mix_shell_input` messages from the current process.
  If a callback is given, it is invoked for each received message.

  ## Examples

      flush &IO.inspect(&1)

  """
  def flush(callback \\ fn(x) -> x end) do
    receive do
      {:mix_shell, _, _} = message ->
        callback.(message)
        flush(callback)
      {:mix_shell_input, _, _} = message ->
        callback.(message)
        flush(callback)
    after
      0 -> :done
    end
  end

  @doc """
  Prints the currently running application if it
  was not printed yet.
  """
  def print_app do
    if Mix.Shell.print_app? do
      send self, {:mix_shell, :info, ["==> #{Mix.Project.config[:app]}"]}
    end
  end

  @doc """
  Executes the given command and forwards its messages to
  the current process.
  """
  def cmd(command) do
    Mix.Shell.cmd(command, fn(data) ->
      send self, {:mix_shell, :run, [data]}
    end)
  end

  @doc """
  Forwards the message to the current process.

  ## Options

    * `:ansi` - If `true`, it means ANSI sequences exist in the message
      The escape sequences are then removed by this function to aid
      debugging/testing
  """
  def info(message, opts \\ []) do
    print_app

    if opts[:ansi] do
      message = IO.ANSI.escape(message, false)
    end

    send self, {:mix_shell, :info, [message]}
  end

  @doc """
  Forwards the message to the current process.
  """
  def error(message) do
    print_app
    send self, {:mix_shell, :error, [message]}
  end

  @doc """
  Forwards the message to the current process.
  It also checks the inbox for an input message matching:

      {:mix_shell_input, :prompt, value}

  If one does not exist, it will abort since there was no shell
  process inputs given. Value must be a string.
  """
  def prompt(message) do
    print_app
    send self, {:mix_shell, :prompt, [message]}

    receive do
      {:mix_shell_input, :prompt, response} -> response
    after
      0 -> raise "No shell process input given for prompt/1"
    end
  end

  @doc """
  Forwards the message to the current process.
  It also checks the inbox for an input message matching:

      {:mix_shell_input, :yes?, value}

  If one does not exist, it will abort since there was no shell
  process inputs given. Value must be `true` or `false`.
  """
  def yes?(message) do
    print_app
    send self, {:mix_shell, :yes?, [message]}

    receive do
      {:mix_shell_input, :yes?, response} -> response
    after
      0 -> raise "No shell process input given for yes?/1"
    end
  end
end
