defmodule Mix.Shell.Process do
  @moduledoc """
  This is a Mix shell that uses the current process mailbox
  for communication instead of IO.

  When a developer calls `info("hello")`, the following
  message will be sent to the current process:

      { :mix_shell, :info, ["hello"] }

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
      { :mix_shell, _, _ } = message ->
        callback.(message)
        flush(callback)
      { :mix_shell_input, _, _ } = message ->
        callback.(message)
        flush(callback)
    after
      0 -> :done
    end
  end

  @doc """
  Executes the given command and fowards its messages to
  the current process.
  """
  def cmd(command) do
    put_app
    Mix.Shell.cmd(command, fn(data) ->
      send self, { :mix_shell, :run, [data] }
    end)
  end

  @doc """
  Forwards the message to the current process.
  """
  def info(message) do
    put_app
    send self, { :mix_shell, :info, [IO.ANSI.escape(message, false)] }
  end

  @doc """
  Forwards the message to the current process.
  """
  def error(message) do
    put_app
    send self, { :mix_shell, :error, [IO.ANSI.escape(message, false)] }
  end

  @doc """
  Forwards the message to the current process.
  It also checks the inbox for an input message matching:

      { :mix_shell_input, :prompt, value }

  If one does not exist, it will abort since there was no shell
  process inputs given. Value must be a string.
  """
  def prompt(message) do
    put_app
    send self, { :mix_shell, :prompt, [IO.ANSI.escape(message, false)] }

    receive do
      { :mix_shell_input, :prompt, response } -> response
    after
      0 -> raise Mix.Error, message: "No shell process input given for prompt/1"
    end
  end

  @doc """
  Forwards the message to the current process.
  It also checks the inbox for an input message matching:

      { :mix_shell_input, :yes?, value }

  If one does not exist, it will abort since there was no shell
  process inputs given. Value must be `true` or `false`.
  """
  def yes?(message) do
    put_app
    send self, { :mix_shell, :yes?, [IO.ANSI.escape(message, false)] }

    receive do
      { :mix_shell_input, :yes?, response } -> response
    after
      0 -> raise Mix.Error, message: "No shell process input given for yes?/1"
    end
  end

  defp put_app do
    if Mix.Shell.output_app? do
      send self, { :mix_shell, :info, ["==> #{Mix.project[:app]}"] }
    end
  end
end
