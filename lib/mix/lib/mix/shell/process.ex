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
    if name = Mix.Shell.printable_app_name do
      send self, {:mix_shell, :info, ["==> #{name}"]}
    end
  end

  @doc """
  Executes the given command and forwards its messages to
  the current process.
  """
  def cmd(command, opts \\ []) do
    print_app? = Keyword.get(opts, :print_app, true)
    Mix.Shell.cmd(command, opts, fn(data) ->
      if print_app?, do: print_app()
      send self, {:mix_shell, :run, [data]}
    end)
  end

  @doc """
  Forwards the message to the current process.
  """
  def info(message) do
    print_app
    send self, {:mix_shell, :info, [format(message)]}
  end

  @doc """
  Forwards the message to the current process.
  """
  def error(message) do
    print_app
    send self, {:mix_shell, :error, [format(message)]}
  end

  defp format(message) do
    message |> IO.ANSI.format(false) |> IO.iodata_to_binary
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
