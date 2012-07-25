defmodule Mix.Shell.Process do
  @moduledoc """
  This is Mix shell that uses the current process mailbox
  for communication instead of IO.

  When a developer calls `info("hello")`, the following
  message will be sent to the current process:

      { :mix_shell, :info, ["hello"] }

  This is mainly useful in tests, allowing us to assert
  if given messages were received or not. Since we need
  to guarantee a clean slate in between tests, there
  is also a flush function responsible for flushing all
  `:mix_shell` related tasks from the process inbox.
  """

  @behavior Mix.Shell

  @doc """
  Flush all :mix_shell messages from the current process.
  If a callback is given, it is invoked for each received message.

  ## Examples

      flush IO.inspect(&1)

  """
  def flush(callback // fn(x) -> x end) do
    receive do
      { :mix_shell, _, _ } = message ->
        callback.(message)
        flush(callback)
    after
      0 -> :done
    end
  end

  @doc """
  Simply forwards the message to the current process.
  """
  def info(message) do
    self <- { :mix_shell, :info, [message] }
  end

  @doc """
  Simply forwards the message to the current process.
  """
  def error(message) do
    self <- { :mix_shell, :error, [message] }
  end

  @doc """
  Simply forwards the message to the current process.
  It also checks the inbox for an input message matching:

      { :mix_shell_input, :yes?, value }

  If one does not exist, it will abort since there no shell
  process input given. Value must be true or false.
  """
  def yes?(message) do
    self <- { :mix_shell, :yes?, [message] }

    receive do
      { :mix_shell_input, :yes?, response } -> response
    after
      0 -> raise Mix.Error, message: "No shell process input given"
    end
  end
end