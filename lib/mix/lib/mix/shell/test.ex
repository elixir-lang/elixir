defmodule Mix.Shell.Test do
  @moduledoc """
  This is Mix's test shell.
  It simply sends the received calls to
  the current process queue, so they can be read later.
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
        flush
    after
      0 -> :done
    end
  end

  def info(message) do
    self <- { :mix_shell, :info, [message] }
  end

  def error(message) do
    self <- { :mix_shell, :error, [message] }
  end
end