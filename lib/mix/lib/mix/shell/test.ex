defmodule Mix.Shell.Test do
  @moduledoc """
  This is Mix's test shell.
  It simply sends the received calls to
  the current process queue, so they can be read later.
  """

  @behavior Mix.Shell

  def info(message) do
    self <- { :mix_shell, :info, [message] }
  end

  def error(message) do
    self <- { :mix_shell, :error, [message] }
  end
end