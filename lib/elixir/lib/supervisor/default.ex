defmodule Supervisor.Default do
  @moduledoc false

  def init(args) do
    args
  end

  def handle_child_restart(_old_pid, _new_pid), do: :ok
end
