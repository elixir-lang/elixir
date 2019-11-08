defmodule PID do
  defmacro __sigil__({:<<>>, _, [string]}, _) do
    :erlang.list_to_pid('<#{string}>')
  end
end
