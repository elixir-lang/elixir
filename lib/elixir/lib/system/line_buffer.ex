defmodule System.LineBuffer do
  @moduledoc false
  defstruct current_line: "", lines: []

  def new() do
    %__MODULE__{}
  end
end

defimpl Collectable, for: System.LineBuffer do
  def into(line_buffer) do
    collector_fun = fn
      acc, {:cont, {:noeol, elem}} ->
        %{acc | current_line: acc.current_line <> elem}

      acc, {:cont, {:eol, elem}} ->
        current_line = acc.current_line <> elem
        %{acc | current_line: "", lines: [current_line | acc.lines]}

      acc, :done ->
        Enum.reverse(acc.lines)

      _acc, :halt ->
        :ok
    end

    initial_acc = line_buffer

    {initial_acc, collector_fun}
  end
end
