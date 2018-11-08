defmodule Mix.Tasks.Do do
  use Mix.Task

  @shortdoc "Executes the tasks separated by comma"

  @moduledoc """
  Executes the tasks separated by comma.

  The comma should be followed by a space.

  This task is automatically reenabled, so it can be called multiple times.

  ## Examples

  The example below prints the available compilers and
  then the list of dependencies.

      mix do compile --list, deps

  """

  @impl true
  def run(args) do
    Mix.Task.reenable("do")
    Enum.each(gather_commands(args), fn [task | args] -> Mix.Task.run(task, args) end)
  end

  @doc false
  def gather_commands(args) do
    gather_commands(args, [], [])
  end

  defp gather_commands([head | rest], current, acc)
       when binary_part(head, byte_size(head), -1) == "," do
    current =
      case binary_part(head, 0, byte_size(head) - 1) do
        "" -> Enum.reverse(current)
        part -> Enum.reverse([part | current])
      end

    gather_commands(rest, [], [current | acc])
  end

  defp gather_commands([head | rest], current, acc) do
    gather_commands(rest, [head | current], acc)
  end

  defp gather_commands([], current, acc) do
    Enum.reverse([Enum.reverse(current) | acc])
  end
end
