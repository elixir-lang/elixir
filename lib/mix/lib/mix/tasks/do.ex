defmodule Mix.Tasks.Do do
  use Mix.Task

  @shortdoc "Executes the tasks separated by comma"

  @moduledoc """
  Executes the tasks separated by comma.

  ## Examples

  The example below prints the available compilers and
  then the list of dependencies.

      mix do compile --list, deps

  """
  def run(args) do
    Enum.each gather_commands(args), fn
      [task|args] -> Mix.Task.run task, args
      [] -> raise Mix.Error, message: "No expression between commas"
    end
  end

  defp gather_commands(args) do
    gather_commands args, [], []
  end

  defp gather_commands([h|t], current, acc) when binary_part(h, byte_size(h), -1) == "," do
    part    = binary_part(h, 0, byte_size(h) - 1)
    current = Enum.reverse([part|current])
    gather_commands t, [], [current|acc]
  end

  defp gather_commands([h|t], current, acc) do
    gather_commands t, [h|current], acc
  end

  defp gather_commands([], current, acc) do
    Enum.reverse [Enum.reverse(current)|acc]
  end
end