defmodule Mix.Tasks.Do do
  use Mix.Task

  @shortdoc "Executes the tasks separated by comma"

  @moduledoc """
  Executes the tasks separated by comma.

  The comma should be followed by a space.

  ## Examples

  The example below prints the available compilers and
  then the list of dependencies.

      mix do compile --list, deps

  """

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    Enum.each gather_commands(args), fn
      [task | args] -> Mix.Task.run task, args
    end
  end

  @doc false
  def gather_commands(args) do
    gather_commands args, [], []
  end

  def gather_commands([head | rest], current, acc)
      when binary_part(head, byte_size(head), -1) == "," do
    part    = binary_part(head, 0, byte_size(head) - 1)
    current = Enum.reverse([part | current])
    gather_commands rest, [], [current | acc]
  end

  def gather_commands([head | rest], current, acc) do
    gather_commands rest, [head | current], acc
  end

  def gather_commands([], current, acc) do
    Enum.reverse [Enum.reverse(current) | acc]
  end
end
