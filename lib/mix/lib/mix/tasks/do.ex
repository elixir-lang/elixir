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

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    Enum.each gather_commands(args), fn
      [task | args] -> Mix.Task.run task, args
    end
  end

  @doc false
  def gather_commands(args) do
    gather_commands(args, [], [])
  end

  defp gather_commands([], current, commands) do
    [current | commands]
    |> Enum.reject(&(&1 == []))
    |> Enum.map(&Enum.reverse(&1))
    |> Enum.reverse
  end

  defp gather_commands([arg | rest], current, commands) do
    case String.split(arg, ",", parts: 2) do
      [arg] ->
        gather_commands(rest, [arg | current], commands)
      [left, right] ->
        rest    = append_unless_empty(right, rest)
        current = append_unless_empty(left, current)
        gather_commands(rest, [], [current | commands])
    end
  end

  defp append_unless_empty("", list), do: list
  defp append_unless_empty(h, list),  do: [h | list]
end
