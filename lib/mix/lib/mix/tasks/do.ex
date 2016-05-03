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
    {current, commands} =
      case String.split(arg, ",") do
        [arg] -> {[arg | current], commands}
        # special care if the argument contains a comma
        args  -> update_commands(args, current, commands)
      end
    gather_commands(rest, current, commands)
  end

  defp update_commands([], current, commands) do
    {current, commands}
  end
  defp update_commands([arg], current, commands) when arg != "" do
    {[arg], [current | commands]}
  end
  defp update_commands([arg | args], current, commands) do
    # if the argument is empty, we had a leading or trailing comma
    # so we simply terminate the current command
    command = if arg == "", do: current, else: [arg | current]
    update_commands(args, [], [command | commands])
  end
end
