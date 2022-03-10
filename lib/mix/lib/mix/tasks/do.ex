defmodule Mix.Tasks.Do do
  use Mix.Task

  @shortdoc "Executes the tasks separated by comma"

  @moduledoc """
  Executes the tasks separated by comma:

      mix do compile --list, deps

  The comma should be followed by a space.

  ## Examples

  The example below prints the available compilers and
  then the list of dependencies.

      mix do compile --list, deps

  Note that the majority of Mix tasks are only executed once
  per invocation. So for example, the following command will
  only compile once:

      mix do compile, some_other_command, compile

  When `compile` is executed again, Mix will notice the task
  has already ran, and skip it.

  Inside umbrella projects, you can limit recursive tasks
  (the ones that run inside every app) by selecting the
  desired application via the `--app` flag after `do` and
  before the first task:

      mix do --app app1 --app app2 compile --list, deps

  On Windows in cmd and PowerShell the comma is a reserved
  special character. You can use a + in place of the comma
  instead:

      mix do compile --list + deps

  ## Command line options

    * `--app` - limit recursive tasks to the given apps.
      This option may be given multiple times and must come
      before any of the tasks.

  """

  @impl true
  def run(args) do
    Mix.Task.reenable("do")

    {apps, args} = extract_apps_from_args(args)
    show_forgotten_apps_warning(apps)

    Enum.each(gather_commands(args), fn [task | args] ->
      if apps == [] do
        Mix.Task.run(task, args)
      else
        Mix.Task.run_in_apps(task, apps, args)
      end
    end)
  end

  defp show_forgotten_apps_warning([]), do: nil

  defp show_forgotten_apps_warning(apps) do
    config = Mix.Project.config()

    if Mix.Project.umbrella?(config) do
      known_apps = Mix.Project.apps_paths(config)

      for app <- apps, not Map.has_key?(known_apps, app) do
        Mix.shell().info([:yellow, "warning: could not find application #{inspect(app)}"])
      end
    end
  end

  defp extract_apps_from_args(args) do
    {opts, args} = OptionParser.parse_head!(args, strict: [app: :keep])

    apps =
      opts
      |> Keyword.get_values(:app)
      |> Enum.map(&String.to_atom/1)

    {apps, args}
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

  defp gather_commands([head | rest], current, acc)
       when binary_part(head, byte_size(head), -2) == " +" do
    current =
      case binary_part(head, 0, byte_size(head) - 2) do
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
