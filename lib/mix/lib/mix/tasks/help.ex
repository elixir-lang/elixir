defmodule Mix.Tasks.Help do
  use Mix.Task

  @shortdoc "Print help information for tasks"

  @moduledoc """
  If given a task name, prints the documentation for that task.
  If no task name is given, prints the short form documentation
  for all tasks.

  ## Arguments

      mix help      - prints all tasks and their shortdoc
      mix help TASK - prints full docs for the given task

  """

  def run([]) do
    Mix.Task.load_all

    shell   = Mix.shell
    modules = Mix.Task.all_modules

    docs = lc module inlist modules,
        not Mix.Task.hidden?(module),
        doc = Mix.Task.shortdoc(module) do
      { Mix.Task.task_name(module), doc }
    end

    max = Enum.reduce docs, 0, fn({ task, _ }, acc) ->
      max(size(task), acc)
    end

    sorted = Enum.sort(docs)

    Enum.each sorted, fn({ task, doc }) ->
      shell.info format(IO.ANSI.escape("mix ~-#{max}s #[faint, black]# ~s"), [task, doc])
    end
  end

  def run([task]) do
    module = Mix.Task.get(task)
    IO.puts IO.ANSI.escape "#[faint, black]# mix help #{task}\n"

    if doc = Mix.Task.moduledoc(module) do
      IO.write doc
    else
      IO.puts IO.ANSI.escape "#[faint, black]There is no documentation for this task"
    end

    IO.puts IO.ANSI.escape "#[faint, black]Location: #{where_is_file(module)}"
  end

  defp where_is_file(module) do
    case :code.where_is_file(atom_to_list(module) ++ '.beam') do
      :non_existing -> "not available"
      location -> Path.expand(Path.dirname(location))
    end
  end

  defp format(expression, args) do
    :io_lib.format(expression, args) |> iolist_to_binary
  end
end
