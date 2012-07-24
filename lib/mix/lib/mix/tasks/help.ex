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

    docs = lc module inlist modules, not Mix.Task.hidden?(module) do
      { Mix.Task.task_name(module), Mix.Task.shortdoc(module) }
    end

    max = Enum.reduce docs, 0, fn({ task, _ }, acc) ->
      max(size(task), acc)
    end

    sorted = Enum.qsort(docs)

    Enum.each sorted, fn({ task, doc }) ->
      shell.info format('mix ~-#{max}s # ~s', [task, doc])
    end
  end

  def run([task]) do
    module = Mix.Task.get(task)
    IO.puts "# mix help #{task}\n"

    if doc = Mix.Task.moduledoc(module) do
      IO.write doc
    else
      IO.puts "There is no documentation for this task"
    end

    IO.puts "Source: #{Mix.Utils.source(module)}"
  end

  defp format(expression, args) do
    :io_lib.format(expression, args) /> iolist_to_binary
  end
end
