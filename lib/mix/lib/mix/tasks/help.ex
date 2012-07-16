defmodule Mix.Tasks.Help do
  use Mix.Task

  @shortdoc "Prints help information for tasks"

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
    modules = Mix.Task.all_modules

    docs = lc module inlist modules, doc = Mix.Task.shortdoc(module) do
      { Mix.Task.task_name(module), doc }
    end

    max = Enum.reduce docs, 0, fn({ task, _ }, acc) ->
      max(size(task), acc)
    end

    sorted = Enum.qsort(docs)

    Enum.each sorted, fn({ task, doc }) ->
      :io.format('mix ~-#{max}s # ~s~n', [task, doc])
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
  end
end
