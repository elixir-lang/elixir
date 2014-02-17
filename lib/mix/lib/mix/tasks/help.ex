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

    docs = for module <- modules,
        doc = Mix.Task.shortdoc(module) do
      { "mix " <> Mix.Task.task_name(module), doc }
    end

    max = Enum.reduce docs, 0, fn({ task, _ }, acc) ->
      max(size(task), acc)
    end

    display_default_task_doc(max)

    Enum.each Enum.sort(docs), fn({ task, doc }) ->
      shell.info format_task(task, max, doc)
    end

    display_iex_task_doc(max)
  end

  def run([task]) do
    module = Mix.Task.get!(task)
    shell  = Mix.shell
    shell.info "%{bright}# mix help #{task}\n"

    if doc = Mix.Task.moduledoc(module) do
      shell.info doc
    else
      shell.info "There is no documentation for this task"
    end

    shell.info "Location: #{where_is_file(module)}"
  end

  defp format_task(task, max, doc) do
    String.ljust(task, max) <> " # " <> doc
  end

  defp where_is_file(module) do
    case :code.where_is_file(atom_to_list(module) ++ '.beam') do
      :non_existing -> "not available"
      location -> Path.expand(Path.dirname(location))
    end
  end

  defp display_default_task_doc(max) do
    Mix.shell.info format_task("mix", max,
                    "Run the default task (current: mix #{Mix.project[:default_task]})")
  end

  defp display_iex_task_doc(max) do
    Mix.shell.info format_task("iex -S mix", max,
                    "Start IEx and run the default task")
  end
end
