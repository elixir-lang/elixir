defmodule Mix.Tasks.Local do
  use Mix.Task

  @shortdoc "List local tasks"

  @moduledoc """
  List local tasks.
  """

  def run([]) do
    shell   = Mix.shell
    modules = Mix.Local.all_tasks

    docs = lc module inlist modules do
      { Mix.Task.task_name(module), Mix.Task.shortdoc(module) }
    end

    max = Enum.reduce docs, 0, fn({ task, _ }, acc) ->
      max(size(task), acc)
    end

    sorted = Enum.sort(docs)

    Enum.each sorted, fn({ task, doc }) ->
      shell.info format('mix ~-#{max}s # ~s', [task, doc])
    end
  end

  defp format(expression, args) do
    :io_lib.format(expression, args) |> iolist_to_binary
  end
end
