defmodule Mix.Tasks.Local do
  use Mix.Task

  @shortdoc "Lists local tasks"

  @moduledoc """
  Lists local tasks.
  """

  @impl true
  def run([]) do
    shell = Mix.shell()
    modules = Mix.Local.archives_tasks()

    docs =
      for module <- modules do
        {Mix.Task.task_name(module), Mix.Task.shortdoc(module)}
      end

    max =
      Enum.reduce(docs, 0, fn {task, _}, acc ->
        max(byte_size(task), acc)
      end)

    sorted = Enum.sort(docs)

    Enum.each(sorted, fn {task, doc} ->
      shell.info(format('mix ~-#{max}s # ~ts', [task, doc]))
    end)
  end

  defp format(expression, args) do
    :io_lib.format(expression, args) |> IO.iodata_to_binary()
  end
end
