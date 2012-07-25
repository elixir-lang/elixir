defmodule Mix.Tasks.Local do
  use Mix.Task

  @shortdoc "List local tasks"

  @moduledoc """
  List local tasks.
  """

  def run([]) do
    shell   = Mix.shell
    modules = get_locals

    docs = lc module inlist modules do
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

  defp get_locals do
    query   = File.join(Mix.Utils.home, ".mix/tasks/Elixir-Mix-Tasks-*.beam")
    files   = File.wildcard(query)
    modules = Enum.map files, &1 /> File.basename /> File.rootname(".beam") /> binary_to_atom
    Enum.filter(modules, fn(mod) ->
      match? { :module, _ }, Code.ensure_loaded(mod)
    end)
  end

  defp format(expression, args) do
    :io_lib.format(expression, args) /> iolist_to_binary
  end
end