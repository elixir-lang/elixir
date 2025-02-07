# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Mix.Tasks.Local do
  use Mix.Task

  @shortdoc "Lists tasks installed locally via archives"

  @moduledoc """
  Lists tasks installed locally via archives.
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
      shell.info(format(~c"mix ~-#{max}s # ~ts", [task, doc]))
    end)
  end

  defp format(expression, args) do
    :io_lib.format(expression, args) |> IO.iodata_to_binary()
  end
end
