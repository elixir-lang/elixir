defmodule Mix.Tasks.Local.Install do
  use Mix.Task

  import Mix.Generator, only: [create_file: 2]

  @shortdoc "Install a task locally"

  @moduledoc """
  Install a task locally.

  The task can be either a local beam file or a beam
  file located at some URL.

      mix local.install http://example.com/some_task.beam

  After installed, the task can be invoked locally:

      mix some_task

  """

  def run(argv) do
    { _, argv } = OptionParser.parse(argv)
    case argv do
      [] ->
        raise Mix.Error, message: "expected PATH to be given, please use `mix local.install PATH`"
      [path|_] ->
        do_install path
    end
  end

  defp do_install(path) do
    beam = Mix.Utils.read_path(path)
    { :module, module } = get_module(path, beam)

    validate_module_name!(path, module)
    task_name = Mix.Task.task_name(module)

    if Mix.shell.yes?("Are you sure you want to install task #{inspect task_name}?") do
      tasks = Mix.Local.tasks_path
      File.mkdir_p! tasks
      create_file Path.join(tasks, "#{module}.beam"), beam
    end
  end

  defp get_module(path, beam) do
    case :beam_lib.info(beam) do
      list when is_list(list) ->
        List.keyfind list, :module, 0
      _ ->
        raise Mix.Error, message: "could not parse beam file at #{path}"
    end
  end

  defp validate_module_name!(path, module) do
    case inspect(module) do
      "Mix.Tasks." <> _ ->
        :ok
      other ->
        raise Mix.Error, message: "expected a Mix.Tasks module at #{path}, got #{other}"
    end
  end

end