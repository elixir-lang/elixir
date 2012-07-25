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
    { _, argv } = OptionParser.Simple.parse(argv)
    case argv do
      [] ->
        raise Mix.Error, message: "expected PATH to be given, please use `mix local.install PATH`"
      [path|_] ->
        do_install path
    end
  end

  defp do_install(path) do
    beam = open_path(path)
    { :module, module } = get_module(path, beam)

    validate_module_name!(path, module)
    task_name = Mix.Task.task_name(module)

    if Mix.shell.yes?("Are you sure you want to install task #{inspect task_name}?") do
      tasks = File.join Mix.Utils.home, ".mix/tasks"
      File.mkdir_p! tasks
      create_file File.join(tasks, "#{module}.beam"), beam
    end
  end

  defp open_path(path) do
    cond do
      is_url?(path)  -> open_url(path)
      is_file?(path) -> open_file(path)
      :else          -> raise Mix.Error, message: "expected PATH in `mix local.install PATH` to be a url or a local file path"
    end
  end

  defp get_module(path, beam) do
    case :beam_lib.info(beam) do
      list when is_list(list) ->
        List.keyfind list, :module, 1
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

  defp open_file(path) do
    File.read!(path)
  end

  defp open_url(path) do
    if URI.parse(path).scheme == "https" do
      :ssl.start
    end

    :inets.start

    case :httpc.request(binary_to_list(path)) do
      { :ok, { { _, status, _ }, _, body } } when status in 200..299 ->
        iolist_to_binary(body)
      { :ok, { { _, status, _ }, _, _ } } ->
        raise Mix.Error, message: "could not access url #{path}, got status: #{status}"
      { :error, reason } ->
        raise Mix.Error, message: "could not access url #{path}, error: #{inspect reason}"
    end
  end

  defp is_file?(path) do
    File.regular?(path)
  end

  defp is_url?(path) do
    URI.parse(path).scheme in ["http", "https"]
  end
end