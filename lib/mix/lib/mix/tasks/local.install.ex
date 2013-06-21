defmodule Mix.Tasks.Local.Install do
  use Mix.Task

  import Mix.Generator, only: [create_file: 2]

  @shortdoc "Install a task or an archive locally"

  @moduledoc """
  Install a task or an archive locally.

  If no argument is supplied but there is an archive in the root
  (created with mix archive), then the archive will be installed
  locally. For example:
  
      mix do archive, local.install

  The task can also be a single local beam file or a beam
  file located at some URL.

      mix local.install http://example.com/some_task.beam

  After installed, the task can be invoked locally:

      mix some_task

  ## Command line options

  * `--force` forces installation without a shell prompt. Primarily
    intended for automation in build systems like Make.

  """

  def run(argv) do
    { opts, argv } = OptionParser.parse(argv, switches: [force: :boolean])

    unless path = Enum.first(argv) do
      path = "#{Mix.project[:app]}.ez"
      unless File.exists?(path) do
        raise Mix.Error, message: "expected PATH to be given, please use `mix local.install PATH`"
      end
    end

    case Path.extname(path) do
      ".ez"   -> install_archive(path, opts)
      ".beam" -> install_beam(path, opts)
      _ ->
        raise Mix.Error, message: "mix local.install doesn't know how to install #{path}"
    end
  end

  defp install_beam(path, opts) do
    beam = Mix.Utils.read_path(path)
    { :module, module } = get_module(path, beam)

    validate_module_name!(path, module)
    task_name = Mix.Task.task_name(module)

    if opts[:force] || Mix.shell.yes?("Are you sure you want to install task #{inspect task_name, pretty: false}?") do
      tasks = Mix.Local.tasks_path
      File.mkdir_p! tasks
      create_file Path.join(tasks, "#{module}.beam"), beam
    end
  end

  defp install_archive(name, opts) do
    if opts[:force] || Mix.shell.yes?("Are you sure you want to install archive #{name}?") do
      tasks = Mix.Local.tasks_path
      File.mkdir_p! tasks
      File.copy(name, Path.join(tasks, name))
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
    case atom_to_binary(module) do
      "Elixir.Mix.Tasks." <> _ ->
        :ok
      other ->
        raise Mix.Error, message: "expected a Mix.Tasks module at #{path}, got #{other}"
    end
  end
end
