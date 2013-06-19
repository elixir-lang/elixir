defmodule Mix.Tasks.Local.Install do
  use Mix.Task

  import Mix.Generator, only: [create_file: 2]

  @shortdoc "Install a task or an archive locally"

  @moduledoc """
  Install an archive locally.

  If no argument is supplied but there is an archive in the root
  (created with mix archive), then the archive will be installed
  locally. For example:
  
      mix do archive, local.install

  The task can also be an archive located at some URL:

      mix local.install http://example.com/foo.ez

  After installed, the tasks in the archive are available locally:

      mix some_task

  ## Command line options

  * `--force` forces installation without a shell prompt. Primarily
    intended for automation in build systems like make.

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
      ".ez" -> install_archive(path, opts)
      _     -> raise Mix.Error, message: "mix local.install doesn't know how to install #{path}"
    end
  end

  defp install_archive(src, opts) do
    if opts[:force] || Mix.shell.yes?("Are you sure you want to install archive #{src}?") do
      dest = Mix.Local.archives_path
      File.mkdir_p! dest
      create_file Path.join(dest, Path.basename(src)), Mix.Utils.read_path(src)
    end
  end
end
