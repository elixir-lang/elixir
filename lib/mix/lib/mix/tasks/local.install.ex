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

  The argument can be an archive located at some URL:

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
      path = Mix.Archive.name(Mix.project[:app], Mix.project[:version])

      unless File.exists?(path) do
        raise Mix.Error, message: "Expected PATH to be given, please use `mix local.install PATH`"
      end
    end

    case Path.extname(path) do
      ".ez" -> install_archive(path, opts)
      _     -> raise Mix.Error, message: "mix local.install doesn't know how to install #{path}"
    end
  end

  defp install_archive(src, opts) do
    if should_install?(src, opts[:force]) do
      dest = Mix.Local.archives_path
      File.mkdir_p! dest
      create_file Path.join(dest, Path.basename(src)), Mix.Utils.read_path!(src)
    end
  end

  defp should_install?(src, force) do
    if file = already_installed(src) do
      if force || Mix.shell.yes?("Found existing archive #{Path.basename(file)}.\n" <>
          "Do you want to override this archive?") do
        File.rm(file)
        true
      end
    else
      force || Mix.shell.yes?("Are you sure you want to install archive #{src}?")
    end
  end

  defp already_installed(src) do
    app = Mix.Archive.dir(src) |> String.split("-") |> Enum.first
    Path.join(Mix.Local.archives_path, app <> "-*.ez") |> Path.wildcard |> Enum.first
  end
end
