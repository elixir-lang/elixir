defmodule Mix.Tasks.Compile.Make do
  use Mix.Task

  @shortdoc "Runs `make` in the current project"

  @moduledoc """
  Runs `make` in the current project.

  This task runs `make` in the current project; any output coming from `make` is
  printed in real-time on stdout. `make` will be called without specifying a
  Makefile, so there has to be a `Makefile` in the current working directory.

  ## Configuration

    * `:make_makefile` - it's a binary. It's the Makefile to use. Defaults to
      `"Makefile"`.

    * `:make_targets` - it's a list of binaries. It's the list of Make targets
      that should be run. Defaults to `[]`, meaning `make` will run the first
      target.

    * `:make_cwd` - it's a binary. It's the directory where `make` will be run,
      relative to the root of the project.

  """

  @spec run([binary]) :: :ok | no_return
  def run(_args) do
    config = Mix.Project.config()
    build(config)
    Mix.Project.build_structure
    :ok
  end

  defp build(config) do
    makefile = Keyword.get(config, :make_makefile)
    targets  = Keyword.get(config, :make_targets, [])
    cwd      = Keyword.get(config, :make_cwd, ".")

    exit_status = File.cd! cwd, fn ->
      targets = if makefile, do: ["-f", makefile] ++ targets, else: targets
      cmd("make", targets)
    end

    if exit_status == 0, do: :ok, else: build_error("make", exit_status)
  end

  defp cmd(exec, args) do
    opts = [into: IO.stream(:stdio, :line), stderr_to_stdout: true]
    {%IO.Stream{}, status} = System.cmd(executable(exec), args, opts)
    status
  end

  defp executable(exec) do
    if found = System.find_executable(exec) do
      found
    else
      executable_not_found_error(exec)
    end
  end

  defp executable_not_found_error(exec) do
    Mix.raise """
    `#{exec}` not found in the current path.
    """
  end

  defp build_error(exec, exit_status) do
    Mix.raise """
    Could not compile with `#{exec}`.
    """
  end
end
