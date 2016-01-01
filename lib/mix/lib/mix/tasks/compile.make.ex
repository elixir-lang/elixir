defmodule Mix.Tasks.Compile.Make do
  use Mix.Task

  @shortdoc "Runs `make` in the current project"

  @moduledoc """
  Runs `make` in the current project.

  This task runs `make` in the current project; any output coming from `make` is
  printed in real-time on stdout. `make` will be called without specifying a
  Makefile, so there has to be a `Makefile` in the current working directory.

  ## Configuration

    * `:make_targets` - it's a list of binaries. It's the list of Make targets
      that should be run. Defaults to `[]`, meaning `make` will run the first
      target.

    * `:make_cwd` - it's a binary. It's the directory where `make` will be run,
      relative to the root of the project.

  """

  @spec run([binary]) :: :ok | no_return
  def run(_args) do
    config  = Mix.Project.config()
    targets = Keyword.get(config, :make_targets, [])
    cwd     = Keyword.get(config, :make_cwd, ".")

    exit_status = File.cd!(cwd, fn -> cmd("make", targets) end)

    if exit_status == 0 do
      :ok
    else
      Mix.raise "`make` exited with a non-zero status (#{exit_status})"
    end
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
      Mix.raise "`#{exec}` not found in the current path. It was needed by the"
                <> " :make compiler."
    end
  end
end
