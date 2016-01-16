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
      `"Makefile"` for Unix systems and `"Makefile.win"` for Windows systems.

    * `:make_targets` - it's a list of binaries. It's the list of Make targets
      that should be run. Defaults to `[]`, meaning `make` will run the first
      target.

    * `:make_cwd` - it's a binary. It's the directory where `make` will be run,
      relative to the root of the project.

    * `:make_error_message` - it's a binary. It's a custom error message that
      can be used to give instructions as of how to fix the error (e.g., it can
      be used to suggest installing `gcc` if you're compiling a C dependency).

  """

  @spec run([binary]) :: :ok | no_return
  def run(_args) do
    config = Mix.Project.config()
    build(config)
    Mix.Project.build_structure
    :ok
  end

  defp build(config) do
    makefile  = Keyword.get(config, :make_makefile, :default)
    targets   = Keyword.get(config, :make_targets, [])
    cwd       = Keyword.get(config, :make_cwd, ".")
    error_msg = Keyword.get(config, :make_error_message, "")
    exec      = executable_for_current_os()

    args = args_for_makefile(exec, makefile) ++ targets
    exit_status = cmd(exec, args, cwd)

    if exit_status == 0, do: :ok, else: build_error(exec, exit_status, error_msg)
  end

  # Runs `exec [args]` in `cwd` and prints the stdout and stderr in real time,
  # as soon as `exec` prints them (using `IO.Stream`).
  defp cmd(exec, args, cwd) do
    opts = [into: IO.stream(:stdio, :line),
            stderr_to_stdout: true,
            cd: cwd]

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

  defp build_error(exec, exit_status, error_msg) do
    msg = """
    Could not compile with `#{exec}`.
    """
    Mix.raise(Enum.join([msg, error_msg], "\n"))
  end

  defp executable_for_current_os() do
    case :os.type() do
      {:win32, _}                                     -> "nmake"
      {:unix, type} when type in [:freebsd, :openbsd] -> "gmake"
      _                                               -> "make"
    end
  end

  # Returns a list of command-line args to pass to make (or nmake/gmake) in
  # order to specify the makefile to use.
  defp args_for_makefile("nmake", :default), do: ["/F", "Makefile.win"]
  defp args_for_makefile("nmake", makefile), do: ["/F", makefile]
  defp args_for_makefile(_, :default),       do: []
  defp args_for_makefile(_, makefile),       do: ["-f", makefile]
end
