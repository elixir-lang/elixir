defmodule Mix.Tasks.Deps.Compile do
  use Mix.Task

  @shortdoc "Compile dependencies"

  @moduledoc """
  Compile dependencies.

  By default, compile all dependencies that do not provide an app file yet.
  A list of deps can be given to force the compilation of specific deps
  or --force recompiling all deps.
  """

  import Mix.Tasks.Deps, only: [all: 0, all: 1, format_dep: 1, deps_path: 1]

  def run(args) do
    case OptionParser.Simple.parse(args) do
      { opts, [] } ->
        deps =
          if opts[:force] do
            all -- all(:unavailable)
          else
            all(:noappfile)
          end

        do_compile(deps)
      { _, tail } ->
        candidates = all

        deps = Enum.map tail, fn(app) ->
          case List.keyfind(candidates, app, 2) do
            nil -> raise Mix.Error, message: "unknown dependency #{app}"
            dep -> dep
          end
        end

        do_compile(deps)
    end
  end

  defp do_compile(deps) do
    shell = Mix.shell

    Enum.each deps, fn({ _scm, app, _req, _status, _opts }) ->
      shell.info "* Compiling #{app}"
      deps_path = deps_path(app)

      File.cd! deps_path, fn ->        
        cond do
          mix?   -> shell.info  System.cmd("mix compile --no-deps")
          rebar? -> shell.info  System.cmd("rebar compile")
          make?  -> shell.info  System.cmd("make")
          true   -> shell.error "Could not compile #{app}, no mix.exs, rebar.config or Makefile"
        end
      end
    end
  end

  defp mix?,   do: File.regular?("mix.exs")
  defp rebar?, do: File.regular?("rebar.config") or File.regular?("rebar.config.script")
  defp make?,  do: File.regular?("Makefile")
end