defmodule Mix.Tasks.Deps.Compile do
  use Mix.Task

  @shortdoc "Compile dependencies"

  @moduledoc """
  Compile dependencies.

  By default, compile all dependencies that do not provide an
  app file yet. A list of deps can be given to force the compilation
  of specific deps or --force recompiling all deps.

  By default, it tries to detect if the project contains one of
  the following files:

  * `mix.exs`      - if so, invokes `mix compile`
  * `rebar.config` - if so, invokes `rebar compile`
  * `Makefile`     - if so, invokes `make`

  The compilation can be customized by passing a `compile` option
  in the dependency:

      { :some_dependency, "0.1.0", git: "...", compile: :compile_some_dependency }

  If the compile option is an atom, it will invoke the given atom
  in the current project passing the app name as argument. If a binary,
  it is considered to be command line instructions.
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

    # Load available paths but still allow it to be called down the road
    Mix.Task.run "deps.loadpaths", ["--no-check"]
    Mix.Task.reenable "deps.loadpaths"

    Enum.each deps, fn({ _scm, app, _req, _status, opts }) ->
      shell.info "* Compiling #{app}"
      deps_path = deps_path(app)

      File.cd! deps_path, fn ->        
        cond do
          opts[:compile] -> do_command(opts[:compile], app)
          mix?           -> Mix.Project.in_subproject fn -> Mix.Task.run "compile" end
          rebar?         -> shell.info  System.cmd("rebar compile")
          make?          -> shell.info  System.cmd("make")
          true           -> shell.error "Could not compile #{app}, no mix.exs, rebar.config or Makefile (pass :compile as an option to customize compilation)"
        end
      end

      Code.prepend_path File.join(deps_path, "ebin")
    end
  end

  defp mix?,   do: File.regular?("mix.exs")
  defp rebar?, do: File.regular?("rebar.config") or File.regular?("rebar.config.script")
  defp make?,  do: File.regular?("Makefile")

  defp do_command(atom, app) when is_atom(atom) do
    apply Mix.Project.current, atom, [app]
  end

  defp do_command(command, _) do
    Mix.shell.info System.cmd command
  end
end