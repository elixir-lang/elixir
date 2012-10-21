defmodule Mix.Tasks.Deps.Compile do
  use Mix.Task

  @shortdoc "Compile dependencies"

  @moduledoc """
  Compile dependencies.

  By default, compile all dependencies. A list of deps can
  be given to force the compilation of specific deps.

  By default, it tries to detect if the project contains one of
  the following files:

  * `mix.exs`      - if so, invokes `mix compile`
  * `rebar.config` - if so, invokes `rebar compile`
  * `Makefile`     - if so, invokes `make`

  The compilation can be customized by passing a `compile` option
  in the dependency:

      { :some_dependency, "0.1.0", git: "...", compile: :compile_some_dependency }

  If the compile option is an atom, it will invoke the given atom
  in the current project passing the app name as argument. Except
  if the atom is `:noop`, where nothing is done.

  If a binary, it is considered to be command line instructions
  which mix will use to shell out.
  """

  import Mix.Deps, only: [all: 0, available?: 1, by_name!: 1, format_dep: 1]

  def run(args) do
    case OptionParser.parse(args) do
      { _, [] } ->
        do_compile Enum.filter(all, available?(&1))
      { _, tail } ->
        do_compile by_name!(tail)
    end
  end

  defp do_compile(deps) do
    shell = Mix.shell

    Enum.each deps, fn(dep) ->
      Mix.Dep[app: app, status: status, opts: opts] = dep

      check_unavailable!(app, status)
      shell.info "* Compiling #{app}"

      deps_path = opts[:path]
      ebin = File.join(deps_path, "ebin") /> binary_to_list

      # Avoid compilation conflicts
      :code.del_path(ebin /> File.expand_path)

      root_path = File.expand_path(Mix.project[:deps_path])

      config = [
        deps_path: root_path,
        lockfile:  File.expand_path(Mix.project[:lockfile])
      ]

      File.cd! deps_path, fn ->
        cond do
          opts[:compile] -> do_command(opts[:compile], app)
          mix?           -> do_mix(dep, config)
          rebar?         -> shell.info System.cmd("rebar compile deps_dir=#{inspect root_path}")
          make?          -> shell.info System.cmd("make")
          true           -> shell.error "Could not compile #{app}, no mix.exs, rebar.config or Makefile " <>
                             "(pass :compile as an option to customize compilation, set it to :noop to do nothing)"
        end
      end

      Code.prepend_path ebin
    end
  end

  defp mix?,   do: File.regular?("mix.exs")
  defp rebar?, do: File.regular?("rebar.config") or File.regular?("rebar.config.script")
  defp make?,  do: File.regular?("Makefile")

  defp check_unavailable!(app, { :unavailable, _ }) do
    raise Mix.Error, message: "Cannot compile dependency #{app} because " <>
      "it isn't available, run `mix deps.get` first"
  end

  defp check_unavailable!(_, _) do
    :ok
  end

  defp do_mix(Mix.Dep[opts: opts, project: project], config) do
    env       = opts[:env] || :prod
    old_env   = Mix.env
    old_tasks = Mix.Task.clear

    try do
      Mix.env(env)
      Mix.Project.post_config(config)
      Mix.Project.push project
      Mix.Task.run "compile", ["--no-deps"]
    after
      Mix.env(old_env)
      Mix.Project.pop
      Mix.Task.set_tasks(old_tasks)
    end
  end

  defp do_command(:noop, _) do
    :ok
  end

  defp do_command(atom, app) when is_atom(atom) do
    apply Mix.Project.get!, atom, [app]
  end

  defp do_command(command, _) do
    Mix.shell.info System.cmd command
  end
end