defmodule Mix.Tasks.Deps.Compile do
  use Mix.Task

  @shortdoc "Compile dependencies"
  @recursive :both

  @moduledoc """
  Compile dependencies.

  By default, compile all dependencies. A list of dependencies can
  be given to force the compilation of specific dependencies.

  By default, attempt to detect if the project contains one of
  the following files:

  * `mix.exs`      - if so, invokes `mix compile`
  * `rebar.config` - if so, invokes `rebar compile`
  * `Makefile`     - if so, invokes `make`

  The compilation can be customized by passing a `compile` option
  in the dependency:

      { :some_dependency, "0.1.0", git: "...", compile: "command to compile" }

  """

  import Mix.Deps, only: [ all: 0, available?: 1, by_name: 2, compile_paths: 1,
                           depending: 2, format_dep: 1, make?: 1, mix?: 1, rebar?: 1 ]

  def run(args) do
    Mix.Project.get! # Require the project to be available

    case OptionParser.parse(args) do
      { _, [] } ->
        do_compile Enum.filter(all, available?(&1))
      { _, tail } ->
        all_deps = all
        deps = by_name(tail, all_deps)
        do_compile(deps ++ depending(deps, all_deps))
    end
  end

  defp do_compile(deps) do
    shell = Mix.shell

    Enum.each deps, fn(dep) ->
      Mix.Dep[app: app, status: status, opts: opts] = dep

      check_unavailable!(app, status)
      shell.info "* Compiling #{app}"

      deps_path = opts[:dest]
      root_path = Path.expand(Mix.project[:deps_path])

      config = [
        deps_path: root_path,
        root_lockfile: Path.expand(Mix.project[:lockfile])
      ]

      ebins = Enum.map compile_paths(dep), binary_to_list(&1)

      # Avoid compilation conflicts
      Enum.each ebins, fn ebin -> :code.del_path(ebin |> Path.expand) end

      cond do
        opts[:compile] -> File.cd! deps_path, fn -> do_compile app, opts[:compile] end
        mix?(dep)      -> File.cd! deps_path, fn -> do_mix dep, config end
        rebar?(dep)    -> File.cd! deps_path, fn -> do_rebar app, root_path end
        make?(dep)     -> File.cd! deps_path, fn -> do_command app, "make" end
        true           -> shell.error "Could not compile #{app}, no mix.exs, rebar.config or Makefile " <>
                           "(pass :compile as an option to customize compilation, set it to :noop to do nothing)"
      end

      Enum.each ebins, Code.prepend_path &1
    end

    Mix.Deps.Lock.touch
  end

  defp check_unavailable!(app, { :unavailable, _ }) do
    raise Mix.Error, message: "Cannot compile dependency #{app} because " <>
      "it isn't available, run `mix deps.get` first"
  end

  defp check_unavailable!(_, _) do
    :ok
  end

  defp do_mix(Mix.Dep[app: app, opts: opts], config) do
    env       = opts[:env] || :prod
    old_env   = Mix.env

    try do
      Mix.env(env)
      Mix.Project.in_project(app, ".", config, fn _ ->
        Mix.Task.run "compile", ["--no-deps"]
      end)
    catch
      kind, reason ->
        Mix.shell.error "could not compile dependency #{app}, mix compile failed. " <>
          "If you want to recompile this dependency, please run: mix deps.compile #{app}"
        :erlang.raise(kind, reason, System.stacktrace)
    after
      Mix.env(old_env)
    end
  end

  defp do_rebar(app, root_path) do
    do_command app, rebar_cmd(app), "compile skip_deps=true deps_dir=#{inspect root_path}"
  end

  defp rebar_cmd(app) do
    Mix.Rebar.rebar_cmd || handle_rebar_not_found(app)
  end

  defp handle_rebar_not_found(app) do
    shell = Mix.shell
    shell.info "Could not find rebar, which is needed to build #{app}"
    shell.info "I can install a local copy which is just used by mix"

    unless shell.yes?("Shall I install this local copy?") do
      raise Mix.Error, message: "Could not find rebar to compile " <>
        "dependency #{app}, please ensure rebar is available"
    end

    Mix.Task.run "local.rebar", []
    Mix.Rebar.local_rebar_cmd || raise Mix.Error, message: "rebar instalation failed"
  end

  defp do_command(app, command, extra // "") do
    if Mix.shell.cmd("#{command} #{extra}") != 0 do
      raise Mix.Error, message: "Could not compile dependency #{app}, #{command} command failed. " <>
        "If you want to recompile this dependency, please run: mix deps.compile #{app}"
    end
  end

  defp do_compile(_, false) do
    :ok
  end

  defp do_compile(app, command) when is_binary(command) do
    Mix.shell.info("#{app}: #{command}")
    if Mix.shell.cmd(command) != 0 do
      raise Mix.Error, message: "Could not compile dependency #{app}, custom #{command} command failed. " <>
        "If you want to recompile this dependency, please run: mix deps.compile #{app}"
    end
  end
end
