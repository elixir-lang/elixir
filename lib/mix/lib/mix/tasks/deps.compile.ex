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

      { :some_dependency, "0.1.0", git: "...", compile: "command to compile" }

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

      deps_path = opts[:dest]
      ebin = Path.join(deps_path, "ebin") |> binary_to_list

      # Avoid compilation conflicts
      :code.del_path(ebin |> Path.expand)

      root_path = Path.expand(Mix.project[:deps_path])

      config = [
        deps_path: root_path,
        root_lockfile:  Path.expand(Mix.project[:lockfile])
      ]

      File.cd! deps_path, fn ->
        cond do
          opts[:compile] -> do_compile app, opts[:compile]
          mix?           -> do_mix dep, config
          rebar?         -> do_command app, "rebar", "compile deps_dir=#{inspect root_path}"
          make?          -> do_command app, "make"
          true           -> shell.error "Could not compile #{app}, no mix.exs, rebar.config or Makefile " <>
                             "(pass :compile as an option to customize compilation, set it to :noop to do nothing)"
        end
      end

      Code.prepend_path ebin
    end

    Mix.Deps.Lock.touch
  end

  defp mix?,   do: File.regular?("mix.exs")
  defp rebar?, do: Enum.any? ["rebar.config", "rebar.config.script"], File.regular?(&1)
  defp make?,  do: File.regular?("Makefile")

  defp check_unavailable!(app, { :unavailable, _ }) do
    raise Mix.Error, message: "Cannot compile dependency #{app} because " <>
      "it isn't available, run `mix deps.get` first"
  end

  defp check_unavailable!(_, _) do
    :ok
  end

  defp do_mix(Mix.Dep[app: app, opts: opts, project: project], config) do
    env       = opts[:env] || :prod
    old_env   = Mix.env
    old_tasks = Mix.Task.clear

    try do
      Mix.env(env)
      Mix.Project.post_config(config)
      Mix.Project.push project
      Mix.Task.run "compile", ["--no-deps"]
    catch
      kind, reason ->
        Mix.shell.error "could not compile dependency #{app}, mix compile failed. " <>
          "In case you want to recompile this dependency, please run: mix deps.compile #{app}"
        :erlang.raise(kind, reason, System.stacktrace)
    after
      Mix.env(old_env)
      Mix.Project.pop
      Mix.Task.set_tasks(old_tasks)
    end
  end

  defp do_command(app, command, extra // "") do
    if System.find_executable(command) do
      if Mix.shell.cmd("#{command} #{extra}") != 0 do
        raise Mix.Error, message: "could not compile dependency #{app}, #{command} command failed. " <>
          "In case you want to recompile this dependency, please run: mix deps.compile #{app}"
      end
    else
      raise Mix.Error, message: "could not find executable #{command} to compile " <>
        "dependency #{app}, please ensure #{command} is available"
    end
  end

  defp do_compile(_, false) do
    :ok
  end

  defp do_compile(app, command) when is_binary(command) do
    if Mix.shell.cmd(command) != 0 do
      raise Mix.Error, message: "could not compile dependency #{app}, custom #{command} command failed." <>
        "In case you want to recompile this dependency, please run: mix deps.compile #{app}"
    end
  end
end
