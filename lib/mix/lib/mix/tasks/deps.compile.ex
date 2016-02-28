defmodule Mix.Tasks.Deps.Compile do
  use Mix.Task

  @shortdoc "Compiles dependencies"

  @moduledoc """
  Compiles dependencies.

  By default, compile all dependencies. A list of dependencies can
  be given to force the compilation of specific dependencies.

  This task attempts to detect if the project contains one of
  the following files and act accordingly:

    * `mix.exs`      - invokes `mix compile`
    * `rebar.config` - invokes `rebar compile`
    * `Makefile.win` - invokes `nmake /F Makefile.win` (only on Windows)
    * `Makefile`     - invokes `make` (except on Windows)

  The compilation can be customized by passing a `compile` option
  in the dependency:

      {:some_dependency, "0.1.0", compile: "command to compile"}

  """

  import Mix.Dep, only: [loaded: 1, available?: 1, loaded_by_name: 2,
                         make?: 1, mix?: 1]

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    Mix.Project.get!

    case OptionParser.parse(args) do
      {_, [], _} ->
        # Because this command is invoked explicitly with
        # deps.compile, we simply try to compile any available
        # dependency.
        compile(Enum.filter(loaded(env: Mix.env), &available?/1))
      {_, tail, _} ->
        compile(loaded_by_name(tail, env: Mix.env))
    end
  end

  @doc false
  def compile(deps) do
    shell  = Mix.shell
    config = Mix.Project.deps_config

    Mix.Task.run "deps.loadpaths"

    compiled =
      Enum.map(deps, fn %Mix.Dep{app: app, status: status, opts: opts, scm: scm} = dep ->
        check_unavailable!(app, status)

        compiled = cond do
          not is_nil(opts[:compile]) ->
            do_compile dep, config
          mix?(dep) ->
            do_mix dep, config
          make?(dep) ->
            do_make dep, config
          dep.manager == :rebar ->
            do_rebar dep, config
          dep.manager == :rebar3 ->
            do_rebar3 dep, config
          true ->
            shell.error "Could not compile #{inspect app}, no \"mix.exs\", \"rebar.config\" or \"Makefile\" " <>
              "(pass :compile as an option to customize compilation, set it to \"false\" to do nothing)"
        end

        unless mix?(dep), do: build_structure(dep, config)
        touch_fetchable(scm, opts[:build])
        compiled
      end)

    if Enum.any?(compiled), do: Mix.Dep.Lock.touch_manifest, else: :ok
  end

  defp touch_fetchable(scm, path) do
    if scm.fetchable? do
      File.mkdir_p!(path)
      File.touch!(Path.join(path, ".compile.fetch"))
    end
  end

  defp check_unavailable!(app, {:unavailable, _}) do
    Mix.raise "Cannot compile dependency #{inspect app} because " <>
      "it isn't available, run \"mix deps.get\" first"
  end

  defp check_unavailable!(_, _) do
    :ok
  end

  defp do_mix(dep, _config) do
    Mix.Dep.in_dependency dep, fn _ ->
      if req = old_elixir_req(Mix.Project.config) do
        Mix.shell.error "warning: the dependency #{inspect dep.app} requires Elixir #{inspect req} " <>
                        "but you are running on v#{System.version}"
      end

      # Force recompilation on compile status
      if dep.status == :compile do
        Mix.Dep.Lock.touch_manifest
      end

      try do
        res = Mix.Task.run("compile", ["--no-deps", "--no-elixir-version-check"])
        :ok in List.wrap(res)
      catch
        kind, reason ->
          stacktrace = System.stacktrace
          app = dep.app
          Mix.shell.error "could not compile dependency #{inspect app}, \"mix compile\" failed. " <>
            "You can recompile this dependency with \"mix deps.compile #{app}\", update it " <>
            "with \"mix deps.update #{app}\" or clean it with \"mix deps.clean #{app}\""
          :erlang.raise(kind, reason, stacktrace)
      end
    end
  end

  defp do_rebar(dep, config) do
    lib_path = Path.join(config[:env_path], "lib")
    cmd      = "#{rebar_cmd(dep)} compile skip_deps=true deps_dir=#{inspect lib_path}"
    do_command dep, config, cmd, false
  end

  defp do_rebar3(%Mix.Dep{opts: opts} = dep, config) do
    dep_path    = opts[:build]
    config_path = Path.join(dep_path, "mix.rebar.config")
    lib_path    = Path.join(config[:env_path], "lib/*/ebin")

    env = [{"REBAR_CONFIG", config_path}]
    cmd = "#{rebar_cmd(dep)} bare compile --paths #{inspect lib_path}"

    File.mkdir_p!(dep_path)
    File.write!(config_path, Mix.Rebar.serialize_config(dep.extra))
    do_command dep, config, cmd, false, env
  end

  defp rebar_cmd(%Mix.Dep{manager: manager} = dep) do
    Mix.Rebar.rebar_cmd(manager) || handle_rebar_not_found(dep)
  end

  defp handle_rebar_not_found(%Mix.Dep{app: app, manager: manager}) do
    shell = Mix.shell
    shell.info "Could not find \"#{manager}\", which is needed to build dependency #{inspect app}"
    shell.info "I can install a local copy which is just used by Mix"

    unless shell.yes?("Shall I install #{manager}?") do
      Mix.raise "Could not find \"#{manager}\" to compile " <>
        "dependency #{inspect app}, please ensure \"#{manager}\" is available"
    end

    (Mix.Tasks.Local.Rebar.run([]) && Mix.Rebar.local_rebar_cmd(manager)) ||
      Mix.raise "\"#{manager}\" installation failed"
  end

  defp do_make(dep, config) do
    command = if match?({:win32, _}, :os.type) and File.regular?("Makefile.win") do
      "nmake /F Makefile.win"
    else
      "make"
    end
    do_command(dep, config, command, true)
  end

  defp do_compile(%Mix.Dep{opts: opts} = dep, config) do
    if command = opts[:compile] do
      do_command(dep, config, command, true)
    else
      false
    end
  end

  defp do_command(%Mix.Dep{app: app} = dep, config, command, print_app?, env \\ []) do
    Mix.Dep.in_dependency dep, fn _ ->
      env = [{"ERL_LIBS", Path.join(config[:env_path], "lib")}] ++ env
      if Mix.shell.cmd(command, print_app: print_app?, env: env) != 0 do
        Mix.raise "Could not compile dependency #{inspect app}, \"#{command}\" command failed. " <>
          "You can recompile this dependency with \"mix deps.compile #{app}\", update it " <>
          "with \"mix deps.update #{app}\" or clean it with \"mix deps.clean #{app}\""
      end
    end
    true
  end

  defp build_structure(%Mix.Dep{opts: opts} = dep, config) do
    build_path = Path.dirname(opts[:build])
    Enum.each Mix.Dep.source_paths(dep), fn {source, base} ->
      app = Path.join(build_path, base)
      build_structure(source, app, config)
      Code.prepend_path(Path.join(app, "ebin"))
    end
  end

  defp build_structure(dest, build, config) do
    File.cd! dest, fn ->
      config = Keyword.put(config, :app_path, build)
      Mix.Project.build_structure(config, symlink_ebin: true)
    end
  end

  defp old_elixir_req(config) do
    req = config[:elixir]
    if req && not Version.match?(System.version, req) do
      req
    end
  end
end
