defmodule Mix.Tasks.Deps.Compile do
  use Mix.Task

  @shortdoc "Compile dependencies"

  @moduledoc """
  Compile dependencies.

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
                         format_dep: 1, make?: 1, mix?: 1, rebar?: 1]

  def run(args) do
    Mix.Project.get! # Require the project to be available

    case OptionParser.parse(args) do
      {opts, [], _} ->
        compile(Enum.filter(loaded(env: Mix.env), &compilable?/1), opts)
      {opts, tail, _} ->
        compile(loaded_by_name(tail, env: Mix.env), opts)
    end
  end

  @doc false
  def compile(deps, _opts) do
    shell  = Mix.shell
    config = Mix.Project.deps_config

    compiled =
      Enum.map(deps, fn %Mix.Dep{app: app, status: status, opts: opts, scm: scm} = dep ->
        check_unavailable!(app, status)

        compiled = cond do
          not nil?(opts[:compile]) ->
            do_compile dep
          mix?(dep) ->
            do_mix dep
          rebar?(dep) ->
            do_rebar dep, config
          make?(dep) ->
            do_make dep
          true ->
            shell.error "Could not compile #{app}, no mix.exs, rebar.config or Makefile " <>
              "(pass :compile as an option to customize compilation, set it to false to do nothing)"
        end

        unless mix?(dep), do: build_structure(dep, config)
        touch_fetchable(scm, opts[:build])
        compiled
      end)

    if Enum.any?(compiled), do: Mix.Dep.Lock.touch
  end

  # All available dependencies can be compiled
  # except for umbrella applications.
  defp compilable?(%Mix.Dep{extra: extra} = dep) do
    available?(dep) and !extra[:umbrella?]
  end

  defp touch_fetchable(scm, path) do
    if scm.fetchable? do
      File.mkdir_p!(path)
      File.touch!(Path.join(path, ".compile.fetch"))
    end
  end

  defp check_unavailable!(app, {:unavailable, _}) do
    Mix.raise "Cannot compile dependency #{app} because " <>
      "it isn't available, run `mix deps.get` first"
  end

  defp check_unavailable!(_, _) do
    :ok
  end

  defp do_mix(dep) do
    Mix.Dep.in_dependency dep, fn _ ->
      try do
        res = Mix.Task.run("compile", ["--no-deps", "--no-elixir-version-check", "--no-readd"])
        :ok in List.wrap(res)
      catch
        kind, reason ->
          stacktrace = System.stacktrace
          app = dep.app
          Mix.shell.error "could not compile dependency #{app}, mix compile failed. " <>
            "You can recompile this dependency with `mix deps.compile #{app}` or " <>
            "update it with `mix deps.update #{app}`"
          :erlang.raise(kind, reason, stacktrace)
      end
    end
  end

  defp do_rebar(%Mix.Dep{app: app} = dep, config) do
    do_command dep, rebar_cmd(app), "compile skip_deps=true deps_dir=#{inspect config[:deps_path]}"
  end

  defp rebar_cmd(app) do
    Mix.Rebar.rebar_cmd || handle_rebar_not_found(app)
  end

  defp handle_rebar_not_found(app) do
    shell = Mix.shell
    shell.info "Could not find rebar, which is needed to build dependency #{inspect app}"
    shell.info "I can install a local copy which is just used by mix"

    unless shell.yes?("Shall I install rebar?") do
      Mix.raise "Could not find rebar to compile " <>
        "dependency #{app}, please ensure rebar is available"
    end

    (Mix.Tasks.Local.Rebar.run([]) && Mix.Rebar.local_rebar_cmd) ||
      Mix.raise "rebar installation failed"
  end

  defp do_make(dep) do
    if match?({:win32, _}, :os.type) and File.regular?("Makefile.win") do
      do_command(dep, "nmake /F Makefile.win")
    else
      do_command(dep, "make")
    end
  end

  defp do_compile(%Mix.Dep{app: app, opts: opts} = dep) do
    if command = opts[:compile] do
      Mix.shell.info("#{app}: #{command}")
      do_command(dep, command)
    else
      false
    end
  end

  defp do_command(%Mix.Dep{app: app, opts: opts}, command, extra \\ "") do
    File.cd! opts[:dest], fn ->
      if Mix.shell.cmd("#{command} #{extra}") != 0 do
        Mix.raise "Could not compile dependency #{app}, #{command} command failed. " <>
          "If you want to recompile this dependency, please run: mix deps.compile #{app}"
      end
    end
    true
  end

  defp build_structure(%Mix.Dep{opts: opts} = dep, config) do
    build_path = Path.dirname(opts[:build])
    Enum.each Mix.Dep.source_paths(dep), fn source ->
      app = Path.join(build_path, Path.basename(source))
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
end
