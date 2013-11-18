defmodule Mix.Tasks.Deps.Compile do
  use Mix.Task

  @shortdoc "Compile dependencies"

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

  ## Command line options

  * `--quiet` - do not output verbose messages

  """

  import Mix.Deps, only: [loaded: 0, compilable?: 1, loaded_by_name: 1,
                          format_dep: 1, make?: 1, mix?: 1, rebar?: 1]

  def run(args) do
    Mix.Project.get! # Require the project to be available

    case OptionParser.parse(args, switches: [quiet: :boolean]) do
      { opts, [], _ } ->
        do_run(Enum.filter(loaded, &compilable?/1), opts)
      { opts, tail, _ } ->
        do_run(loaded_by_name(tail), opts)
    end
  end

  defp do_run(deps, run_opts) do
    shell  = Mix.shell
    config = Mix.Project.deps_config

    compiled =
      Enum.map deps, fn(dep) ->
        Mix.Dep[app: app, status: status, opts: opts] = dep
        check_unavailable!(app, status)

        unless run_opts[:quiet] || opts[:compile] == false do
          shell.info "* Compiling #{app}"
        end

        compiled = cond do
          not nil?(opts[:compile]) ->
            do_compile dep
          mix?(dep) ->
            do_mix dep, config
          rebar?(dep) ->
            do_rebar dep, config
          make?(dep) ->
            do_make dep
          true ->
            shell.error "Could not compile #{app}, no mix.exs, rebar.config or Makefile " <>
              "(pass :compile as an option to customize compilation, set it to false to do nothing)"
        end

        unless mix?(dep), do: build_structure(dep, config)
        compiled
      end

    if Enum.any?(compiled), do: Mix.Deps.Lock.touch
  end

  defp check_unavailable!(app, { :unavailable, _ }) do
    raise Mix.Error, message: "Cannot compile dependency #{app} because " <>
      "it isn't available, run `mix deps.get` first"
  end

  defp check_unavailable!(_, _) do
    :ok
  end

  defp do_mix(Mix.Dep[app: app, opts: opts] = dep, config) do
    # Set the app_path to be the one stored in the dependency.
    # This is important because the name of application in the
    # mix.exs file can be different than the actual name and we
    # choose to respect the one in the mix.exs.
    config = Keyword.put(config, :app_path, opts[:build])

    Mix.Deps.in_dependency dep, config, fn _ ->
      case dev_compilation(app, config) do
        { source, target } ->
          File.rm_rf!(target)
          File.mkdir_p!(target)
          File.cp_r!(source, Path.dirname(target))
        false ->
          :ok
      end

      try do
        res = Mix.Task.run("compile", ["--no-deps"])
        :ok in List.wrap(res)
      catch
        kind, reason ->
          app = dep.app
          Mix.shell.error "could not compile dependency #{app}, mix compile failed. " <>
            "You can recompile this dependency with `mix deps.compile #{app}` or " <>
            "update it with `mix deps.update #{app}`"
          :erlang.raise(kind, reason, System.stacktrace)
      end
    end
  end

  # In case we have build per environments and the dependency was already
  # compiled for development and the target is stale compared to the
  # development (source) one, we simply copy the source to target and do
  # the compilation on top of it.
  defp dev_compilation(app, config) do
    if config[:build_per_environment] do
      source = config[:build_path] |> Path.dirname |> Path.join("dev/lib/#{app}")
      target = Mix.Project.manifest_path(config)
      source != target && Mix.Deps.Lock.mix_env(source) == to_string(Mix.env) &&
        not Mix.Utils.stale?([target], [source]) && { source, target }
    else
      false
    end
  end

  defp do_rebar(Mix.Dep[app: app] = dep, config) do
    do_command dep, rebar_cmd(app), "compile skip_deps=true deps_dir=#{inspect config[:deps_path]}"
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

  defp do_make(dep) do
    do_command(dep, "make")
  end

  defp do_compile(Mix.Dep[app: app, opts: opts] = dep) do
    if command = opts[:compile] do
      Mix.shell.info("#{app}: #{command}")
      do_command(dep, command)
    else
      false
    end
  end

  defp do_command(Mix.Dep[app: app, opts: opts], command, extra // "") do
    File.cd! opts[:dest], fn ->
      if Mix.shell.cmd("#{command} #{extra}") != 0 do
        raise Mix.Error, message: "Could not compile dependency #{app}, #{command} command failed. " <>
          "If you want to recompile this dependency, please run: mix deps.compile #{app}"
      end
    end
    true
  end

  defp build_structure(Mix.Dep[opts: opts] = dep, config) do
    build_path = Path.dirname(opts[:build])
    Enum.each Mix.Deps.source_paths(dep), fn source ->
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
