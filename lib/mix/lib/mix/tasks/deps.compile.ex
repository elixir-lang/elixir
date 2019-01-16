defmodule Mix.Tasks.Deps.Compile do
  use Mix.Task

  @shortdoc "Compiles dependencies"

  @moduledoc """
  Compiles dependencies.

  By default, compile all dependencies. A list of dependencies
  can be given to compile multiple dependencies in order.

  This task attempts to detect if the project contains one of
  the following files and act accordingly:

    * `mix.exs`      - invokes `mix compile`
    * `rebar.config` - invokes `rebar compile`
    * `Makefile.win` - invokes `nmake /F Makefile.win` (only on Windows)
    * `Makefile`     - invokes `gmake` on FreeBSD and OpenBSD, invokes `make` on
                       any other operating system (except on Windows)

  The compilation can be customized by passing a `compile` option
  in the dependency:

      {:some_dependency, "0.1.0", compile: "command to compile"}

  If a list of dependencies is given, Mix will attempt to compile
  them as is. For example, if project `a` depends on `b`, calling
  `mix deps.compile a` will compile `a` even if `b` is out of
  date. This is to allow parts of the dependency tree to be
  recompiled without propagating those changes upstream. To ensure
  `b` is included in the compilation step, pass `--include-children`.
  """

  @switches [include_children: :boolean, force: :boolean]

  @impl true
  def run(args) do
    unless "--no-archives-check" in args do
      Mix.Task.run("archive.check", args)
    end

    Mix.Project.get!()
    deps = Mix.Dep.load_and_cache()

    case OptionParser.parse(args, switches: @switches) do
      {opts, [], _} ->
        # Because this command may be invoked explicitly with
        # deps.compile, we simply try to compile any available
        # dependency.
        compile(Enum.filter(deps, &Mix.Dep.available?/1), opts)

      {opts, tail, _} ->
        compile(Mix.Dep.filter_by_name(tail, deps, opts), opts)
    end
  end

  @doc false
  def compile(deps, options \\ []) do
    shell = Mix.shell()
    config = Mix.Project.deps_config()

    Mix.Task.run("deps.precompile")

    compiled =
      Enum.map(deps, fn %Mix.Dep{app: app, status: status, opts: opts, scm: scm} = dep ->
        check_unavailable!(app, status)
        maybe_clean(dep, options)

        compiled? =
          cond do
            not is_nil(opts[:compile]) ->
              do_compile(dep, config)

            Mix.Dep.mix?(dep) ->
              do_mix(dep, config)

            Mix.Dep.make?(dep) ->
              do_make(dep, config)

            dep.manager == :rebar ->
              do_rebar(dep, config)

            dep.manager == :rebar3 ->
              do_rebar3(dep, config)

            true ->
              shell.error(
                "Could not compile #{inspect(app)}, no \"mix.exs\", \"rebar.config\" or \"Makefile\" " <>
                  "(pass :compile as an option to customize compilation, set it to \"false\" to do nothing)"
              )

              false
          end

        unless Mix.Dep.mix?(dep), do: build_structure(dep, config)

        # We should touch fetchable dependencies even if they
        # did not compile otherwise they will always be marked
        # as stale, even when there is nothing to do.
        fetchable? = touch_fetchable(scm, opts[:build])

        compiled? and fetchable?
      end)

    if true in compiled, do: Mix.Task.run("will_recompile"), else: :ok
  end

  defp maybe_clean(dep, opts) do
    # If a dependency was marked as fetched or with an out of date lock
    # or missing the app file, we always compile it from scratch.
    if Keyword.get(opts, :force, false) or Mix.Dep.compilable?(dep) do
      File.rm_rf!(Path.join([Mix.Project.build_path(), "lib", Atom.to_string(dep.app)]))
    end
  end

  defp touch_fetchable(scm, path) do
    if scm.fetchable? do
      path = Path.join(path, ".mix")
      File.mkdir_p!(path)
      File.touch!(Path.join(path, "compile.fetch"))
      true
    else
      false
    end
  end

  defp check_unavailable!(app, {:unavailable, _}) do
    Mix.raise(
      "Cannot compile dependency #{inspect(app)} because " <>
        "it isn't available, run \"mix deps.get\" first"
    )
  end

  defp check_unavailable!(_, _) do
    :ok
  end

  defp do_mix(dep, _config) do
    Mix.Dep.in_dependency(dep, fn _ ->
      config = Mix.Project.config()

      if req = old_elixir_req(config) do
        Mix.shell().error(
          "warning: the dependency #{inspect(dep.app)} requires Elixir #{inspect(req)} " <>
            "but you are running on v#{System.version()}"
        )
      end

      try do
        options = [
          "--no-deps",
          "--no-archives-check",
          "--no-elixir-version-check",
          "--no-warnings-as-errors"
        ]

        res = Mix.Task.run("compile", options)
        match?({:ok, _}, res)
      catch
        kind, reason ->
          app = dep.app

          Mix.shell().error(
            "could not compile dependency #{inspect(app)}, \"mix compile\" failed. " <>
              "You can recompile this dependency with \"mix deps.compile #{app}\", update it " <>
              "with \"mix deps.update #{app}\" or clean it with \"mix deps.clean #{app}\""
          )

          :erlang.raise(kind, reason, __STACKTRACE__)
      end
    end)
  end

  defp do_rebar(dep, config) do
    lib_path = Path.join(config[:env_path], "lib")
    cmd = "#{rebar_cmd(dep)} compile skip_deps=true deps_dir=#{inspect(lib_path)}"
    do_command(dep, config, cmd, false)
  end

  defp do_rebar3(%Mix.Dep{opts: opts} = dep, config) do
    dep_path = opts[:build]
    config_path = Path.join(dep_path, "mix.rebar.config")
    lib_path = Path.join(config[:env_path], "lib/*/ebin")

    env = [{"REBAR_CONFIG", config_path}, {"TERM", "dumb"}]
    cmd = "#{rebar_cmd(dep)} bare compile --paths #{inspect(lib_path)}"

    File.mkdir_p!(dep_path)
    File.write!(config_path, rebar_config(dep))
    do_command(dep, config, cmd, false, env)
  end

  defp rebar_config(dep) do
    dep.extra
    |> Mix.Rebar.dependency_config()
    |> Mix.Rebar.serialize_config()
  end

  defp rebar_cmd(%Mix.Dep{manager: manager} = dep) do
    Mix.Rebar.rebar_cmd(manager) || handle_rebar_not_found(dep)
  end

  defp handle_rebar_not_found(%Mix.Dep{app: app, manager: manager}) do
    shell = Mix.shell()

    shell.info(
      "Could not find \"#{manager}\", which is needed to build dependency #{inspect(app)}"
    )

    shell.info("I can install a local copy which is just used by Mix")

    install_question =
      "Shall I install #{manager}? (if running non-interactively, " <>
        "use \"mix local.rebar --force\")"

    unless shell.yes?(install_question) do
      error_message =
        "Could not find \"#{manager}\" to compile " <>
          "dependency #{inspect(app)}, please ensure \"#{manager}\" is available"

      Mix.raise(error_message)
    end

    (Mix.Tasks.Local.Rebar.run([]) && Mix.Rebar.local_rebar_cmd(manager)) ||
      Mix.raise("\"#{manager}\" installation failed")
  end

  defp do_make(dep, config) do
    command = make_command(dep)
    do_command(dep, config, command, true, [{"IS_DEP", "1"}])
  end

  defp make_command(dep) do
    makefile_win? = makefile_win?(dep)

    command =
      case :os.type() do
        {:win32, _} when makefile_win? ->
          "nmake /F Makefile.win"

        {:unix, type} when type in [:freebsd, :openbsd] ->
          "gmake"

        _ ->
          "make"
      end

    if erlang_mk?(dep) do
      "#{command} clean && #{command}"
    else
      command
    end
  end

  defp do_compile(%Mix.Dep{opts: opts} = dep, config) do
    if command = opts[:compile] do
      do_command(dep, config, command, true)
    else
      false
    end
  end

  defp do_command(dep, config, command, print_app?, env \\ []) do
    %Mix.Dep{app: app, system_env: system_env, opts: opts} = dep

    File.cd!(opts[:dest], fn ->
      env = [{"ERL_LIBS", Path.join(config[:env_path], "lib")} | system_env] ++ env

      if Mix.shell().cmd(command, env: env, print_app: print_app?) != 0 do
        Mix.raise(
          "Could not compile dependency #{inspect(app)}, \"#{command}\" command failed. " <>
            "You can recompile this dependency with \"mix deps.compile #{app}\", update it " <>
            "with \"mix deps.update #{app}\" or clean it with \"mix deps.clean #{app}\""
        )
      end
    end)

    true
  end

  defp build_structure(%Mix.Dep{opts: opts} = dep, config) do
    build_path = Path.dirname(opts[:build])

    Enum.each(Mix.Dep.source_paths(dep), fn {source, base} ->
      app = Path.join(build_path, base)
      build_structure(source, app, config)
      Code.prepend_path(Path.join(app, "ebin"))
    end)
  end

  defp build_structure(dest, build, config) do
    File.cd!(dest, fn ->
      config = Keyword.put(config, :app_path, build)
      Mix.Project.build_structure(config, symlink_ebin: true)
    end)
  end

  defp old_elixir_req(config) do
    req = config[:elixir]

    if req && not Version.match?(System.version(), req) do
      req
    end
  end

  defp erlang_mk?(%Mix.Dep{opts: opts}) do
    File.regular?(Path.join(opts[:dest], "erlang.mk"))
  end

  defp makefile_win?(%Mix.Dep{opts: opts}) do
    File.regular?(Path.join(opts[:dest], "Makefile.win"))
  end
end
