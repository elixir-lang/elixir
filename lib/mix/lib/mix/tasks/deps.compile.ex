defmodule Mix.Tasks.Deps.Compile do
  use Mix.Task

  @shortdoc "Compiles dependencies"

  @moduledoc """
  Compiles dependencies.

  By default, this task attempts to compile all dependencies.
  A list of dependencies can be given to compile multiple
  dependencies in order.

  This task attempts to detect if the project contains one of
  the following files and act accordingly:

    * `mix.exs` - invokes `mix compile`
    * `rebar.config` - invokes `rebar compile`
    * `Makefile.win`- invokes `nmake /F Makefile.win` (only on Windows)
    * `Makefile` - invokes `gmake` on DragonFlyBSD, FreeBSD, NetBSD, and OpenBSD,
      invokes `make` on any other operating system (except on Windows)

  The compilation can be customized by passing a `compile` option
  in the dependency:

      {:some_dependency, "0.1.0", compile: "command to compile"}

  If a list of dependencies is given, Mix will attempt to compile
  them as is. For example, if project `a` depends on `b`, calling
  `mix deps.compile a` will compile `a` even if `b` is out of
  date. This is to allow parts of the dependency tree to be
  recompiled without propagating those changes upstream. To ensure
  `b` is included in the compilation step, pass `--include-children`.

  ## Command line options

    * `--force` - force compilation of deps
    * `--skip-umbrella-children` - skips umbrella applications from compiling
    * `--skip-local-deps` - skips non-remote dependencies, such as path deps, from compiling

  """

  @switches [
    include_children: :boolean,
    force: :boolean,
    skip_umbrella_children: :boolean,
    skip_local_deps: :boolean
  ]

  @impl true
  def run(args) do
    if "--no-archives-check" not in args do
      Mix.Task.run("archive.check", args)
    end

    Mix.Project.get!()

    config = Mix.Project.config()

    Mix.Project.with_build_lock(config, fn ->
      deps = Mix.Dep.load_and_cache()

      case OptionParser.parse(args, switches: @switches) do
        {opts, [], _} ->
          compile(filter_available_and_local_deps(deps), opts)

        {opts, tail, _} ->
          compile(Mix.Dep.filter_by_name(tail, deps, opts), opts)
      end
    end)
  end

  @doc false
  def compile(deps, options \\ []) do
    shell = Mix.shell()
    config = Mix.Project.deps_config()
    Mix.Task.run("deps.precompile")

    compiled =
      deps
      |> reject_umbrella_children(options)
      |> reject_local_deps(options)
      |> Enum.map(fn %Mix.Dep{app: app, status: status, opts: opts, scm: scm} = dep ->
        check_unavailable!(app, scm, status)
        maybe_clean(dep, options)

        compiled? =
          cond do
            not is_nil(opts[:compile]) ->
              do_compile(dep, config)

            Mix.Dep.mix?(dep) ->
              do_mix(dep, config)

            Mix.Dep.make?(dep) ->
              do_make(dep, config)

            dep.manager == :rebar3 ->
              do_rebar3(dep, config)

            true ->
              shell.error(
                "Could not compile #{inspect(app)}, no \"mix.exs\", \"rebar.config\" or \"Makefile\" " <>
                  "(pass :compile as an option to customize compilation, set it to \"false\" to do nothing)"
              )

              false
          end

        if compiled? do
          build_path = Mix.Project.build_path(config)

          lazy_message = fn ->
            info = %{
              app: dep.app,
              scm: dep.scm,
              manager: dep.manager,
              os_pid: System.pid()
            }

            {:dep_compiled, info}
          end

          Mix.Sync.PubSub.broadcast(build_path, lazy_message)
        end

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
    if scm.fetchable?() do
      path = Path.join(path, ".mix")
      File.mkdir_p!(path)
      File.touch!(Path.join(path, "compile.fetch"))
      true
    else
      false
    end
  end

  defp check_unavailable!(app, scm, {:unavailable, path}) do
    if scm.fetchable?() do
      Mix.raise(
        "Cannot compile dependency #{inspect(app)} because " <>
          "it isn't available, run \"mix deps.get\" first"
      )
    else
      Mix.raise(
        "Cannot compile dependency #{inspect(app)} because " <>
          "it isn't available, please ensure the dependency is at " <>
          inspect(Path.relative_to_cwd(path))
      )
    end
  end

  defp check_unavailable!(_, _, _) do
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
        options = ["--from-mix-deps-compile", "--no-warnings-as-errors", "--no-code-path-pruning"]
        res = Mix.Task.run("compile", options)
        match?({:ok, _}, res)
      catch
        kind, reason ->
          app = dep.app

          Mix.shell().error(
            "could not compile dependency #{inspect(app)}, \"mix compile\" failed. " <>
              deps_compile_feedback(app)
          )

          :erlang.raise(kind, reason, __STACKTRACE__)
      end
    end)
  end

  defp do_rebar3(%Mix.Dep{opts: opts} = dep, config) do
    if not Mix.Rebar.available?(:rebar3) do
      handle_rebar_not_found(dep)
    end

    dep_path = opts[:dest]
    build_path = opts[:build]
    File.mkdir_p!(build_path)

    # For Rebar3, we need to copy the source/ebin to the target/ebin
    # before we run the command given that REBAR_BARE_COMPILER_OUTPUT_DIR
    # writes directly to _build.
    File.cp_r(Path.join(dep_path, "ebin"), Path.join(build_path, "ebin"))

    # Now establish symlinks to the remaining sources
    for dir <- ~w(include priv src) do
      Mix.Utils.symlink_or_copy(Path.join(dep_path, dir), Path.join(build_path, dir))
    end

    # Build the rebar config and setup the command line
    config_path = Path.join(build_path, "mix.rebar.config")
    lib_path = Path.join(config[:deps_build_path], "lib/*/ebin")
    File.write!(config_path, rebar_config(dep))

    env = [
      # REBAR_BARE_COMPILER_OUTPUT_DIR is only honored by rebar3 >= 3.14
      {"REBAR_BARE_COMPILER_OUTPUT_DIR", build_path},
      {"REBAR_CONFIG", config_path},
      {"REBAR_PROFILE", "prod"},
      {"TERM", "dumb"}
    ]

    {exec, args} = Mix.Rebar.rebar_args(:rebar3, ["bare", "compile", "--paths", lib_path])

    if Mix.shell().cmd({exec, args}, opts_for_cmd(dep, config, env)) != 0 do
      Mix.raise(
        "Could not compile dependency #{inspect(dep.app)}, \"#{Enum.join([exec | args], " ")}\" command failed. " <>
          deps_compile_feedback(dep.app)
      )
    end

    # Check if we have any new symlinks after compilation
    for dir <- ~w(include priv src),
        File.exists?(Path.join(dep_path, dir)) and not File.exists?(Path.join(build_path, dir)) do
      Mix.Utils.symlink_or_copy(Path.join(dep_path, dir), Path.join(build_path, dir))
    end

    Code.prepend_path(Path.join(build_path, "ebin"), cache: true)
    true
  end

  defp rebar_config(dep) do
    dep.extra
    |> Mix.Rebar.dependency_config()
    |> Mix.Rebar.serialize_config()
  end

  defp handle_rebar_not_found(%Mix.Dep{app: app, manager: manager}) do
    shell = Mix.shell()

    shell.info(
      "Could not find \"#{manager}\", which is needed to build dependency #{inspect(app)}"
    )

    install_question =
      "Shall I install #{manager}? (if running non-interactively, " <>
        "use \"mix local.rebar --force\")"

    if not shell.yes?(install_question) do
      error_message =
        "Could not find \"#{manager}\" to compile " <>
          "dependency #{inspect(app)}, please ensure \"#{manager}\" is available"

      Mix.raise(error_message)
    end

    Mix.Tasks.Local.Rebar.run(["--force"])

    if not Mix.Rebar.available?(manager) do
      Mix.raise("\"#{manager}\" installation failed")
    end
  end

  defp do_make(dep, config) do
    command = make_command(dep)
    shell_cmd!(dep, config, command, [{"IS_DEP", "1"}])
    build_structure(dep, config)
    true
  end

  defp make_command(dep) do
    makefile_win? = makefile_win?(dep)

    command =
      case :os.type() do
        {:win32, _} when makefile_win? ->
          "nmake /F Makefile.win"

        {:unix, type} when type in [:dragonfly, :freebsd, :netbsd, :openbsd] ->
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
      shell_cmd!(dep, config, command)
      build_structure(dep, config)
      true
    else
      false
    end
  end

  defp shell_cmd!(%Mix.Dep{app: app} = dep, config, command, env \\ []) do
    if Mix.shell().cmd(command, [print_app: true] ++ opts_for_cmd(dep, config, env)) != 0 do
      Mix.raise(
        "Could not compile dependency #{inspect(app)}, \"#{command}\" command failed. " <>
          deps_compile_feedback(app)
      )
    end

    :ok
  end

  defp opts_for_cmd(dep, config, env) do
    %Mix.Dep{system_env: system_env, opts: opts} = dep
    env = [{"ERL_LIBS", Path.join(config[:deps_build_path], "lib")} | system_env] ++ env
    [env: env, cd: opts[:dest]]
  end

  defp build_structure(%Mix.Dep{opts: opts}, config) do
    config = Keyword.put(config, :deps_app_path, opts[:build])
    Mix.Project.build_structure(config, symlink_ebin: true, source: opts[:dest])
    Code.prepend_path(Path.join(opts[:build], "ebin"), cache: true)
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

  defp reject_umbrella_children(deps, options) do
    if options[:skip_umbrella_children] do
      Enum.reject(deps, fn %{opts: opts} -> Keyword.get(opts, :from_umbrella) == true end)
    else
      deps
    end
  end

  defp filter_available_and_local_deps(deps) do
    Enum.filter(deps, fn dep ->
      Mix.Dep.available?(dep) or not dep.scm.fetchable?()
    end)
  end

  defp reject_local_deps(deps, options) do
    if options[:skip_local_deps] do
      Enum.filter(deps, fn %{scm: scm} -> scm.fetchable?() end)
    else
      deps
    end
  end

  defp deps_compile_feedback(app) do
    if Mix.install?() do
      "Errors may have been logged above. You may run Mix.install/2 to try again or " <>
        "change the arguments to Mix.install/2 to try another version"
    else
      "Errors may have been logged above. You can recompile this dependency with " <>
        "\"mix deps.compile #{app} --force\", update it with \"mix deps.update #{app}\" or " <>
        "clean it with \"mix deps.clean #{app}\""
    end
  end
end
