defmodule Mix.Tasks.Compile.All do
  use Mix.Task.Compiler

  @moduledoc false
  @compile {:no_warn_undefined, Logger}
  @recursive true

  # This is an internal task used by "mix compile" which
  # is meant to be recursive and be invoked for each child
  # project.

  @impl true
  def run(args) do
    Mix.Project.get!()
    config = Mix.Project.config()

    # Compute the app cache if it is stale and we are
    # not compiling from a dependency.
    app_cache =
      unless "--from-mix-deps-compile" in args do
        Mix.AppLoader.stale_cache(config)
      end

    # Make sure Mix.Dep is cached to avoid loading dependencies
    # during compilation. This is also important because we prune
    # the load paths before compiling, which means any SCM coming
    # from archives will be removed from the code path.
    deps = Mix.Dep.cached()
    apps = project_apps(config)
    validate_compile_env? = "--no-validate-compile-env" not in args

    {loaded_paths, loaded_modules} =
      Mix.AppLoader.load_apps(apps, deps, config, validate_compile_env?, {[], []}, fn
        {app, path}, {paths, mods} ->
          paths = if path, do: [path | paths], else: paths
          mods = if app_cache, do: [{app, Application.spec(app, :modules)} | mods], else: mods
          {paths, mods}
      end)

    # We compute the diff as that will be more efficient
    # than re-adding common paths several times
    current_paths = :code.get_path()
    Code.delete_paths(current_paths -- loaded_paths)
    Code.prepend_paths(loaded_paths -- current_paths)

    result =
      if "--no-compile" in args do
        Mix.Task.reenable("compile.all")
        {:noop, []}
      else
        # Build the project structure so we can write down compiled files.
        Mix.Project.build_structure(config)

        with_logger_app(config, fn ->
          config
          |> Mix.Tasks.Compile.compilers()
          |> compile(args, :noop, [])
        end)
      end

    if app_cache do
      Mix.AppLoader.write_cache(app_cache, Map.new(loaded_modules))
    end

    compile_path = to_charlist(Mix.Project.compile_path())
    _ = Code.prepend_path(compile_path)

    unless "--no-app-loading" in args do
      with {:error, message} <-
             Mix.AppLoader.load_app(config[:app], compile_path, validate_compile_env?) do
        Mix.raise(message)
      end
    end

    result
  end

  defp with_logger_app(config, fun) do
    app = Keyword.fetch!(config, :app)
    logger_config_app = Application.get_env(:logger, :compile_time_application)

    try do
      Logger.configure(compile_time_application: app)
      fun.()
    after
      Logger.configure(compile_time_application: logger_config_app)
    end
  end

  defp compile([], _, status, diagnostics) do
    {status, diagnostics}
  end

  defp compile([compiler | rest], args, status, diagnostics) do
    {new_status, new_diagnostics} = run_compiler(compiler, args)
    diagnostics = diagnostics ++ new_diagnostics

    case new_status do
      :error ->
        if "--return-errors" not in args do
          exit({:shutdown, 1})
        end

        {:error, diagnostics}

      :ok ->
        compile(rest, args, :ok, diagnostics)

      :noop ->
        compile(rest, args, status, diagnostics)
    end
  end

  defp run_compiler(compiler, args) do
    result = Mix.Task.Compiler.normalize(Mix.Task.run("compile.#{compiler}", args), compiler)
    Enum.reduce(Mix.ProjectStack.pop_after_compiler(compiler), result, & &1.(&2))
  end

  defp project_apps(config) do
    project = Mix.Project.get!()

    properties =
      if function_exported?(project, :application, 0), do: project.application(), else: []

    extra =
      Keyword.get(properties, :included_applications, []) ++
        Keyword.get(properties, :extra_applications, [])

    {all, _optional} =
      Mix.Tasks.Compile.App.project_apps(properties, config, extra, fn ->
        # Include all deps by design
        Enum.map(config[:deps], &elem(&1, 0))
      end)

    all
  end
end
