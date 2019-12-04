defmodule Mix.Tasks.App.Start do
  use Mix.Task

  # Do not mark this task as recursive as it is
  # responsible for loading consolidated protocols.
  @shortdoc "Starts all registered apps"

  @moduledoc """
  Starts all registered apps.

  The application is started by default as temporary. In case
  `:start_permanent` is set to `true` in your project configuration
  or the `--permanent` option is given, it is started as permanent,
  which guarantees the node will shut down if the application
  crashes permanently.

  ## Configuration

    * `:start_permanent` - the application and all of its children
      applications are started in permanent mode. Defaults to `false`.

    * `:consolidate_protocols` - when `true`, loads consolidated
      protocols before start. The default value is `true`.

    * `:elixir` - matches the current Elixir version against the
      given requirement

  ## Command line options

    * `--force` - forces compilation regardless of compilation times
    * `--temporary` - starts the application as temporary
    * `--permanent` - starts the application as permanent
    * `--preload-modules` - preloads all modules defined in applications
    * `--no-compile` - does not compile even if files require compilation
    * `--no-protocols` - does not load consolidated protocols
    * `--no-archives-check` - does not check archives
    * `--no-deps-check` - does not check dependencies
    * `--no-elixir-version-check` - does not check Elixir version
    * `--no-start` - does not start applications after compilation
    * `--no-validate-compile-env` - does not validate the application compile environment

  """

  @compile {:no_warn_undefined, Logger.App}

  @switches [
    permanent: :boolean,
    temporary: :boolean,
    preload_modules: :boolean
  ]

  @impl true
  def run(args) do
    Mix.Project.get!()
    config = Mix.Project.config()

    {opts, _, _} = OptionParser.parse(args, switches: @switches)
    Mix.Task.run("loadpaths", args)

    unless "--no-compile" in args do
      Mix.Project.compile(args, config)
    end

    unless "--no-protocols" in args do
      path = Mix.Project.consolidation_path(config)

      if config[:consolidate_protocols] && File.dir?(path) do
        Code.prepend_path(path)
        Enum.each(File.ls!(path), &load_protocol/1)
      end
    end

    unless "--no-start" in args do
      apps = apps(config)

      # Stop Logger if the application does not depend on it.
      #
      # Mix should not depend directly on Logger, that's why we first check if it's loaded.
      if not logger_dependency?(apps) && Process.whereis(Logger), do: Logger.App.stop()

      start(apps, type(config, opts), "--no-validate-compile-env" not in args)

      # If there is a build path, we will let the application
      # that owns the build path do the actual check
      unless config[:build_path] do
        loaded = loaded_applications(opts)
        check_configured(loaded)
      end
    end

    :ok
  end

  @doc false
  def start(apps, type, validate_compile_env? \\ true) do
    Enum.each(apps, &ensure_all_started(&1, type, validate_compile_env?))
    :ok
  end

  defp apps(config) do
    cond do
      Mix.Project.umbrella?(config) ->
        for %Mix.Dep{app: app} <- Mix.Dep.Umbrella.cached(), do: app

      app = config[:app] ->
        [app]

      true ->
        []
    end
  end

  defp ensure_all_started(app, type, validate_compile_env?) do
    case load_check_and_start(app, type, validate_compile_env?) do
      :ok ->
        :ok

      {:error, {:already_started, ^app}} ->
        :ok

      {:error, {:not_started, dep}} ->
        :ok = ensure_all_started(dep, type, validate_compile_env?)
        ensure_all_started(app, type, validate_compile_env?)

      {:error, reason} when type == :permanent ->
        # We need to stop immediately because application_controller is
        # shutting down all applications. Since any work we do here is prone
        # to race conditions as whatever process we call may no longer exist,
        # we print a quick message and then block by calling `System.stop/1`.
        Mix.shell().error(["** (Mix) ", could_not_start(app, reason)])
        System.stop(1)

      {:error, reason} ->
        Mix.raise(could_not_start(app, reason))
    end
  end

  defp could_not_start(app, reason) do
    "Could not start application #{app}: " <> Application.format_error(reason)
  end

  defp load_check_and_start(app, type, validate_compile_env?) do
    case :application.get_key(app, :vsn) do
      {:ok, _} ->
        :application.start(app, type)

      :undefined ->
        name = Atom.to_charlist(app) ++ '.app'

        case :code.where_is_file(name) do
          :non_existing ->
            {:error, {:file.format_error(:enoent), name}}

          path ->
            case :file.consult(path) do
              {:ok, [{:application, _, properties} = application_data]} ->
                with :ok <- :application.load(application_data) do
                  if compile_env = validate_compile_env? && properties[:compile_env] do
                    compile_env = for {path, return} <- compile_env, do: {app, path, return}
                    Config.Provider.validate_compile_env(compile_env)
                  end

                  :application.start(app, type)
                end

              {:error, reason} ->
                {:error, {:file.format_error(reason), name}}
            end
        end
    end
  end

  @doc false
  def type(config, opts) do
    cond do
      opts[:temporary] -> :temporary
      opts[:permanent] -> :permanent
      config[:start_permanent] -> :permanent
      true -> :temporary
    end
  end

  defp loaded_applications(opts) do
    preload_modules? = opts[:preload_modules]

    for {app, _, _} <- Application.loaded_applications() do
      if modules = preload_modules? && Application.spec(app, :modules) do
        :code.ensure_modules_loaded(modules)
      end

      app
    end
  end

  defp check_configured(loaded) do
    configured = Mix.ProjectStack.config_apps()

    for app <- configured -- loaded, :code.lib_dir(app) == {:error, :bad_name} do
      Mix.shell().error("""
      You have configured application #{inspect(app)} in your configuration file,
      but the application is not available.

      This usually means one of:

        1. You have not added the application as a dependency in a mix.exs file.

        2. You are configuring an application that does not really exist.

      Please ensure #{inspect(app)} exists or remove the configuration.
      """)
    end

    :ok
  end

  defp logger_dependency?(apps), do: logger_dependency?(apps, [])

  defp logger_dependency?([], _checked_apps) do
    false
  end

  defp logger_dependency?([app | apps], checked_apps) do
    deps =
      List.wrap(Application.spec(app, :included_applications)) ++
        List.wrap(Application.spec(app, :applications))

    cond do
      deps == [] ->
        logger_dependency?(apps, [app | checked_apps])

      :logger in deps ->
        true

      true ->
        checked_apps = [app | checked_apps]
        new_apps = (deps -- apps) -- checked_apps
        logger_dependency?(apps ++ new_apps, checked_apps)
    end
  end

  defp load_protocol(file) do
    case file do
      "Elixir." <> _ ->
        module = file |> Path.rootname() |> String.to_atom()
        :code.purge(module)
        :code.delete(module)

      _ ->
        :ok
    end
  end
end
