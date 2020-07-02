defmodule Mix.Tasks.App.Start do
  use Mix.Task

  @shortdoc "Starts all registered apps"

  @moduledoc """
  Starts all registered apps.

  First this task guarantees that all dependencies are in place
  and that the current project has been compiled. Then the current
  application is started as a temporary application, unless
  `:start_permanent` is set to `true` in your project configuration
  or the `--permanent` option is given, then it's started as permanent,
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
    * `--no-archives-check` - does not check archives
    * `--no-compile` - does not compile even if files require compilation
    * `--no-deps-check` - does not check dependencies
    * `--no-elixir-version-check` - does not check Elixir version
    * `--no-start` - does not actually start applications, only compiles and loads code
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
    Mix.Task.run("compile", args)
    {opts, _, _} = OptionParser.parse(args, switches: @switches)

    if "--no-start" in args do
      Mix.Task.reenable("app.start")
    else
      # Stop Logger when starting the application as it is up to the
      # application to decide if it should be restarted or not.
      #
      # Mix should not depend directly on Logger so check that it's loaded.
      if Process.whereis(Logger) do
        Logger.App.stop()
      end

      config = Mix.Project.config()

      # For our own apps, now is the time to validate compile env.
      start(apps(config), type(config, opts), "--no-validate-compile-env" not in args)

      if opts[:preload_modules] do
        for {app, _, _} <- Application.loaded_applications() do
          :code.ensure_modules_loaded(Application.spec(app, :modules))
        end
      end

      # If the build path is set, we assume it is being shared
      # (such as in an umbrella) and therefore we cannot check
      # for applications as we may have both false positives and
      # false negatives.
      #
      # Therefore we let the application that owns the build path
      # to ultimately perform the check.
      unless config[:build_path] do
        check_configured()
      end
    end

    :ok
  end

  @doc false
  def start(apps, type, validate_compile_env? \\ true) do
    Enum.each(apps, fn app ->
      Mix.Tasks.App.Load.load(app, validate_compile_env?)
      ensure_all_started(app, type)
    end)

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

  defp ensure_all_started(app, type) do
    case Application.start(app, type) do
      :ok ->
        :ok

      {:error, {:already_started, ^app}} ->
        :ok

      {:error, {:not_started, dep}} ->
        :ok = ensure_all_started(dep, type)
        ensure_all_started(app, type)

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

  @doc false
  def type(config, opts) do
    cond do
      opts[:temporary] -> :temporary
      opts[:permanent] -> :permanent
      config[:start_permanent] -> :permanent
      true -> :temporary
    end
  end

  defp check_configured() do
    for app <- Mix.ProjectStack.config_apps(),
        is_nil(Application.spec(app, :vsn)) do
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
end
