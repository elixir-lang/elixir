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

  """

  @compile {:no_warn_undefined, Logger.App}

  @switches [
    permanent: :boolean,
    temporary: :boolean
  ]

  @impl true
  def run(args) do
    Mix.Project.get!()
    Mix.Task.run("app.config", args)
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
      start(apps(config), type(config, opts))
    end

    :ok
  end

  @doc false
  def start(apps, type) do
    for app <- apps do
      case Application.ensure_all_started(app, type) do
        {:ok, _} ->
          :ok

        {:error, {app, reason}} when type == :permanent ->
          # We need to stop immediately because application_controller is
          # shutting down all applications. Since any work we do here is prone
          # to race conditions as whatever process we call may no longer exist,
          # we print a quick message and then block by calling `System.stop/1`.
          Mix.shell().error(["** (Mix) ", could_not_start(app, reason)])
          System.stop(1)

        {:error, {app, reason}} ->
          Mix.raise(could_not_start(app, reason))
      end
    end

    :ok
  end

  defp apps(config) do
    cond do
      Mix.Project.umbrella?(config) -> Enum.map(Mix.Dep.Umbrella.cached(), & &1.app)
      app = config[:app] -> [app]
      true -> []
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
end
