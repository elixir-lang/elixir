defmodule Mix.Tasks.App.Start do
  use Mix.Task

  @shortdoc "Starts all registered apps"

  @moduledoc """
  Starts all registered apps.

  First, this task guarantees that all dependencies are in place
  and that the current project has been compiled. Then, the current
  application is started as a temporary application, unless
  `:start_permanent` is set to `true` in your project configuration
  or the `--permanent` option is given. Setting it to permanent
  guarantees the node will shut down if the application terminates
  (typically because its root supervisor has terminated).

  ## Configuration

    * `:start_permanent` - the application and all of its children
      applications are started in permanent mode. Defaults to `false`.

    * `:start_concurrently` - applications are started concurrently
      whenever possible. This option only has an effect on Erlang/OTP 26+.
      Defaults to `false`.

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
      Logger.App.stop()
      config = Mix.Project.config()
      type = type(config, opts)
      mode = if config[:start_concurrently], do: :concurrent, else: :serial
      start(apps(config), type, mode)
    end

    :ok
  end

  @doc false
  def start(apps, type, mode) do
    case Application.ensure_all_started(apps, type: type, mode: mode) do
      {:ok, _} ->
        :ok

      {:error, {app, reason}} when type == :permanent ->
        # We need to stop immediately because application_controller is
        # shutting down all applications. Since any work we do here is prone
        # to race conditions as whatever process we call may no longer exist,
        # we print a quick message, and then we block by calling `System.stop/1`.
        Mix.shell().error(["** (Mix) ", could_not_start(app, reason)])
        System.stop(1)
        Process.sleep(:infinity)

      {:error, {app, reason}} ->
        Mix.raise(could_not_start(app, reason))
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
