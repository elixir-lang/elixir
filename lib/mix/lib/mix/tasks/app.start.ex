defmodule Mix.Tasks.App.Start do
  use Mix.Task

  # Do not mark this task as recursive as it is
  # responsible for loading consolidated protocols.
  @shortdoc "Starts all registered apps"

  @moduledoc """
  Starts all registered apps.

  The application is started by default as temporary. In case
  `:start_permanent` is set to `true` in your project configuration
  or the `--permanent` flag is given, it is started as permanent,
  which guarantees the node will shutdown if the application
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
    * `--no-compile` - does not compile even if files require compilation
    * `--no-protocols` - does not load consolidated protocols
    * `--no-archives-check` - does not check archives
    * `--no-deps-check` - does not check dependencies
    * `--no-elixir-version-check` - does not check Elixir version
    * `--no-start` - does not start applications after compilation

  """
  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    Mix.Project.get!
    config = Mix.Project.config

    {opts, _, _} = OptionParser.parse args, switches: [permanent: :boolean, temporary: :boolean]
    Mix.Task.run "loadpaths", args

    unless "--no-compile" in args do
      Mix.Project.compile(args, config)
    end

    unless "--no-protocols" in args do
      path = Path.join(Mix.Project.build_path(config), "consolidated")

      if config[:consolidate_protocols] && File.dir?(path) do
        Code.prepend_path(path)
        Enum.each(File.ls!(path), &load_protocol/1)
      end
    end

    # Stop Logger when starting the application as it is
    # up to the application to decide if it should be restarted
    # or not.
    #
    # Mix should not depend directly on Logger so check that it's loaded.
    logger = Process.whereis(Logger)
    if logger do
      Logger.App.stop
    end

    if "--no-start" in args do
      # Start Logger again if the application won't be starting it
      if logger do
        :ok = Logger.App.start
      end
    else
      start(Mix.Project.config, opts)
    end

    :ok
  end

  @doc false
  def start(config, opts) do
    apps =
      cond do
        Mix.Project.umbrella?(config) ->
          for %Mix.Dep{app: app} <- Mix.Dep.Umbrella.cached, do: app
        app = config[:app] ->
          [app]
        true ->
          []
      end

    type = type(config, opts)
    Enum.each apps, &ensure_all_started(&1, type)

    # If there is a build path, we will let the application
    # that owns the build path do the actual check
    unless config[:build_path] do
      check_configured()
    end

    :ok
  end

  defp ensure_all_started(app, type) do
    case Application.ensure_all_started(app, type) do
      {:ok, _} -> :ok
      {:error, {app, reason}} ->
        Mix.raise "Could not start application #{app}: " <>
          Application.format_error(reason)
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

  defp check_configured() do
    configured = Mix.ProjectStack.configured_applications
    loaded = for {app, _, _} <- Application.loaded_applications(), do: app
    _ = for app <- configured -- loaded,
           :code.lib_dir(app) == {:error, :bad_name} do
      Mix.shell.error """
      You have configured application #{inspect app} in your configuration
      file, but the application is not available.

      This usually means one of:

      1. You have not added the application as a dependency in a mix.exs file.

      2. You are configuring an application that does not really exist.

      Please ensure #{inspect app} exists or remove the configuration.
      """
    end
    :ok
  end

  defp load_protocol(file) do
    case file do
      "Elixir." <> _ ->
        module = file |> Path.rootname |> String.to_atom
        :code.purge(module)
        :code.delete(module)
      _ ->
        :ok
    end
  end
end
