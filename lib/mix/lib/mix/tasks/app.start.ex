defmodule Mix.Tasks.App.Start do
  use Mix.Task

  @shortdoc "Start all registered apps"

  @moduledoc """
  Starts all registered apps.

  The application is started by default as temporary. In case
  `:start_permanent` is set to true in your project configuration
  or the `--permanent` flag is given, it is started as permanent,
  which guarantee the node will shutdown in case the application
  crashes permanently.

  ## Command line options

    * `--force` - force compilation regardless of compilation times
    * `--temporary` - start the application as temporary
    * `--permanent` - start the application as permanent
    * `--no-compile` - do not compile even if files require compilation
    * `--no-protocols` - do not load consolidated protocols
    * `--no-deps-check` - do not check dependencies
    * `--no-elixir-version-check` - do not check elixir version
    * `--no-start` - do not start applications after compilation

  """
  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    Mix.Project.get!

    {opts, _, _} = OptionParser.parse args, switches: [permanent: :boolean, temporary: :boolean]
    Mix.Task.run "loadpaths", args

    unless "--no-compile" in args do
      Mix.Project.compile(args)
    end

    unless "--no-protocols" in args do
      consolidated = Path.join(Mix.Project.build_path, "consolidated")

      if File.dir?(consolidated) do
        Code.prepend_path(consolidated)

        Enum.each(File.ls!(consolidated), fn file ->
          module = file |> Path.rootname() |> String.to_atom()
          :code.purge(module)
          :code.delete(module)
        end)
      else
        consolidated = nil
      end
    end

    # Stop the Logger when starting the application as it is
    # up to the application to decide if it should be restarted
    # or not.
    #
    # Mix should not depend directly on Logger so check that it's loaded.
    logger = Process.whereis(Logger)
    if logger do
      Logger.App.stop
    end

    if "--no-start" in args do
      # Start the Logger again if the application won't be starting it
      if logger do
        :ok = Logger.App.start
      end
    else
      start(Mix.Project.config, [consolidated: consolidated] ++ opts)
    end

    :ok
  end

  @doc false
  def start(config, opts) do
    apps =
      cond do
        Mix.Project.umbrella?(config) ->
          for %Mix.Dep{app: app} <- Mix.Dep.Umbrella.loaded, do: app
        app = config[:app] ->
          [app]
        true ->
          []
      end

    if config[:build_embedded] do
      ensure_all_loaded([:erts] ++ apps)
      put_code_paths(opts)
    end
    type = type(config, opts)
    Enum.each apps, &ensure_all_started(&1, type)
    if config[:build_embedded], do: check_configured()
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

  defp ensure_all_loaded(apps, loaded \\ [])

  defp ensure_all_loaded([], _), do: :ok
  defp ensure_all_loaded([app | apps], loaded) do
    case ensure_loaded(app) do
      :ok ->
        deps = app_deps(app)
        loaded = [app | loaded]
        apps = (deps -- loaded) ++ apps
        ensure_all_loaded(apps, loaded)
      {:error, reason} ->
        _ = for app <- loaded, do: :application.unload(app)
        Mix.raise "Could not load application #{app}: " <>
          Application.format_error(reason)
    end
  end

  defp ensure_loaded(app) do
    case :application.load(app) do
      :ok                               -> :ok
      {:error, {:already_loaded, ^app}} -> :ok
      {:error, _} = error               -> error
    end
  end

  defp app_deps(app) do
    app_key(app, :included_applications) ++ app_key(app, :applications)
  end

  defp app_key(app, key) do
    case :application.get_key(app, key) do
      {:ok, val} -> val
      :undefined -> []
    end
  end

  defp put_code_paths(opts) do
    apps = for {app, _, _} <- :application.loaded_applications(), do: app
    ebins = app_ebins(apps)
    put_code_paths(ebins, opts)
  end

  defp app_ebins(apps) do
    for app <- apps do
      case :code.lib_dir(app, :ebin) do
        ebin when is_list(ebin) ->
          ebin
        {:error, reason} ->
          Mix.raise "Could not find ebin directory for application #{app}: #{reason}"
      end
    end
  end

  defp put_code_paths(paths, opts) do
    _ = for path <- :code.get_path(), not (path in paths), do: :code.del_path(path)
    consolidated = opts[:consolidated]
    if consolidated, do: Code.append_path(consolidated)
    :ok
  end

  defp check_configured() do
    started = for {app, _, _} <- :application.which_applications(), do: app
    apps = Enum.flat_map(started, fn(app) ->
      {:ok, included} = :application.get_key(app, :included_applications)
      [app | included]
    end)
    configured = Mix.State.get(:configured_application, [])
    _ = for app <- configured -- apps do
      Mix.shell.info """
      You have configured application #{inspect app} but it was not started.
      This usually means one of:

      1. You depend on application #{inspect app} but you (or a dependency)
      haven't listed it in :applications (or :included_applications) in the
      mix.exs file.

      2. You are configuring an application that does not really exist.
      Please ensure it exists or remove the configuration.
      """
    end
    :ok
  end
end
