defmodule Mix.Tasks.Loadconfig do
  use Mix.Task

  @shortdoc "Loads and persists the given configuration"

  @moduledoc """
  Loads and persists the given configuration.

      mix loadconfig path/to/config.exs

  Any configuration file loaded with `loadconfig` is treated
  as a compile-time configuration.

  Note that "config/config.exs" is always loaded automatically
  by the Mix CLI when it boots. "config/runtime.exs" is loaded
  automatically by `mix app.config` before starting the current
  application. Therefore there is no need to load those config
  files directly.

  This task is automatically reenabled, so it can be called
  multiple times to load different configs.
  """

  @impl true
  def run(args) do
    Mix.Task.reenable("loadconfig")

    case args do
      [] -> load_default()
      [file] -> load_compile(file)
    end
  end

  defp load_default do
    config = Mix.Project.config()

    if File.regular?(config[:config_path]) or config[:config_path] != "config/config.exs" do
      load_compile(config[:config_path])
    else
      []
    end
  end

  @doc false
  # Loads compile-time configuration, they support imports, and are not deep merged.
  def load_compile(file) do
    {config, files} = Config.Reader.read_imports!(file, env: Mix.env(), target: Mix.target())
    Mix.ProjectStack.loaded_config(persist_apps(config, file), files)
    config
  end

  @doc false
  # Loads runtime configuration, they do not support imports, and are deep merged.
  def load_runtime(file) do
    config = Config.Reader.read!(file, env: Mix.env(), target: Mix.target(), imports: :disabled)
    Mix.ProjectStack.loaded_config(persist_apps(hydrate_apps(config), file), [])
    config
  end

  defp hydrate_apps(config) do
    for {app, pairs} <- config do
      hd(Config.Reader.merge([{app, Application.get_all_env(app)}], [{app, pairs}]))
    end
  end

  defp persist_apps(config, file) do
    Application.put_all_env(config, persistent: true)
    apps = Keyword.keys(config)

    case Enum.filter(config, &check_app/1) do
      [] ->
        :ok

      reserved_apps ->
        Mix.shell().error("""
        Cannot configure base applications: #{inspect(Keyword.keys(reserved_apps))}

        These applications are already started by the time the configuration
        executes and these configurations have no effect.

        If you want to configure these applications for a release, you can
        specify them in your vm.args file:

            -kernel config_key config_value

        Alternatively, if you must configure them dynamically, you can wrap
        them in a conditional block in your config files:

            if System.get_env("RELEASE_MODE") do
              config :kernel, ...
            end

        and then configure your releases to reboot after configuration:

            releases: [
              my_app: [reboot_system_after_config: true]
            ]

        This happened when loading #{Path.relative_to_cwd(file)} or
        one of its imports.
        """)
    end

    apps
  end

  defp check_app({:kernel, opts}) do
    {logger_opts, rest} = Keyword.split(opts, [:logger, :logger_level])

    case Keyword.fetch(logger_opts, :logger_level) do
      {:ok, level} -> :logger.update_primary_config(%{level: level})
      _ -> :ok
    end

    setup_logger(Keyword.get(logger_opts, :logger, []))

    rest != []
  end

  defp check_app({:stdlib, _}), do: true
  defp check_app(_app_config), do: false

  defp setup_logger(entries) when is_list(entries), do: Enum.each(entries, &install/1)

  defp install({:handler, :default, :undefined}) do
    :logger.remove_handler(:default)
  end

  defp install({:handler, handler_id, module, handler_config}) do
    if handler_id == :default, do: :logger.remove_handler(handler_id)

    :logger.add_handler(handler_id, module, handler_config)
  end

  defp install({:filters, filter_default, filters}) do
    for {id, filter} <- filters, do: :logger.add_primary_filter(id, filter)

    :logger.update_primary_config(%{filter_default: filter_default})
  end

  defp install({:module_level, level, modules}) do
    for module <- modules, do: :logger.set_module_level(module, level)
  end
end
