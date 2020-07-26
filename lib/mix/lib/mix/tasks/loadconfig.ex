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

  @reserved_apps [:stdlib, :kernel]

  @impl true
  def run(args) do
    Mix.Task.reenable("loadconfig")

    case args do
      [] -> load_default()
      [file] -> load_imports(file)
    end
  end

  @doc false
  def load_default do
    config = Mix.Project.config()

    if File.regular?(config[:config_path]) or config[:config_path] != "config/config.exs" do
      load_imports(config[:config_path])
    else
      []
    end
  end

  @doc false
  def load_imports(file) do
    {config, files} = Config.Reader.read_imports!(file, env: Mix.env(), target: Mix.target())
    Mix.ProjectStack.loaded_config(persist_apps(config, file), files)
    config
  end

  @doc false
  def load_file(file) do
    config = Config.Reader.read!(file, env: Mix.env(), target: Mix.target(), imports: :disabled)
    Mix.ProjectStack.loaded_config(persist_apps(config, file), [])
    config
  end

  defp persist_apps(config, file) do
    Application.put_all_env(config, persistent: true)
    apps = Keyword.keys(config)

    case Enum.filter(@reserved_apps, &(&1 in apps)) do
      [] ->
        :ok

      reserved_apps ->
        Mix.shell().error("""
        Cannot configure base applications: #{inspect(reserved_apps)}

        These applications are already started by the time Mix loads and
        therefore these configurations have no effect.

        If you want to configure these applications for a release, wrap
        them in a condition, such as:

            if System.get_env("RELEASE_MODE") do
              config :kernel, ...
            end

        Alternatively, specify the configuration in your vm.args file:

            -kernel config_key config_value

        This happened when loading #{Path.relative_to_cwd(file)} or
        one of its imports.
        """)
    end

    apps
  end
end
