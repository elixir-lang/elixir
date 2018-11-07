defmodule Mix.Tasks.Loadconfig do
  use Mix.Task

  @shortdoc "Loads and persists the given configuration"

  @moduledoc """
  Loads and persists the given configuration.

  If no configuration file is given, it loads the project's
  configuration file, "config/config.exs", if it exists. Keep in mind that
  the "config/config.exs" file is always loaded by the CLI and
  invoking it is only required in cases you are starting Mix
  manually.

  This task is automatically reenabled, so it can be called
  multiple times to load different configs.
  """

  @impl true
  def run(args) do
    config = Mix.Project.config()

    cond do
      file = Enum.at(args, 0) ->
        load(file)

      File.regular?(config[:config_path]) or config[:config_path] != "config/config.exs" ->
        load(config[:config_path])

      true ->
        :ok
    end

    Mix.Task.reenable("loadconfig")
  end

  defp load(file) do
    {config, files} = Mix.Config.eval!(file)
    apps = Mix.Config.persist(config)
    Mix.ProjectStack.loaded_config(apps, files)
    :ok
  end
end
