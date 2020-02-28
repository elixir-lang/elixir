defmodule Mix.Tasks.Loadconfig do
  use Mix.Task

  @shortdoc "Loads and persists the given configuration"

  @moduledoc """
  Loads and persists the given configuration.

  If no configuration file is given, it loads the project's
  configuration file, "config/config.exs", if it exists.
  Keep in mind that the "config/config.exs" file is always
  loaded by the CLI and invoking it is only required in cases
  you are starting Mix manually.

  This task is automatically reenabled, so it can be called
  multiple times to load different configs.
  """

  @impl true
  def run(args) do
    Mix.Task.reenable("loadconfig")
    load(args)
  end

  @doc false
  def load([]) do
    config = Mix.Project.config()

    if File.regular?(config[:config_path]) or config[:config_path] != "config/config.exs" do
      load([config[:config_path]])
    else
      []
    end
  end

  def load([file]) do
    {config, files} = Config.Reader.read_imports!(file)
    Application.put_all_env(config, persistent: true)
    Mix.ProjectStack.loaded_config(Keyword.keys(config), files)
    config
  end
end
