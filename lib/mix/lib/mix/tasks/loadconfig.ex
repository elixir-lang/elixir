defmodule Mix.Tasks.Loadconfig do
  use Mix.Task

  @shortdoc "Loads and persists the given configuration"

  @moduledoc """
  Loads and persists the given configuration.

  In case no configuration file is given, it
  loads the project one at "config/config.exs".

  This task is automatically reenabled, so it
  can be called multiple times to load different
  configs.
  """
  def run(args) do
    cond do
      file = Enum.at(args, 0) ->
        load file
      File.regular?("config/config.exs") ->
        load "config/config.exs"
      true ->
        :ok
    end

    Mix.Task.reenable "loadconfig"
  end

  defp load(file) do
    opts = Mix.Config.read!(file)
    Mix.Config.persist(opts)

    # In case the Logger was changed, we reload the whole
    # Logger. This is required because many of Logger config
    # reflects on its supervision tree. No other application
    # that is bundled with Elixir requires such reloading.
    if opts[:logger], do: Logger.Config.restart

    :ok
  end
end
