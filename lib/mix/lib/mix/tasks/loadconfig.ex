defmodule Mix.Tasks.Loadconfig do
  use Mix.Task

  @moduledoc """
  Loads and persists the project configuration.
  """

  def run(_) do
    if File.regular?("config/config.exs") do
      Mix.Config.persist Mix.Config.read! "config/config.exs"
    end
  end
end
