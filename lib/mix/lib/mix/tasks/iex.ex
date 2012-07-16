defmodule Mix.Tasks.Iex do
  use Mix.Task

  @shortdoc "Start IEx with your project's settings"

  @moduledoc """
  Starts an iex repl with your project settings.

  Elixir ensures your code is compiled and loaded
  before starting IEx.
  """
  def run(_) do
    Enum.each ["compile", "loadpaths"], Mix.Task.run(&1)
    IEx.start
    :timer.sleep(:infinity)
  end
end
