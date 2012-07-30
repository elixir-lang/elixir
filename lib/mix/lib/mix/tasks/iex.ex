defmodule Mix.Tasks.Iex do
  use Mix.Task

  @shortdoc "Start IEx with your project's settings"

  @moduledoc """
  Starts an iex repl with your project settings.

  Elixir ensures your code is compiled and loaded
  before starting IEx.
  """
  def run(argv) do
    Mix.Task.run "compile"
    { opts, _ } = OptionParser.parse(argv)
    IEx.run(opts)
    :timer.sleep(:infinity)
  end
end
