defmodule Mix do
  @doc """
  Starts the mix application and its dependencies.
  """
  def start do
    Enum.each [:elixir, :mix], :application.start(&1)
    Mix.Task.start
    Mix.Project.start
  end
end