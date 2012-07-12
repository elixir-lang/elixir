defmodule Mix.Project do
  @doc """
  A simple module that provides conveniences for creating projects.
  """
  defmacro __using__(_) do
    Mix.push_project __CALLER__.module
  end
end