defmodule Mix.Escript do
  @moduledoc """
  Module to keep helper functions for [escripts](http://www.erlang.org/doc/man/escript.html).
  """

  @doc """
  The name that will be used for the resulting escript file based on the project configuration.
  """
  @spec escript_name(Keyword.t) :: String.t
  def escript_name(project) do
    case get_in(project, [:escript, :name]) do
      nil -> project[:app]
      name -> name
    end |> to_string()
  end
end
