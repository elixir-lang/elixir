defmodule URI.Parser do
  @moduledoc """
  Defines the behavior for each URI.Parser.
  Check URI.HTTP for a possible implementation.
  """

  def behaviour_info(:callbacks) do
    [parse: 1,
     default_port: 0]
  end
end
