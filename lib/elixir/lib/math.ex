defmodule Math do
	@moduledoc """
  Module related with Mathematical operations.
  """

  @doc """
  Generates a random integer.

  ## Examples

      iex> Math.random
      0.7230402056221108

      iex> Math.random(3)
      2
  """
  def random() do
    :random.uniform()
	end

	def random(range) do
    :random.uniform(range)
	end
end