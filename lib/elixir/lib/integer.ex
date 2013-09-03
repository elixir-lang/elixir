defmodule Integer do
  @moduledoc """
  Functions for working with integers.
  """

  @doc """
  Returns true if `n` is an odd number, otherwise false.
  """
  defmacro odd?(n) do
    quote do: rem(unquote(n), 2) != 0
  end

  @doc """
  Returns true if `n` is an even number, otherwise false.
  """
  defmacro even?(n) do
    quote do: rem(unquote(n), 2) == 0
  end
end
