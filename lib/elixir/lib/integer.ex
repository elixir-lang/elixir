defmodule Integer do
  @moduledoc """
  Functions for working with integers.
  """

  @doc """
  Returns true if `n` is an odd number, otherwise false.
  Implemented as a macro so it is allowed in guard clauses.
  """
  defmacro odd?(n) do
    quote do: rem(unquote(n), 2) != 0
  end

  @doc """
  Returns true if `n` is an even number, otherwise false.
  Implemented as a macro so it is allowed in guard clauses.
  """
  defmacro even?(n) do
    quote do: rem(unquote(n), 2) == 0
  end

  @doc """
  Converts a binary to an integer. If successful, returns a
  tuple of the form `{ integer, remainder_of_binary }`.
  Otherwise `:error`.

  ## Examples

      iex> Integer.parse("34")
      {34,""}
      iex> Integer.parse("34.5")
      {34,".5"}
      iex> Integer.parse("three")
      :error

  """
  @spec parse(binary) :: { integer, binary } | :error
  def parse(binary) do
    { result, remainder } = :string.to_integer(:binary.bin_to_list(binary))
    case result do
      :error -> :error
      _ -> { result, :binary.list_to_bin(remainder) }
    end
  end
end
