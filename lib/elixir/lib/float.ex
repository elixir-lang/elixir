defmodule Float do
  @moduledoc """
  Functions for working with floats.
  """

  @doc """
  Parses a binary into a float. If successful, returns a
  tuple of the form `{ float, remainder_of_binary }`.
  Otherwise `:error`.

  ## Examples

      iex> Float.parse("34")
      {34.0,""}
      iex> Float.parse("34.25")
      {34.25,""}
      iex> Float.parse("56.5xyz")
      {56.5,"xyz"}
      iex> Float.parse("pi")
      :error

  """
  @spec parse(binary) :: { float, binary } | :error
  def parse(binary) do
    list = :binary.bin_to_list(binary)

    case :string.to_float(list) do
      { :error, _ } ->
        case :string.to_integer(list) do
          { :error, _ } ->
            :error
          { result, remainder } ->
            { :erlang.float(result), :binary.list_to_bin(remainder) }
        end
      { result, remainder } ->
        { result, :binary.list_to_bin(remainder) }
    end
  end
end
