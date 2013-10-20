defprotocol List.Chars do
  @moduledoc %S"""
  The List.Chars protocol is responsible for
  converting a structure to a list (only if applicable).
  The only function required to be implemented is
  `to_char_list` which does the conversion.

  The `to_char_list` function automatically imported
  by Kernel invokes this protocol.
  """

  def to_char_list(thing)
end

defimpl List.Chars, for: Atom do
  def to_char_list(atom), do: atom_to_list(atom)
end

defimpl List.Chars, for: BitString do
  @doc """
  Returns the given binary converted to a char list.
  """
  def to_char_list(thing) when is_binary(thing) do
    String.to_char_list!(thing)
  end

  def to_char_list(thing) do
    raise Protocol.UndefinedError,
             protocol: @protocol,
                value: thing,
          description: "cannot convert a bitstring to a char list"
  end
end

defimpl List.Chars, for: List do
  def to_char_list(list), do: list
end

defimpl List.Chars, for: Integer do
  def to_char_list(thing) do
    integer_to_list(thing)
  end
end

defimpl List.Chars, for: Float do
  @digits 20
  @limit  :math.pow(10, @digits)

  def to_char_list(thing) when thing > @limit do
    float_to_list(thing, scientific: @digits)
  end

  def to_char_list(thing) do
    float_to_list(thing, compact: true, decimals: @digits)
  end
end
