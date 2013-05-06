defprotocol List.Chars do
  @moduledoc %B"""
  The List.Chars protocol is responsible for
  converting a structure to a list (only if applicable).
  The only function required to be implemented is
  `to_char_list` which does the conversion.

  The `to_char_list` function automatically imported
  by Kernel invokes this protocol.
  """

  @only [BitString, List, Atom, Number, Record]

  def to_char_list(thing)
end

defimpl List.Chars, for: Atom do
  def to_char_list(atom), do: atom_to_list(atom)
end

defimpl List.Chars, for: BitString do
  def to_char_list(bitstring), do: bitstring_to_list(bitstring)
end

defimpl List.Chars, for: List do
  def to_char_list(list), do: list
end

defimpl List.Chars, for: Number do
  @digits 20
  @limit  :math.pow(10, @digits)

  def to_char_list(thing) when is_integer(thing) do
    :erlang.integer_to_list(thing)
  end

  def to_char_list(thing) when thing > @limit do
    :erlang.float_to_list(thing, [{ :scientific, @digits }])
  end

  def to_char_list(thing) do
    :erlang.float_to_list(thing, [:compact, { :decimals, @digits }])
  end
end