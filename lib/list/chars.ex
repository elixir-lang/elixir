defprotocol List::Chars, [to_char_list(thing)],
  only: [BitString, List, Atom, Number, Tuple]

defimpl List::Chars, for: Atom do
  def to_char_list(atom), do: atom_to_list(atom)
end

defimpl List::Chars, for: BitString do
  def to_char_list(bitstring), do: bitstring_to_list(bitstring)
end

defimpl List::Chars, for: List do
  def to_char_list(list), do: list
end

defimpl List::Chars, for: Number do
  def to_char_list(integer) when is_integer(integer), do: integer_to_list(integer)
  def to_char_list(float)   when is_float(float),     do: float_to_list(float)
end