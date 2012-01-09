defprotocol List::Inspect, [to_list(thing)],
  only: [BitString, List, Tuple, Atom]

defimpl List::Inspect, for: Atom do
  def to_list(atom), do: atom_to_list(atom)
end

defimpl List::Inspect, for: BitString do
  def to_list(bitstring), do: bitstring_to_list(bitstring)
end

defimpl List::Inspect, for: Tuple do
  def to_list(tuple), do: tuple_to_list(tuple)
end

defimpl List::Inspect, for: List do
  def to_list(list), do: list
end

defimpl List::Inspect, for: Number do
  def to_list(integer) when is_integer(integer), do: integer_to_list(integer)
  def to_list(float)   when is_float(float),     do: float_to_list(float)
end