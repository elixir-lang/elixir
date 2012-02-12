import ::Elixir::Builtin, except: [to_binary: 1]

defprotocol String::Chars, [to_binary(thing)],
  only: [BitString, List, Number, Atom, Tuple]

defimpl String::Chars, for: Atom do
  def to_binary(atom) do
    atom_to_binary(atom, :utf8)
  end
end

defimpl String::Chars, for: BitString do
  def to_binary(thing) when is_binary(thing) do
    thing
  end
end

defimpl String::Chars, for: List do
  def to_binary(thing) do
    iolist_to_binary(thing)
  end
end

defimpl String::Chars, for: Number do
  def to_binary(thing) when is_integer(thing) do
    list_to_binary integer_to_list(thing)
  end

  def to_binary(thing) do
    list_to_binary float_to_list(thing)
  end
end