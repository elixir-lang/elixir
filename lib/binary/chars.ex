import Elixir.Builtin, except: [to_binary: 1]

defprotocol Binary.Chars, [to_binary(thing)],
  only: [BitString, List, Number, Atom, Record]

defimpl Binary.Chars, for: Atom do
  def to_binary(nil) do
    ""
  end

  def to_binary(atom) do
    atom_to_binary(atom, :utf8)
  end
end

defimpl Binary.Chars, for: BitString do
  def to_binary(thing) when is_binary(thing) do
    thing
  end
end

defimpl Binary.Chars, for: List do
  def to_binary(thing) do
    iolist_to_binary(thing)
  end
end

defimpl Binary.Chars, for: Number do
  def to_binary(thing) when is_integer(thing) do
    list_to_binary integer_to_list(thing)
  end

  def to_binary(thing) do
    list_to_binary float_to_list(thing)
  end
end