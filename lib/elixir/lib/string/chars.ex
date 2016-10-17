import Kernel, except: [to_string: 1]

defprotocol String.Chars do
  @moduledoc ~S"""
  The `String.Chars` protocol is responsible for
  converting a structure to a binary (only if applicable).
  The only function required to be implemented is
  `to_string` which does the conversion.

  The `to_string/1` function automatically imported
  by `Kernel` invokes this protocol. String
  interpolation also invokes `to_string` in its
  arguments. For example, `"foo#{bar}"` is the same
  as `"foo" <> to_string(bar)`.
  """

  def to_string(term)
end

defimpl String.Chars, for: Atom do
  def to_string(nil) do
    ""
  end

  def to_string(atom) do
    Atom.to_string(atom)
  end
end

defimpl String.Chars, for: BitString do
  def to_string(term) when is_binary(term) do
    term
  end

  def to_string(term) do
    raise Protocol.UndefinedError,
             protocol: @protocol,
                value: term,
          description: "cannot convert a bitstring to a string"
  end
end

defimpl String.Chars, for: List do
  def to_string(charlist), do: List.to_string(charlist)
end

defimpl String.Chars, for: Integer do
  def to_string(term) do
    Integer.to_string(term)
  end
end

defimpl String.Chars, for: Float do
  def to_string(term) do
    IO.iodata_to_binary(:io_lib_format.fwrite_g(term))
  end
end
