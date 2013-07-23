import Kernel, except: [to_binary: 1]

defprotocol Binary.Chars do
  @moduledoc %B"""
  The Binary.Chars protocol is responsible for
  converting a structure to a Binary (only if applicable).
  The only function required to be implemented is
  `to_binary` which does the conversion.

  The `to_binary` function automatically imported
  by Kernel invokes this protocol. String
  interpolation also invokes to_binary in its
  arguments. For example, `"foo#{bar}"` is the same
  as `"foo" <> to_binary(bar)`.
  """

  @only [BitString, List, Number, Atom, Record]

  def to_binary(thing)
end

defimpl Binary.Chars, for: Atom do
  @doc """
  Convert the atom literally to a binary, except
  `nil` which is converted to an empty string.
  """
  def to_binary(nil) do
    ""
  end

  def to_binary(atom) do
    atom_to_binary(atom, :utf8)
  end
end

defimpl Binary.Chars, for: BitString do
  @doc """
  Simply returns the binary itself.
  """
  def to_binary(thing) when is_binary(thing) do
    thing
  end
end

defimpl Binary.Chars, for: List do
  @doc """
  Consider the list is an iolist and converts it
  to a binary. This allows a list of binaries, or
  a charlist, or a mix of both, to be converted
  successfully.

  ## Examples

      iex> to_binary('foo')
      "foo"
      iex> to_binary(["foo", 'bar'])
      "foobar"

  """
  def to_binary(thing) do
    try do
      iolist_to_binary(thing)
    rescue
      ArgumentError ->
        raise Protocol.UndefinedError,
                 protocol: @protocol,
                    value: thing,
              description: "only iolists are supported"
    end
  end
end

defimpl Binary.Chars, for: Number do
  @doc """
  Simply converts the number (integer or a float) to a binary.
  """
  def to_binary(thing) when is_integer(thing) do
    integer_to_binary(thing)
  end

  def to_binary(thing) do
    list_to_binary(:io_lib_format.fwrite_g(thing))
  end
end
