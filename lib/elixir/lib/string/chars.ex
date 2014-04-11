import Kernel, except: [to_string: 1]

defprotocol String.Chars do
  @moduledoc ~S"""
  The String.Chars protocol is responsible for
  converting a structure to a Binary (only if applicable).
  The only function required to be implemented is
  `to_string` which does the conversion.

  The `to_string` function automatically imported
  by Kernel invokes this protocol. String
  interpolation also invokes to_string in its
  arguments. For example, `"foo#{bar}"` is the same
  as `"foo" <> to_string(bar)`.
  """

  def to_string(thing)
end

defimpl String.Chars, for: Atom do
  @doc """
  Convert the atom literally to a binary, except
  `nil` which is converted to an empty string.
  """
  def to_string(nil) do
    ""
  end

  def to_string(atom) do
    atom_to_binary(atom)
  end
end

defimpl String.Chars, for: BitString do
  @doc """
  Returns the given binary or raises an error for bitstrings.
  """
  def to_string(thing) when is_binary(thing) do
    thing
  end

  def to_string(thing) do
    raise Protocol.UndefinedError,
             protocol: @protocol,
                value: thing,
          description: "cannot convert a bitstring to a string"
  end
end

defimpl String.Chars, for: List do
  @doc """
  Consider the list is an iolist and converts it
  to a binary. This allows a list of binaries, or
  a charlist, or a mix of both, to be converted
  successfully.

  ## Examples

      iex> to_string('foo')
      "foo"

      iex> to_string(["foo", 'bar'])
      "foobar"

  """
  def to_string(char_list), do: String.from_char_list!(char_list)
end

defimpl String.Chars, for: Integer do
  @doc """
  Simply converts the integer to a string.
  """
  def to_string(thing) do
    integer_to_binary(thing)
  end
end

defimpl String.Chars, for: Float do
  @doc """
  Simply converts the float to a string.
  """
  def to_string(thing) do
    iolist_to_binary(:io_lib_format.fwrite_g(thing))
  end
end
