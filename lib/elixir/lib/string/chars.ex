import Kernel, except: [to_string: 1]

defprotocol String.Chars do
  @moduledoc ~S"""
  The `String.Chars` protocol is responsible for
  converting a structure to a Binary (only if applicable).
  The only function required to be implemented is
  `to_string` which does the conversion.

  The `to_string` function automatically imported
  by Kernel invokes this protocol. String
  interpolation also invokes `to_string` in its
  arguments. For example, `"foo#{bar}"` is the same
  as `"foo" <> to_string(bar)`.
  """

  def to_string(thing)
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
  def to_string(char_list) do
    try do
       List.to_string(char_list)
    rescue
      ArgumentError ->
        raise ArgumentError, """
        cannot convert this list to a string.

        The only lists that can be directly converted into a string are so-called chardata lists. Those represent a string as a list of strings, integers representing Unicode codepoints, or nested such lists.

        The list you provided is not a valid chardata list.

        If you wanted a code-like string representation of the list, use `Kernel.inspect/2`. For example:

        "… \#{inspect([:a, :b])} …" # => "… [:a, :b] …"
        """
    end
  end
end

defimpl String.Chars, for: Integer do
  def to_string(thing) do
    Integer.to_string(thing)
  end
end

defimpl String.Chars, for: Float do
  def to_string(thing) do
    IO.iodata_to_binary(:io_lib_format.fwrite_g(thing))
  end
end
