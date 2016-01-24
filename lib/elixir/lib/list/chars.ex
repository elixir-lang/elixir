defprotocol List.Chars do
  @moduledoc ~S"""
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
  def to_char_list(atom), do: Atom.to_char_list(atom)
end

defimpl List.Chars, for: BitString do
  @doc """
  Returns the given binary converted to a char list.
  """
  def to_char_list(thing) when is_binary(thing) do
    String.to_char_list(thing)
  end

  def to_char_list(thing) do
    raise Protocol.UndefinedError,
             protocol: @protocol,
                value: thing,
          description: "cannot convert a bitstring to a char list"
  end
end

defimpl List.Chars, for: List do
  # Note that same inlining is used for the rewrite rule.
  def to_char_list(list), do: list
end

defimpl List.Chars, for: Integer do
  def to_char_list(thing) do
    Integer.to_char_list(thing)
  end
end

defimpl List.Chars, for: Float do
  @digits 20
  @limit  :math.pow(10, @digits)

  def to_char_list(thing) when thing > @limit do
    Float.to_char_list(thing, scientific: @digits)
  end

  def to_char_list(thing) do
    Float.to_char_list(thing, compact: true, decimals: @digits)
  end
end
