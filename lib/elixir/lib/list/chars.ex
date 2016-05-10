defprotocol List.Chars do
  @moduledoc ~S"""
  The List.Chars protocol is responsible for
  converting a structure to a list (only if applicable).
  The only function required to be implemented is
  `to_charlist` which does the conversion.

  The `to_charlist` function automatically imported
  by Kernel invokes this protocol.
  """

  def to_charlist(thing)

  # TODO: Deprecate by v1.5
  @doc false
  Kernel.def to_char_list(thing) do
    __MODULE__.to_charlist(thing)
  end
end

defimpl List.Chars, for: Atom do
  def to_charlist(atom), do: Atom.to_charlist(atom)
end

defimpl List.Chars, for: BitString do
  @doc """
  Returns the given binary converted to a charlist.
  """
  def to_charlist(thing) when is_binary(thing) do
    String.to_charlist(thing)
  end

  def to_charlist(thing) do
    raise Protocol.UndefinedError,
             protocol: @protocol,
                value: thing,
          description: "cannot convert a bitstring to a charlist"
  end
end

defimpl List.Chars, for: List do
  # Note that same inlining is used for the rewrite rule.
  def to_charlist(list), do: list
end

defimpl List.Chars, for: Integer do
  def to_charlist(thing) do
    Integer.to_charlist(thing)
  end
end

defimpl List.Chars, for: Float do
  @digits 20
  @limit  :math.pow(10, @digits)

  def to_charlist(thing) when thing > @limit do
    Float.to_charlist(thing, scientific: @digits)
  end

  def to_charlist(thing) do
    Float.to_charlist(thing, compact: true, decimals: @digits)
  end
end

defimpl List.Chars, for: Range do
  def to_charlist(thing) do
    Range.to_charlist(thing)
  end
end
