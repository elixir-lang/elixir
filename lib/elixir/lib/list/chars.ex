defprotocol List.Chars do
  @moduledoc ~S"""
  The `List.Chars` protocol is responsible for
  converting a structure to a charlist (only if applicable).

  The only function required to be implemented is
  `to_charlist/1` which does the conversion.

  The `to_charlist/1` function automatically imported
  by `Kernel` invokes this protocol.
  """

  @doc """
  Converts `term` to a charlist.
  """
  @spec to_charlist(t) :: charlist
  def to_charlist(term)

  @doc false
  @deprecated "Use List.Chars.to_charlist/1 instead"
  Kernel.def to_char_list(term) do
    __MODULE__.to_charlist(term)
  end
end

defimpl List.Chars, for: Atom do
  def to_charlist(atom), do: Atom.to_charlist(atom)
end

defimpl List.Chars, for: BitString do
  @doc """
  Returns the given binary `term` converted to a charlist.
  """
  def to_charlist(term) when is_binary(term) do
    String.to_charlist(term)
  end

  def to_charlist(term) do
    raise Protocol.UndefinedError,
      protocol: @protocol,
      value: term,
      description: "cannot convert a bitstring to a charlist"
  end
end

defimpl List.Chars, for: List do
  # Note that same inlining is used for the rewrite rule.
  def to_charlist(list), do: list
end

defimpl List.Chars, for: Integer do
  def to_charlist(term) do
    Integer.to_charlist(term)
  end
end

defimpl List.Chars, for: Float do
  def to_charlist(term) do
    :io_lib_format.fwrite_g(term)
  end
end
