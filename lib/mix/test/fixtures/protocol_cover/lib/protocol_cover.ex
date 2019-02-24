defprotocol ProtocolCover do
  def to_uppercase(string)
end

defimpl ProtocolCover, for: BitString do
  def to_uppercase(string), do: String.upcase(string)
end
