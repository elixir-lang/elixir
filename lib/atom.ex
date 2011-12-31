defimpl Inspect, for: Atom do
  def inspect(atom) do
    bitstr ':', atom_to_binary(atom, :utf8) | :binary
  end

  def stringify(atom) do
    atom_to_binary(atom, :utf8)
  end
end