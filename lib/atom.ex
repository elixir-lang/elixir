defimpl Inspect, for: Atom do
  def inspect(atom) do
    ":#{atom_to_binary(atom, :utf8)}"
  end
end