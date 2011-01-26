object Atom
  def to_s
    String.new Erlang.atom_to_list(self)
  end
end