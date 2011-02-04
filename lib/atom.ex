object Atom
  % Returns a version of this atom that is a representation of itself.
  %
  % ## Examples
  %
  %     'a.inspect % => "'a"
  %     'A.inspect % => "'A"
  %
  % TODO Use bit syntax here
  def inspect
    String.new <<$', Erlang.atom_to_binary(self, 'utf8)|binary>>
  end

  % Convert an atom to a string.
  %
  % ## Examples
  %
  %     'a.to_s % => "a"
  %     'A.to_s % => "A"
  %
  def to_s
    String.new Erlang.atom_to_binary(self, 'utf8)
  end
end