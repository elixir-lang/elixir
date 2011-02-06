object Atom
  % Returns a version of this atom that is a representation of itself.
  %
  % ## Examples
  %
  %     'a.inspect % => "'a"
  %     'A.inspect % => "'A"
  %
  def inspect
    String.new <<$', Erlang.atom_to_binary(self, 'utf8)|binary>>
  end

  % Convert an atom to a char list.
  %
  % ## Examples
  %
  %     'a.to_s % => [97]
  %     'A.to_s % => [65]
  %
  def to_char_list
    Erlang.atom_to_list(self)
  end

  % Convert an atom to a string.
  %
  % ## Examples
  %
  %     'a.to_s % => "a"
  %     'A.to_s % => "A"
  %
  def to_s
    String.new to_bin
  end

  % Converts the given atom to binary.
  %
  % ## Examples
  %
  %     'a.to_s % => <<"a">>
  %     'A.to_s % => <<"A">>
  %
  def to_bin
    Erlang.atom_to_binary(self, 'utf8)
  end
end