object Atom
  % Returns a version of this atom that is a representation of itself.
  %
  % ## Examples
  %
  %     'a.inspect % => "'a"
  %     'A.inspect % => "'A"
  %
  def inspect
    inspect(self)
  end

  % Convert an atom to a char list.
  %
  % ## Examples
  %
  %     'a.to_char_list % => [97]
  %     'A.to_char_list % => [65]
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

  private

  def inspect(true)
    "true"
  end

  def inspect(false)
    "false"
  end

  def inspect(other)
    String.new <<$', Erlang.atom_to_binary(other, 'utf8)|binary>>
  end
end