% elixir: cache

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

  % Converts the given atom to a constant with the same name.
  %
  % ## Examples
  %
  %     'Atom.to_constant % => Atom
  %
  def to_constant
    Erlang.elixir_constants.lookup(self)
  end

  % A convenience method that returns a tuple representing a method.
  % The syntax was borrowed from Erlang.
  %
  % ## Examples
  %
  %     'a/2  % => { 'a, 2 }
  %
  def /(arity)
    {self, arity}
  end

  private

  def inspect(true)
    "true"
  end

  def inspect(false)
    "false"
  end

  def inspect(other)
    bin = Erlang.atom_to_binary(other, 'utf8)

    if ~r"\A@?\w*[?!]?\z".match?(bin)
      String.new <<$', bin|binary>>
    else
      String.new <<$', $\", bin|binary, $\">>
    end
  end
end