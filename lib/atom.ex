module Atom
  % Returns true if an atom representation of the given string exists.
  def exists?(string)
    try
      Erlang.binary_to_existing_atom(string.to_bin, 'utf8)
      true
    catch 'error: 'badarg
      false
    end
  end

  def from_char_list(list)
    Erlang.list_to_atom(list)
  end

  module Instance
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
      Erlang.atom_to_binary(self, 'utf8)
    end

    % Converts the given atom to a constant with the same name.
    % Raises a no_constant error if the constant does not exist.
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

    % Sends a message to the given that represents an registered process.
    % This is the equivalent to Erlang's ! and is also aliased as `deliver`.
    def <-(message)
      Erlang.send(self, message)
    end
    alias_local '<-, 'dispatch, 1

    private

    def inspect(nil)
      "nil"
    end

    def inspect(true)
      "true"
    end

    def inspect(false)
      "false"
    end

    def inspect(other)
      bin = Erlang.atom_to_binary(other, 'utf8)

      if ~r"\A@?(\w|::)*[?!]?\z".match?(bin)
        <<$', bin|binary>>
      else
        <<$', $\", bin|binary, $\">>
      end
    end
  end
end