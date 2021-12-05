defmodule Atom do
  @moduledoc """
  Atoms are constants whose values are their own name.

  They are often useful to enumerate over distinct values, such as:

      iex> :apple
      :apple
      iex> :orange
      :orange
      iex> :watermelon
      :watermelon

  Atoms are equal if their names are equal.

      iex> :apple == :apple
      true
      iex> :apple == :orange
      false

  Often they are used to express the state of an operation, by using
  values such as `:ok` and `:error`.

  The booleans `true` and `false` are also atoms:

      iex> true == :true
      true
      iex> is_atom(false)
      true
      iex> is_boolean(:false)
      true

  Elixir allows you to skip the leading `:` for the atoms `false`, `true`,
  and `nil`.

  Atoms must be composed of Unicode characters such as letters, numbers,
  underscore, and `@`. If the keyword has a character that does not
  belong to the category above, such as spaces, you can wrap it in
  quotes:

      iex> :"this is an atom with spaces"
      :"this is an atom with spaces"

  """

  @doc """
  Converts an atom to a string.

  Inlined by the compiler.

  ## Examples

      iex> Atom.to_string(:foo)
      "foo"

  """
  @spec to_string(atom) :: String.t()
  def to_string(atom) do
    :erlang.atom_to_binary(atom, :utf8)
  end

  @doc """
  Converts an atom to a charlist.

  Inlined by the compiler.

  ## Examples

      iex> Atom.to_charlist(:"An atom")
      'An atom'

  """
  @spec to_charlist(atom) :: charlist
  def to_charlist(atom) do
    :erlang.atom_to_list(atom)
  end

  @doc """
  Classifies the given `atom`.

  It returns one of the following atoms:

    * `:alias` - the atom represents an alias

    * `:identifier` - the atom can be used as a variable or local function call

    * `:unquoted` - the atom can be used in its unquoted form,
      includes operators and atoms with `@` in them

    * `:quoted` - all other atoms which can only be used in their quoted form

  ## Examples

      iex> Atom.classify(:foo)
      :identifier
      iex> Atom.classify(Foo)
      :alias
      iex> Atom.classify(:foo@bar)
      :unquoted
      iex> Atom.classify(:+)
      :unquoted
      iex> Atom.classify(:Foo)
      :unquoted
      iex> Atom.classify(:"with spaces")
      :quoted

  """
  @doc since: "1.14.0"
  @spec classify(atom) :: :alias | :identifier | :quoted | :unquoted
  def classify(atom) do
    case inner_classify(atom) do
      :alias -> :alias
      :identifier -> :identifier
      type when type in [:unquoted_operator, :not_callable] -> :unquoted
      _ -> :quoted
    end
  end

  @doc ~S"""
  Inspects the given atom.

  This is equivalent to calling `inspect(atom)`, but provided
  for completeness.

  ## Examples

      iex> Atom.inspect(nil)
      "nil"
      iex> Atom.inspect(:foo)
      ":foo"
      iex> Atom.inspect(:<>)
      ":<>"
      iex> Atom.inspect(:Foo)
      ":Foo"
      iex> Atom.inspect(:"with spaces")
      ":\"with spaces\""

  """
  @doc since: "1.14.0"
  @spec inspect(atom) :: binary
  def inspect(atom) when is_nil(atom) or is_boolean(atom) do
    Atom.to_string(atom)
  end

  def inspect(atom) when is_atom(atom) do
    binary = Atom.to_string(atom)

    case classify(atom) do
      :alias ->
        case binary do
          binary when binary in ["Elixir", "Elixir.Elixir"] -> binary
          "Elixir.Elixir." <> _rest -> binary
          "Elixir." <> rest -> rest
        end

      :quoted ->
        {escaped, _} = Code.Identifier.escape(binary, ?")
        IO.iodata_to_binary([?:, ?", escaped, ?"])

      _ ->
        ":" <> binary
    end
  end

  @doc ~S"""
  Inspects the given atom as a key in a keyword list.

  This function is when inspecting keyword lists.

  ## Examples

      iex> Atom.inspect_as_key(:foo)
      "foo:"
      iex> Atom.inspect_as_key(:<>)
      "<>:"
      iex> Atom.inspect_as_key(:Foo)
      "Foo:"
      iex> Atom.inspect_as_key(:"with spaces")
      "\"with spaces\":"

  """
  @doc since: "1.14.0"
  @spec inspect_as_key(atom) :: binary
  def inspect_as_key(atom) when is_atom(atom) do
    binary = Atom.to_string(atom)

    case classify(atom) do
      :alias ->
        IO.iodata_to_binary([?", binary, ?", ?:])

      :quoted ->
        {escaped, _} = Code.Identifier.escape(binary, ?")
        IO.iodata_to_binary([?", escaped, ?", ?:])

      _ ->
        IO.iodata_to_binary([binary, ?:])
    end
  end

  @doc ~S"""
  Inspects the given atom as a function name in a function call.

  This function is typically used when converting code to string
  and you want to accurately represent a function call.

  ## Examples

      iex> Atom.inspect_as_function(:foo)
      "foo"
      iex> Atom.inspect_as_function(:<>)
      "<>"
      iex> Atom.inspect_as_function(:Foo)
      "\"Foo\""
      iex> Atom.inspect_as_function(:"with spaces")
      "\"with spaces\""

  """
  @doc since: "1.14.0"
  @spec inspect_as_function(atom) :: binary
  def inspect_as_function(atom) when is_atom(atom) do
    binary = Atom.to_string(atom)

    case inner_classify(atom) do
      type when type in [:identifier, :unquoted_operator, :quoted_operator] ->
        binary

      type ->
        escaped =
          if type in [:not_callable, :alias] do
            binary
          else
            elem(Code.Identifier.escape(binary, ?"), 0)
          end

        IO.iodata_to_binary([?", escaped, ?"])
    end
  end

  # Classifies the given atom into one of the following categories:
  #
  #   * `:alias` - a valid Elixir alias, like `Foo`, `Foo.Bar` and so on
  #
  #   * `:identifier` - an atom that can be used as a variable/local call;
  #     this category includes identifiers like `:foo`
  #
  #   * `:unquoted_operator` - all callable operators, such as `:<>`. Note
  #     operators such as `:..` are not callable because of ambiguity
  #
  #   * `:quoted_operator` - callable operators that must be wrapped in quotes when
  #     defined as an atom. For example, `::` must be written as `:"::"` to avoid
  #     the ambiguity between the atom and the keyword identifier
  #
  #   * `:not_callable` - an atom that cannot be used as a function call after the
  #     `.` operator. Those are typically AST nodes that are special forms (such as
  #     `:%{}` and `:<<>>>`) as well as nodes that are ambiguous in calls (such as
  #     `:..` and `:...`). This category also includes atoms like `:Foo`, since
  #     they are valid identifiers but they need quotes to be used in function
  #     calls (`Foo."Bar"`)
  #
  #   * `:other` - any other atom (these are usually escaped when inspected, like
  #     `:"foo and bar"`)
  #
  defp inner_classify(atom) when is_atom(atom) do
    cond do
      atom in [:%, :%{}, :{}, :<<>>, :..., :.., :., :"..//", :->] ->
        :not_callable

      atom in [:"::"] ->
        :quoted_operator

      # Code.Identifier treats :// as a binary operator for precedence
      # purposes but it isn't really one, so we explicitly skip it.
      Code.Identifier.unary_op(atom) != :error or
          (Code.Identifier.binary_op(atom) != :error and atom != :"//") ->
        :unquoted_operator

      true ->
        charlist = Atom.to_charlist(atom)

        if valid_alias?(charlist) do
          :alias
        else
          case :elixir_config.identifier_tokenizer().tokenize(charlist) do
            {kind, _acc, [], _, _, special} ->
              if kind == :identifier and not :lists.member(?@, special) do
                :identifier
              else
                :not_callable
              end

            _ ->
              :other
          end
        end
    end
  end

  defp valid_alias?('Elixir' ++ rest), do: valid_alias_piece?(rest)
  defp valid_alias?(_other), do: false

  defp valid_alias_piece?([?., char | rest]) when char >= ?A and char <= ?Z,
    do: valid_alias_piece?(trim_leading_while_valid_identifier(rest))

  defp valid_alias_piece?([]), do: true
  defp valid_alias_piece?(_other), do: false

  defp trim_leading_while_valid_identifier([char | rest])
       when char >= ?a and char <= ?z
       when char >= ?A and char <= ?Z
       when char >= ?0 and char <= ?9
       when char == ?_ do
    trim_leading_while_valid_identifier(rest)
  end

  defp trim_leading_while_valid_identifier(other) do
    other
  end

  @doc false
  @deprecated "Use Atom.to_charlist/1 instead"
  @spec to_char_list(atom) :: charlist
  def to_char_list(atom), do: Atom.to_charlist(atom)
end
