defmodule Code.Identifier do
  @moduledoc false

  @doc """
  Checks if the given identifier is an unary op.

  ## Examples

      iex> Code.Identifier.unary_op(:+)
      {:non_associative, 300}

  """
  @spec unary_op(atom) :: {:non_associative, precedence :: pos_integer} | :error
  def unary_op(op) do
    cond do
      op in [:&] -> {:non_associative, 90}
      op in [:!, :^, :not, :+, :-, :~~~] -> {:non_associative, 300}
      op in [:@] -> {:non_associative, 320}
      true -> :error
    end
  end

  @doc """
  Checks if the given identifier is a binary op.

  ## Examples

      iex> Code.Identifier.binary_op(:+)
      {:left, 210}

  """
  @spec binary_op(atom) :: {:left | :right, precedence :: pos_integer} | :error
  def binary_op(op) do
    cond do
      op in [:<-, :\\] -> {:left, 40}
      op in [:when] -> {:right, 50}
      op in [:"::"] -> {:right, 60}
      op in [:|] -> {:right, 70}
      op in [:=] -> {:right, 100}
      op in [:||, :|||, :or] -> {:left, 130}
      op in [:&&, :&&&, :and] -> {:left, 140}
      op in [:==, :!=, :=~, :===, :!==] -> {:left, 150}
      op in [:<, :<=, :>=, :>] -> {:left, 160}
      op in [:|>, :<<<, :>>>, :<~, :~>, :<<~, :~>>, :<~>, :<|>] -> {:left, 170}
      op in [:in] -> {:left, 180}
      op in [:^^^] -> {:left, 190}
      op in [:++, :--, :.., :<>] -> {:right, 200}
      op in [:+, :-] -> {:left, 210}
      op in [:*, :/] -> {:left, 220}
      op in [:.] -> {:left, 310}
      true -> :error
    end
  end

  @doc """
  Classifies the given atom into one of the following categories:

    * `:alias` - a valid Elixir alias, like `Foo`, `Foo.Bar` and so on

    * `:callable_local` - an atom that can be used as a local call;
      this category includes identifiers like `:foo`

    * `:callable_operator` - all callable operators, such as `:<>`. Note
      operators such as `:..` are not callable because of ambiguity

    * `:not_atomable` - callable operators that must be wrapped in quotes when
      defined as an atom. For example, `::` must be written as `:"::"` to avoid
      the ambiguity between the atom and the keyword identifier

    * `:not_callable` - an atom that cannot be used as a function call after the
      `.` operator (for example, `:<<>>` is not callable because `Foo.<<>>` is a
      syntax error); this category includes atoms like `:Foo`, since they are
      valid identifiers but they need quotes to be used in function calls
      (`Foo."Bar"`)

    * `:other` - any other atom (these are usually escaped when inspected, like
      `:"foo and bar"`)

  """
  def classify(atom) when is_atom(atom) do
    charlist = Atom.to_charlist(atom)

    cond do
      atom in [:%, :%{}, :{}, :<<>>, :..., :.., :., :->] ->
        :not_callable

      atom in [:"::"] ->
        :not_atomable

      unary_op(atom) != :error or binary_op(atom) != :error ->
        :callable_operator

      valid_alias?(charlist) ->
        :alias

      true ->
        case :elixir_config.get(:identifier_tokenizer, String.Tokenizer).tokenize(charlist) do
          {kind, _acc, [], _, _, special} ->
            if kind == :identifier and not :lists.member(?@, special) do
              :callable_local
            else
              :not_callable
            end

          _ ->
            :other
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

  @doc """
  Inspects the identifier as an atom.
  """
  def inspect_as_atom(atom) when is_nil(atom) or is_boolean(atom) do
    Atom.to_string(atom)
  end

  def inspect_as_atom(atom) when is_atom(atom) do
    binary = Atom.to_string(atom)

    case classify(atom) do
      :alias ->
        case binary do
          binary when binary in ["Elixir", "Elixir.Elixir"] -> binary
          "Elixir.Elixir." <> _rest -> binary
          "Elixir." <> rest -> rest
        end

      type when type in [:callable_local, :callable_operator, :not_callable] ->
        ":" <> binary

      _ ->
        {escaped, _} = escape(binary, ?")
        IO.iodata_to_binary([?:, ?", escaped, ?"])
    end
  end

  @doc """
  Inspects the given identifier as a key.
  """
  def inspect_as_key(atom) when is_atom(atom) do
    binary = Atom.to_string(atom)

    case classify(atom) do
      type when type in [:callable_local, :callable_operator, :not_callable] ->
        IO.iodata_to_binary([binary, ?:])

      _ ->
        {escaped, _} = escape(binary, ?")
        IO.iodata_to_binary([?", escaped, ?", ?:])
    end
  end

  @doc """
  Inspects the given identifier as a function name.
  """
  def inspect_as_function(atom) when is_atom(atom) do
    binary = Atom.to_string(atom)

    case classify(atom) do
      type when type in [:callable_local, :callable_operator, :not_atomable] ->
        binary

      type ->
        escaped =
          if type in [:not_callable, :alias] do
            binary
          else
            elem(escape(binary, ?"), 0)
          end

        IO.iodata_to_binary([?", escaped, ?"])
    end
  end

  @doc """
  Extracts the name and arity of the parent from the anonymous function identifier.
  """
  # Example of this format: -NAME/ARITY-fun-COUNT-
  def extract_anonymous_fun_parent(atom) when is_atom(atom) do
    with "-" <> rest <- Atom.to_string(atom),
         [trailing | reversed] = rest |> String.split("/") |> Enum.reverse(),
         [arity, _inner, _count, ""] <- String.split(trailing, "-") do
      {reversed |> Enum.reverse() |> Enum.join("/") |> String.to_atom(), arity}
    else
      _ -> :error
    end
  end

  @doc """
  Escapes the given identifier.
  """
  def escape(other, char, count \\ :infinity, fun \\ &escape_map/1) do
    escape(other, char, count, [], fun)
  end

  defp escape(<<_, _::binary>> = binary, _char, 0, acc, _fun) do
    {acc, binary}
  end

  defp escape(<<char, t::binary>>, char, count, acc, fun) do
    escape(t, char, decrement(count), [acc | [?\\, char]], fun)
  end

  defp escape(<<?#, ?{, t::binary>>, char, count, acc, fun) do
    escape(t, char, decrement(count), [acc | '\\\#{'], fun)
  end

  defp escape(<<h::utf8, t::binary>>, char, count, acc, fun) do
    escaped = if value = fun.(h), do: value, else: escape_char(h)
    escape(t, char, decrement(count), [acc | escaped], fun)
  end

  defp escape(<<a::4, b::4, t::binary>>, char, count, acc, fun) do
    escape(t, char, decrement(count), [acc | ['\\x', to_hex(a), to_hex(b)]], fun)
  end

  defp escape(<<>>, _char, _count, acc, _fun) do
    {acc, <<>>}
  end

  defp escape_char(0), do: '\\0'

  defp escape_char(65279), do: '\\uFEFF'

  defp escape_char(char)
       when char in 0x20..0x7E
       when char in 0xA0..0xD7FF
       when char in 0xE000..0xFFFD
       when char in 0x10000..0x10FFFF do
    <<char::utf8>>
  end

  defp escape_char(char) when char < 0x100 do
    <<a::4, b::4>> = <<char::8>>
    ['\\x', to_hex(a), to_hex(b)]
  end

  defp escape_char(char) when char < 0x10000 do
    <<a::4, b::4, c::4, d::4>> = <<char::16>>
    ['\\x{', to_hex(a), to_hex(b), to_hex(c), to_hex(d), ?}]
  end

  defp escape_char(char) when char < 0x1000000 do
    <<a::4, b::4, c::4, d::4, e::4, f::4>> = <<char::24>>
    ['\\x{', to_hex(a), to_hex(b), to_hex(c), to_hex(d), to_hex(e), to_hex(f), ?}]
  end

  defp escape_map(?\a), do: '\\a'
  defp escape_map(?\b), do: '\\b'
  defp escape_map(?\d), do: '\\d'
  defp escape_map(?\e), do: '\\e'
  defp escape_map(?\f), do: '\\f'
  defp escape_map(?\n), do: '\\n'
  defp escape_map(?\r), do: '\\r'
  defp escape_map(?\t), do: '\\t'
  defp escape_map(?\v), do: '\\v'
  defp escape_map(?\\), do: '\\\\'
  defp escape_map(_), do: false

  @compile {:inline, to_hex: 1, decrement: 1}
  defp to_hex(c) when c in 0..9, do: ?0 + c
  defp to_hex(c) when c in 10..15, do: ?A + c - 10

  defp decrement(:infinity), do: :infinity
  defp decrement(counter), do: counter - 1
end
