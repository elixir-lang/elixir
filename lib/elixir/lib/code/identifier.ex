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
      op in [:&, :...] -> {:non_associative, 90}
      op in [:!, :^, :not, :+, :-, :"~~~"] -> {:non_associative, 300}
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
      op in [:||, :|||, :or] -> {:left, 120}
      op in [:&&, :&&&, :and] -> {:left, 130}
      op in [:==, :!=, :=~, :===, :!==] -> {:left, 140}
      op in [:<, :<=, :>=, :>] -> {:left, 150}
      op in [:|>, :<<<, :>>>, :<~, :~>, :<<~, :~>>, :<~>, :"<|>"] -> {:left, 160}
      op in [:in] -> {:left, 170}
      op in [:"^^^"] -> {:left, 180}
      op in [:++, :--, :.., :<>, :+++, :---] -> {:right, 200}
      op in [:+, :-] -> {:left, 210}
      op in [:*, :/] -> {:left, 220}
      op in [:**] -> {:left, 230}
      op in [:.] -> {:left, 310}
      true -> :error
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
  @spec escape(binary(), char() | nil, :infinity | non_neg_integer, (char() -> iolist() | false)) ::
          {escaped :: iolist(), remaining :: binary()}
  def escape(binary, char, limit \\ :infinity, fun \\ &escape_map/1)
      when ((char in 0..0x10FFFF or is_nil(char)) and limit == :infinity) or
             (is_integer(limit) and limit >= 0) do
    escape(binary, char, limit, [], fun)
  end

  defp escape(<<_, _::binary>> = binary, _char, 0, acc, _fun) do
    {acc, binary}
  end

  defp escape(<<char, t::binary>>, char, count, acc, fun) do
    escape(t, char, decrement(count), [acc | [?\\, char]], fun)
  end

  defp escape(<<?#, ?{, t::binary>>, char, count, acc, fun) do
    escape(t, char, decrement(count), [acc | [?\\, ?#, ?{]], fun)
  end

  defp escape(<<h::utf8, t::binary>>, char, count, acc, fun) do
    escaped = if value = fun.(h), do: value, else: escape_char(h)
    escape(t, char, decrement(count), [acc | escaped], fun)
  end

  defp escape(<<a::4, b::4, t::binary>>, char, count, acc, fun) do
    escape(t, char, decrement(count), [acc | [?\\, ?x, to_hex(a), to_hex(b)]], fun)
  end

  defp escape(<<>>, _char, _count, acc, _fun) do
    {acc, <<>>}
  end

  defp escape_char(0), do: [?\\, ?0]

  defp escape_char(char)
       # Some characters that are confusing (zero-width / alternative spaces) are displayed
       # using their unicode representation:
       # https://en.wikipedia.org/wiki/Universal_Character_Set_characters#Special-purpose_characters

       # BOM
       when char == 0xFEFF
       # Mathematical invisibles
       when char in 0x2061..0x2064
       # Bidirectional neutral
       when char in [0x061C, 0x200E, 0x200F]
       # Bidirectional general (source of vulnerabilities)
       when char in 0x202A..0x202E
       when char in 0x2066..0x2069
       # Interlinear annotations
       when char in 0xFFF9..0xFFFC
       # Zero-width joiners and non-joiners
       when char in [0x200C, 0x200D, 0x034F]
       # Non-break space / zero-width space
       when char in [0x00A0, 0x200B, 0x2060]
       # Line/paragraph separators
       when char in [0x2028, 0x2029]
       # Spaces
       when char in 0x2000..0x200A
       when char == 0x205F do
    <<a::4, b::4, c::4, d::4>> = <<char::16>>
    [?\\, ?u, to_hex(a), to_hex(b), to_hex(c), to_hex(d)]
  end

  defp escape_char(char)
       when char in 0x20..0x7E
       when char in 0xA0..0xD7FF
       when char in 0xE000..0xFFFD
       when char in 0x10000..0x10FFFF do
    <<char::utf8>>
  end

  defp escape_char(char) when char < 0x100 do
    <<a::4, b::4>> = <<char::8>>
    [?\\, ?x, to_hex(a), to_hex(b)]
  end

  defp escape_char(char) when char < 0x10000 do
    <<a::4, b::4, c::4, d::4>> = <<char::16>>
    [?\\, ?x, ?{, to_hex(a), to_hex(b), to_hex(c), to_hex(d), ?}]
  end

  defp escape_char(char) when char < 0x1000000 do
    <<a::4, b::4, c::4, d::4, e::4, f::4>> = <<char::24>>
    [?\\, ?x, ?{, to_hex(a), to_hex(b), to_hex(c), to_hex(d), to_hex(e), to_hex(f), ?}]
  end

  defp escape_map(?\a), do: [?\\, ?a]
  defp escape_map(?\b), do: [?\\, ?b]
  defp escape_map(?\d), do: [?\\, ?d]
  defp escape_map(?\e), do: [?\\, ?e]
  defp escape_map(?\f), do: [?\\, ?f]
  defp escape_map(?\n), do: [?\\, ?n]
  defp escape_map(?\r), do: [?\\, ?r]
  defp escape_map(?\t), do: [?\\, ?t]
  defp escape_map(?\v), do: [?\\, ?v]
  defp escape_map(?\\), do: [?\\, ?\\]
  defp escape_map(_), do: false

  @compile {:inline, to_hex: 1, decrement: 1}
  defp to_hex(c) when c in 0..9, do: ?0 + c
  defp to_hex(c) when c in 10..15, do: ?A + c - 10

  defp decrement(:infinity), do: :infinity
  defp decrement(counter), do: counter - 1
end
