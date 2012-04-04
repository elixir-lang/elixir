defmodule CharList do
  @doc %B"""
  Unescape the given chars. The unescaping is driven by the same
  rules as single- and double-quoted strings. Check `unescape/2`
  for information on how to customize the escaping map.

  In this setup, Elixir will escape the following: `\b`, `\d`,
  `\e`, `\f`, `\n`, `\r`, `\s`, `\t` and `\v`. Octals are also
  escaped according to the latin1 set they represent.

  ## Examples

      CharList.unescape "example\\n"
      # => "example\n"

  In the example above, we pass a string with `\n` escaped
  and we return a version with it unescaped.
  """
  def unescape(chars) do
    Erlang.elixir_interpolation.unescape_chars(chars)
  end

  @doc %B"""
  Unescape the given chars according to the map given.
  Check `unescape/1` if you want to use the same map as Elixir
  single- and double-quoted strings.

  ## Map

  The map must be a function. The function receives an integer
  representing the number of the characters it wants to unescape.
  Here is the default mapping function implemented by Elixir:

      def unescape_map(?b), do: ?\b
      def unescape_map(?d), do: ?\d
      def unescape_map(?e), do: ?\e
      def unescape_map(?f), do: ?\f
      def unescape_map(?n), do: ?\n
      def unescape_map(?r), do: ?\r
      def unescape_map(?s), do: ?\s
      def unescape_map(?t), do: ?\t
      def unescape_map(?v), do: ?\v
      def unescape_map(e), do: e

  If the `unescape_map` function returns false. The char is
  not escaped and `\` is kept in the char list.

  ## Examples

  Using the unescape_map defined above is easy:

      CharList.unescape "example\\n", unescape_map(&1)

  """
  def unescape(chars, map) do
    Erlang.elixir_interpolation.unescape_chars(chars, map)
  end

  @doc """
  Unescape the given tokens according to the default map.
  Check `unescape/1` and `unescape/2` for more information
  about unescaping. Only tokens that are char lists are
  unescaped, all others are ignored. This method is useful
  when implementing your own sigils. Check the implementation
  of `Elixir::Builtin.__b__` for examples.
  """
  def unescape_tokens(tokens) do
    Erlang.elixir_interpolation.unescape_tokens(tokens)
  end

  @doc """
  Unescape the given tokens according to the given map.
  Check `unescape_tokens/1` and `unescaped/2` for more information.
  """
  def unescape_tokens(tokens, map) do
    Erlang.elixir_interpolation.unescape_tokens(tokens, map)
  end
end