defmodule CharList do
  @doc %B"""
  Receives a char list and escapes all special chars (like \n)
  and interpolation markers. A last argument is given and wraps
  the whole char list given.

  ## Examples

      CharList.escape 'foo', ?"
      #=> '"foo"'

  """
  def escape(other, char) do
    [char|do_escape(other, char)]
  end

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

  ## Helpers

  defp do_escape([char|t], char) do
    [?\\,char|do_escape(t, char)]
  end

  defp do_escape([h|t], char) when
    h == ?#  or h == ?\b or
    h == ?\d or h == ?\e or
    h == ?\f or h == ?\n or
    h == ?\r or h == ?\\ or
    h == ?\t or h == ?\v do
    [?\\,escape_map(h)|do_escape(t, char)]
  end

  defp do_escape([h|t], char) do
    [h|do_escape(t,char)]
  end

  defp do_escape([], char) do
    [char]
  end

  defp escape_map(?#),  do: ?#
  defp escape_map(?\b), do: ?b
  defp escape_map(?\d), do: ?d
  defp escape_map(?\e), do: ?e
  defp escape_map(?\f), do: ?f
  defp escape_map(?\n), do: ?n
  defp escape_map(?\r), do: ?r
  defp escape_map(?\\), do: ?\\
  defp escape_map(?\t), do: ?t
  defp escape_map(?\v), do: ?v
end