defmodule Binary do
  @doc """
  Access the binary via a predicate.

  If a regular expression, it returns a binary with the
  matched contents.

  This implements the same API as the `Access` protocol.

  Notice currently Elixir does not provide functions for
  accessing utf-8 code points.

  ## Examples

      binary = "abc"
      Binary.access binary, %r(a) #=> "a"

  """
  def access(binary, access) do
    Access.BitString.access(binary, access)
  end

  @doc %B"""
  Receives a char list and escapes all special chars (like \n)
  and interpolation markers. A last argument is given and wraps
  the whole char list given.

  ## Examples

      Binary.escape "foo", ?'
      #=> "'foo'"

  """
  def escape(other, char) do
    <<char>> <> do_escape(other, char)
  end

  @doc """
  Check if a binary is printable considering it is encoded
  as UTF-8. Returns true if so, false otherwise.

  ## Examples

      Binary.printable?("abc") #=> true

  """

  # Allow basic ascii chars
  def printable?(<<c, t|binary>>) when c >= ?\s and c <= ?~ do
    printable?(t)
  end

  # From 16#A0 to 16#BF
  def printable?(<<194, c, t|binary>>) when c >= 160 and c <= 191 do
    printable?(t)
  end

  # From 16#C0 to 16#7FF
  def printable?(<<m, o1, t|binary>>) when m >= 195 and m <= 223 and o1 >= 128 and o1 < 192  do
    printable?(t)
  end

  # From 16#800 to 16#CFFF
  def printable?(<<m, o1, o2, t|binary>>) when m >= 224 and m <= 236 and
      o1 >= 128 and o1 < 192 and o2 >= 128 and o2 < 192 do
    printable?(t)
  end

  # From 16#D000 to 16#D7FF
  def printable?(<<237, o1, o2, t|binary>>) when
      o1 >= 128 and o1 < 160 and o2 >= 128 and o2 < 192 do
    printable?(t)
  end

  # Reject 16#FFFF and 16#FFFE
  def printable?(<<239, 191, o>>) when o == 190 or o == 191 do
    false
  end

  # From 16#E000 to 16#EFFF
  def printable?(<<m, o1, o2, t|binary>>) when (m == 238 or m == 239) and
      o1 >= 128 and o1 < 192 and o2 >= 128 and o2 < 192 do
    printable?(t)
  end

  # From 16#F000 to 16#FFFD
  def printable?(<<239, o1, o2, t|binary>>) when
      o1 >= 128 and o1 < 192 and o2 >= 128 and o2 < 192 do
    printable?(t)
  end

  # From 16#10000 to 16#3FFFF
  def printable?(<<240, o1, o2, o3, t|binary>>) when
      o1 >= 144 and o1 < 192 and o2 >= 128 and o2 < 192 and o3 >= 128 and o3 < 192 do
    printable?(t)
  end

  # Reject 16#110000 onwards
  def printable?(<<244, o1, _, _, _|binary>>) when o1 >= 144 do
    false
  end

  # From 16#4000 to 16#10FFFF
  def printable?(<<m, o1, o2, o3, t|binary>>) when m >= 241 and m <= 244 and
      o1 >= 128 and o1 < 192 and o2 >= 128 and o2 < 192 and o3 >= 128 and o3 < 192 do
    printable?(t)
  end

  def printable?(<<?\n, t|binary>>), do: printable?(t)
  def printable?(<<?\r, t|binary>>), do: printable?(t)
  def printable?(<<?\t, t|binary>>), do: printable?(t)
  def printable?(<<?\v, t|binary>>), do: printable?(t)
  def printable?(<<?\b, t|binary>>), do: printable?(t)
  def printable?(<<?\f, t|binary>>), do: printable?(t)
  def printable?(<<?\e, t|binary>>), do: printable?(t)

  def printable?(<<>>), do: true
  def printable?(_),    do: false

  @doc %B"""
  Unescape the given chars. The unescaping is driven by the same
  rules as single- and double-quoted strings. Check `unescape/2`
  for information on how to customize the escaping map.

  In this setup, Elixir will escape the following: `\b`, `\d`,
  `\e`, `\f`, `\n`, `\r`, `\s`, `\t` and `\v`. Octals are also
  escaped according to the latin1 set they represent.

  ## Examples

      Binary.unescape "example\\n"
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

      Binary.unescape "example\\n", unescape_map(&1)

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

  defp do_escape(<<char, t|binary>>, char) do
    <<?\\, char, do_escape(t, char)|binary>>
  end

  defp do_escape(<<h, t|binary>>, char) when
    h == ?#  or h == ?\b or
    h == ?\d or h == ?\e or
    h == ?\f or h == ?\n or
    h == ?\r or h == ?\\ or
    h == ?\t or h == ?\v do
    <<?\\, escape_map(h), do_escape(t, char)|binary>>
  end

  defp do_escape(<<h, t|binary>>, char) do
    <<h, do_escape(t,char)|binary>>
  end

  defp do_escape(<<>>, char) do
    <<char>>
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