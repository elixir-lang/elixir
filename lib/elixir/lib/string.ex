defmodule String do
  @moduledoc """
  A string in Elixir is a utf-8 binary. This module
  contains function to work with utf-8 data and its
  codepoints.

  For working with raw binaries, use Erlang's :binary
  module.
  """

  @doc """
  Checks if a string is printable considering it is encoded
  as UTF-8. Returns true if so, false otherwise.

  ## Examples

      String.printable?("abc") #=> true

  """

  # Allow basic ascii chars
  def printable?(<<c, t|:binary>>) when c in ?\s..?~ do
    printable?(t)
  end

  # From 16#A0 to 16#BF
  def printable?(<<194, c, t|:binary>>) when c in 160..191 do
    printable?(t)
  end

  # From 16#C0 to 16#7FF
  def printable?(<<m, o1, t|:binary>>) when m in 195..223 and o1 in 128..191 do
    printable?(t)
  end

  # From 16#800 to 16#CFFF
  def printable?(<<m, o1, o2, t|:binary>>) when m in 224..236 and
      o1 >= 128 and o1 < 192 and o2 >= 128 and o2 < 192 do
    printable?(t)
  end

  # From 16#D000 to 16#D7FF
  def printable?(<<237, o1, o2, t|:binary>>) when
      o1 >= 128 and o1 < 160 and o2 >= 128 and o2 < 192 do
    printable?(t)
  end

  # Reject 16#FFFF and 16#FFFE
  def printable?(<<239, 191, o>>) when o == 190 or o == 191 do
    false
  end

  # From 16#E000 to 16#EFFF
  def printable?(<<m, o1, o2, t|:binary>>) when (m == 238 or m == 239) and
      o1 in 128..191 and o2 in 128..191 do
    printable?(t)
  end

  # From 16#F000 to 16#FFFD
  def printable?(<<239, o1, o2, t|:binary>>) when
      o1 in 128..191 and o2 in 128..191 do
    printable?(t)
  end

  # From 16#10000 to 16#3FFFF
  def printable?(<<240, o1, o2, o3, t|:binary>>) when
      o1 in 144..191 and o2 in 128..191 and o3 in 128..191 do
    printable?(t)
  end

  # Reject 16#110000 onwards
  def printable?(<<244, o1, _, _, _|:binary>>) when o1 >= 144 do
    false
  end

  # From 16#4000 to 16#10FFFF
  def printable?(<<m, o1, o2, o3, t|:binary>>) when m in 241..244 and
      o1 in 128..191 and o2 in 128..191 and o3 in 128..191 do
    printable?(t)
  end

  def printable?(<<?\n, t|:binary>>), do: printable?(t)
  def printable?(<<?\r, t|:binary>>), do: printable?(t)
  def printable?(<<?\t, t|:binary>>), do: printable?(t)
  def printable?(<<?\v, t|:binary>>), do: printable?(t)
  def printable?(<<?\b, t|:binary>>), do: printable?(t)
  def printable?(<<?\f, t|:binary>>), do: printable?(t)
  def printable?(<<?\e, t|:binary>>), do: printable?(t)

  def printable?(<<>>), do: true
  def printable?(_),    do: false


  @doc """
  Divides a string into sub string based on a pattern,
  returning a list of these sub string. The pattern can
  be a string, a list of strings or a regular expression.

  The string is split into two parts by default, unless
  `global` option is true. If a pattern is not specified,
  the string is split on whitespace occurrences.

  It returns a list with the original string if the pattern
  can't be matched.

  ## Examples

    String.split("a,b,c", ",")  #=> ["a", "b,c"]
    String.split("a,b,c", ",", global: true)  #=> ["a", "b", "c"]
    String.split("foo bar")     #=> ["foo", "bar"]
    String.split("1,2 3,4", [" ", ","]) #=> ["1", "2 3,4"]
    String.split("1,2 3,4", [" ", ","], global: true) #=> ["1", "2", "3", "4"]
    String.split("a,b", ".")    #=> ["a,b"]

    String.split("a,b,c", %r{,})  #=> ["a", "b,c"]
    String.split("a,b,c", %r{,}, global: true) #=> ["a", "b", "c"]
    String.split("a,b", %r{\.})   #=> ["a,b"]

  """
  def split(binary, pattern // " ", options // [])

  def split(binary, pattern, options) when is_regex(pattern) do
    parts = if options[:global], do: :infinity, else: 2
    Regex.split(pattern, binary, parts: parts)
  end

  def split(binary, pattern, options) do
    options_list = []
    if options[:global] do
      options_list = [:global|options_list]
    end
    :binary.split(binary, pattern, options_list)
  end
end