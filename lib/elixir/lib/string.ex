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
  def printable?(<<c, t :: binary>>) when c in ?\s..?~ do
    printable?(t)
  end

  # From 16#A0 to 16#BF
  def printable?(<<194, c, t :: binary>>) when c in 160..191 do
    printable?(t)
  end

  # From 16#C0 to 16#7FF
  def printable?(<<m, o1, t :: binary>>) when m in 195..223 and o1 in 128..191 do
    printable?(t)
  end

  # From 16#800 to 16#CFFF
  def printable?(<<m, o1, o2, t :: binary>>) when m in 224..236 and
      o1 >= 128 and o1 < 192 and o2 >= 128 and o2 < 192 do
    printable?(t)
  end

  # From 16#D000 to 16#D7FF
  def printable?(<<237, o1, o2, t :: binary>>) when
      o1 >= 128 and o1 < 160 and o2 >= 128 and o2 < 192 do
    printable?(t)
  end

  # Reject 16#FFFF and 16#FFFE
  def printable?(<<239, 191, o>>) when o == 190 or o == 191 do
    false
  end

  # From 16#E000 to 16#EFFF
  def printable?(<<m, o1, o2, t :: binary>>) when (m == 238 or m == 239) and
      o1 in 128..191 and o2 in 128..191 do
    printable?(t)
  end

  # From 16#F000 to 16#FFFD
  def printable?(<<239, o1, o2, t :: binary>>) when
      o1 in 128..191 and o2 in 128..191 do
    printable?(t)
  end

  # From 16#10000 to 16#3FFFF
  def printable?(<<240, o1, o2, o3, t :: binary>>) when
      o1 in 144..191 and o2 in 128..191 and o3 in 128..191 do
    printable?(t)
  end

  # Reject 16#110000 onwards
  def printable?(<<244, o1, _, _, _ :: binary>>) when o1 >= 144 do
    false
  end

  # From 16#4000 to 16#10FFFF
  def printable?(<<m, o1, o2, o3, t :: binary>>) when m in 241..244 and
      o1 in 128..191 and o2 in 128..191 and o3 in 128..191 do
    printable?(t)
  end

  def printable?(<<?\n, t :: binary>>), do: printable?(t)
  def printable?(<<?\r, t :: binary>>), do: printable?(t)
  def printable?(<<?\t, t :: binary>>), do: printable?(t)
  def printable?(<<?\v, t :: binary>>), do: printable?(t)
  def printable?(<<?\b, t :: binary>>), do: printable?(t)
  def printable?(<<?\f, t :: binary>>), do: printable?(t)
  def printable?(<<?\e, t :: binary>>), do: printable?(t)

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

  @doc """
  Convert all characters on the given string to upper case.

  ## Examples

      String.upcase("abcd") #=> "ABCD"
      String.upcase("ab 123 xpto") #=> "AB 123 XPTO"
      String.upcase("josé") #=> "JOSÉ"

  """
  def upcase(<<>>), do: <<>>

  def upcase(<<195, c, t :: binary>>) when c in 160..191 do
    <<195, c - 32, upcase(t) :: binary>>
  end

  def upcase(<<c, t :: binary>>) when c in ?a..?z do
    <<c  - 32, upcase(t) :: binary>>
  end

  def upcase(<<c, t :: binary>>) do
    <<c , upcase(t) :: binary>>
  end

  @doc """
  Convert all characters on the given string to down case.

  ## Examples

      String.downcase("ABCD") #=> "abcd"
      String.downcase("AB 123 XPTO") #=> "ab 123 xpto"
      String.downcase("JOSÉ") #=> "josé"

  """
  def downcase(<<>>), do: <<>>

  def downcase(<<195, c, t :: binary>>) when c in 128..159 do
    <<195, c + 32, downcase(t) :: binary>>
  end

  def downcase(<<c, t :: binary>>) when c in ?A..?Z do
    <<c + 32, downcase(t) :: binary>>
  end

  def downcase(<<c, t :: binary>>) do
    <<c , downcase(t) :: binary>>
  end

  @doc """
  Returns a string where trailing char have been
  removed. If no `char` is passed `space`is used.

  ## Examples

      String.rstrip("   abc  ")      #=> "   abc"
      String.rstrip("   abc _", ?_)  #=> "   abc "

  """
  def rstrip(string, char // ?\s)

  # Do a quick check before we traverse the whole
  # binary. :binary.last is a fast operation (it
  # does not traverse the whole binary).
  def rstrip(string, char) do
    if :binary.last(string) == char do
      rstrip(string, "", char)
    else
      string
    end
  end

  defp rstrip(<<char, string :: binary>>, buffer, char) do
    rstrip(string, <<char, buffer :: binary>>, char)
  end

  defp rstrip(<<char, string :: binary>>, buffer, another_char) do
    <<buffer :: binary, char, rstrip(string, "", another_char) :: binary>>
  end

  defp rstrip(<<>>, _, _) do
    <<>>
  end

  @doc """
  Returns a string where leading char have been
  removed. If no `char` is passed `space`is used.

  ## Examples

      String.lstrip("   abc  ")       #=> "abc  "
      String.lstrip("_  abc  _", ?_)  #=> "  abc  _"

  """
  def lstrip(string, char // ?\s)

  def lstrip(<<char, rest :: binary>>, char) do
    <<lstrip(rest, char) :: binary>>
  end

  def lstrip(other, _char) do
    other
  end

  @doc """
  Returns a string where leading/trailing char have been
  removed. If no `char` is passed `space`is used.

  ## Examples

      String.strip("   abc  ")       #=> "abc"
      String.strip("a  abc  a", ?a)  #=> "  abc  "

  """
  def strip(string, char // ?\s) do
    rstrip(lstrip(string, char), char)
  end

  @doc """
  Returns a new binary based on `subject` by replacing the parts
  matching `pattern` for `replacement`. If `options` is specified
  with `[global: true]`, then it will replace all matches, otherwise
  it will replace just the first one.

  For the replaced part must be used in `replacement`, then the
  position or the positions where it is to be inserted must be specified by using
  the option `insert_replaced`.

  ## Examples

      Binary.replace("a,b,c", ",", "-") #=> "a-b,c"
      Binary.replace("a,b,c", ",", "-", global: true) #=> "a-b-c"
      Binary.replace("a,b,c", "b", "[]", insert_replaced: 1) #=> "a,[b],c"
      Binary.replace("a,b,c", ",", "[]", global: true, insert_replaced: 2) #=> "a[],b[],c"
      Binary.replace("a,b,c", ",", "[]", global: true, insert_replaced: [1,1]) #=> "a[,,]b[,,]c"

  """
  def replace(subject, pattern, replacement, raw_options // []) do
    options = translate_replace_options(raw_options)
    Erlang.binary.replace(subject, pattern, replacement, options)
  end


  @doc """
  Returns a binary `subject` duplicated `n` times.

  ## Examples

      Binary.duplicate("abc", 1) #=> "abc"
      Binary.duplicate("abc", 2) #=> "abcabc"

  """
  def duplicate(subject, n) when is_integer(n) and n > 0 do
    Erlang.binary.copy(subject, n)
  end


  defp translate_replace_options([]), do: []
  defp translate_replace_options(raw_options) do
    options = []
    if raw_options[:global] == true do
      options = [:global|options]
    end
    inserted_replaced = raw_options[:insert_replaced]
    if inserted_replaced != nil do
      options = [{:insert_replaced,inserted_replaced}|options]
    end
    options
  end

  @doc """
  Returns a list with codepoints
  """
  def codepoints(string) do
    codepoints(string, [])
  end

  def codepoints(<<194, char, rest :: binary>>, buffer) when char in 161..191 do
    codepoints(rest, buffer ++ [<<194, char>>])
  end

  def codepoints(<<first, char, rest :: binary>>, buffer) when first in 195..223 and char in 128..191 do
    codepoints(rest, buffer ++ [<<first, char>>])
  end

  def codepoints(<<first, second, char, rest :: binary>>, buffer) when first == 224 and second in 160..191 and char in 128..191 do
    codepoints(rest, buffer ++ [<<first, second, char>>])
  end

  def codepoints(<<first, second, char, rest :: binary>>, buffer) when first in 225..239 and second in 128..191 and char in 128..191 do
    codepoints(rest, buffer ++ [<<first, second, char>>])
  end

  def codepoints(<<first, second, char, rest :: binary>>, buffer) when first in 240..239 and second in 144..191 and char in 144..191 do
    codepoints(rest, buffer ++ [<<first, second, char>>])
  end

  def codepoints(<<other, rest :: binary>>, buffer) do
    codepoints(rest, buffer ++ [<<other>>])
  end

  def codepoints(<<>>, buffer) do
    buffer
  end
end
