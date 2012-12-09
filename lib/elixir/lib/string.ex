defmodule String do
  @moduledoc """
  A string in Elixir is a utf-8 binary. This module
  contains function to work with utf-8 data, its
  codepoints and graphemes.

  Notice that graphemes is a superset of UTF-8 codepoints
  which also contains named sequences as defined per
  http://www.unicode.org/reports/tr34/. In short, graphemes
  also contain multiple characters that are "perceived as
  a single character" by readers.

  For working with raw binaries, use Erlang's :binary
  module.
  """

  @type t :: binary
  @type codepoint :: t
  @type grapheme :: t

  @doc """
  Checks if a string is printable considering it is encoded
  as UTF-8. Returns true if so, false otherwise.

  ## Examples

      String.printable?("abc") #=> true

  """
  @spec printable?(t) :: boolean

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

      String.split("a,b,c", ",")  #=> ["a", "b", "c"]
      String.split("a,b,c", ",", global: false)  #=> ["a", "b,c"]

      String.split("foo bar")     #=> ["foo", "bar"]
      String.split("1,2 3,4", [" ", ","]) #=> ["1", "2", "3", "4"]

      String.split("a,b,c", %r{,}) #=> ["a", "b", "c"]
      String.split("a,b,c", %r{,}, global: false)  #=> ["a", "b,c"]
      String.split("a,b", %r{\.})   #=> ["a,b"]

  """
  @spec split(t) :: [t]
  @spec split(t, t | [t] | Regex.t) :: [t]
  @spec split(t, t | [t] | Regex.t, Keyword.t) :: [t]

  def split(binary, pattern // " ", options // [])

  def split(binary, pattern, options) when is_regex(pattern) do
    Regex.split(pattern, binary, global: options[:global])
  end

  def split(binary, pattern, options) do
    opts = if options[:global] != false, do: [:global], else: []
    :binary.split(binary, pattern, opts)
  end

  @doc """
  Convert all characters on the given string to upcase.

  This function relies on the simple uppercase mapping
  available in Unicode 6.2.0, check http://unicode.org/reports/tr44/
  for more information.

  ## Examples

      String.upcase("abcd") #=> "ABCD"
      String.upcase("ab 123 xpto") #=> "AB 123 XPTO"
      String.upcase("josé") #=> "JOSÉ"

  """
  @spec upcase(t) :: t
  defdelegate upcase(binary), to: String.Unicode

  @doc """
  Convert all characters on the given string to downcase.

  This function relies on the simple lowercase mapping
  available in Unicode 6.2.0, check http://unicode.org/reports/tr44/
  for more information.

  ## Examples

      String.downcase("ABCD") #=> "abcd"
      String.downcase("AB 123 XPTO") #=> "ab 123 xpto"
      String.downcase("JOSÉ") #=> "josé"

  """
  @spec downcase(t) :: t
  defdelegate downcase(binary), to: String.Unicode

  @doc """
  Returns a string where trailing whitespace characters
  and new line have been removed.

  ## Examples

      String.rstrip("   abc  ")      #=> "   abc"

  """
  @spec rstrip(t) :: t
  defdelegate rstrip(binary), to: String.Unicode

  @doc """
  Returns a string where trailing `char` have been removed.

  ## Examples

      String.rstrip("   abc _", ?_)  #=> "   abc "

  """
  @spec rstrip(t, char) :: t

  def rstrip("", _char), do: ""

  # Do a quick check before we traverse the whole
  # binary. :binary.last is a fast operation (it
  # does not traverse the whole binary).
  def rstrip(string, char) do
    if :binary.last(string) == char do
      do_rstrip(string, "", char)
    else
      string
    end
  end

  defp do_rstrip(<<char, string :: binary>>, buffer, char) do
    do_rstrip(string, <<char, buffer :: binary>>, char)
  end

  defp do_rstrip(<<char, string :: binary>>, buffer, another_char) do
    <<buffer :: binary, char, do_rstrip(string, "", another_char) :: binary>>
  end

  defp do_rstrip(<<>>, _, _) do
    <<>>
  end

  @doc """
  Returns a string where leading whitespace characters
  have been removed.

  ## Examples

      String.lstrip("   abc  ")       #=> "abc  "

  """
  defdelegate lstrip(binary), to: String.Unicode

  @doc """
  Returns a string where leading `char` have been removed.

  ## Examples

      String.lstrip("_  abc  _", ?_)  #=> "  abc  _"

  """

  @spec lstrip(t, char) :: t

  def lstrip(<<char, rest :: binary>>, char) do
    <<lstrip(rest, char) :: binary>>
  end

  def lstrip(other, _char) do
    other
  end

  @doc """
  Returns a string where leading/trailing whitespace
  and new line characters have been removed.

  ## Examples

      String.strip("   abc  ")       #=> "abc"

  """
  @spec strip(t) :: t

  def strip(string) do
    rstrip(lstrip(string))
  end

  @doc """
  Returns a string where leading/trailing `char` have been
  removed.

  ## Examples

      String.strip("a  abc  a", ?a)  #=> "  abc  "

  """
  @spec strip(t, char) :: t

  def strip(string, char) do
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

      String.replace("a,b,c", ",", "-") #=> "a-b-c"
      String.replace("a,b,c", ",", "-", global: false) #=> "a-b,c"
      String.replace("a,b,c", "b", "[]", insert_replaced: 1) #=> "a,[b],c"
      String.replace("a,b,c", ",", "[]", insert_replaced: 2) #=> "a[],b[],c"
      String.replace("a,b,c", ",", "[]", insert_replaced: [1,1]) #=> "a[,,]b[,,]c"

  """
  @spec replace(t, t, t) :: t
  @spec replace(t, t, t, Keyword.t) :: t

  def replace(subject, pattern, replacement, options // []) do
    opts = translate_replace_options(options)
    :binary.replace(subject, pattern, replacement, opts)
  end

  defp translate_replace_options(options) do
    opts = if options[:global] != false, do: [:global], else: []

    if insert = options[:insert_replaced] do
      opts = [{:insert_replaced,insert}|opts]
    end

    opts
  end

  @doc """
  Returns a binary `subject` duplicated `n` times.

  ## Examples

      String.duplicate("abc", 1) #=> "abc"
      String.duplicate("abc", 2) #=> "abcabc"

  """
  @spec duplicate(t, pos_integer) :: t
  def duplicate(subject, n) when is_integer(n) and n > 0 do
    :binary.copy(subject, n)
  end

  @doc """
  Returns a list with codepoints from an utf8 string.

  ## Examples

      String.codepoints("josé")         #=> ["j", "o", "s", "é"]
      String.codepoints("оптими зации") #=> ["о","п","т","и","м","и"," ","з","а","ц","и","и"]
      String.codepoints("ἅἪῼ")          #=> ["ἅ","Ἢ","ῼ"]

  """
  @spec codepoints(t) :: [codepoint]
  defdelegate codepoints(string), to: String.Unicode

  @doc """
  Returns the next codepoint in a String.

  The result is a tuple with the codepoint and the
  remaining of the string or `:no_codepoint` in case
  the String reached its end.

  ## Examples

      String.next_codepoint("josé") #=> { "j", "osé" }

  """
  @spec next_codepoint(t) :: codepoint | :no_codepoint
  defdelegate next_codepoint(string), to: String.Unicode

  @doc """
  Returns unicode graphemes in the string

  ## Examples
     String.graphemes("Ā̀stute") # => ["Ā̀","s","t","u","t","e"]

  """
  @spec graphemes(t) :: [grapheme]
  defdelegate graphemes(string), to: String.Unicode

  @doc """
  Returns the next grapheme in a String.

  The result is a tuple with the grapheme and the
  remaining of the string or `:no_grapheme` in case
  the String reached its end.

  ## Examples

      String.next_grapheme("josé") #=> { "j", "osé" }

  """
  @spec next_grapheme(t) :: grapheme | :no_grapheme
  defdelegate next_grapheme(string), to: String.Unicode

  @doc """
  Returns the first grapheme from an utf8 string.

  ## Examples

      String.first("elixir")  #=> "e"
      String.first("եոգլի") #=> "ե"

  """
  @spec first(t) :: grapheme | nil
  def first(string) do
    case next_grapheme(string) do
      { char, _ } -> char
      :no_grapheme -> nil
    end
  end

  @doc """
  Returns the last grapheme from an utf8 string.

  ## Examples

      String.last("elixir")  #=> "r"
      String.last("եոգլի") #=> "ի"

  """
  @spec last(t) :: grapheme | nil
  def last(string) do
    do_last(next_grapheme(string), nil)
  end

  defp do_last({char, rest}, _) do
    do_last(next_grapheme(rest), char)
  end

  defp do_last(:no_grapheme, last_char), do: last_char

  @doc """
  Returns the number of unicode graphemes in an utf8 string.

  ## Examples

      String.length("elixir")  #=> 6
      String.length("եոգլի") #=> 5

  """
  @spec length(t) :: non_neg_integer
  def length(string) do
    do_length(next_grapheme(string))
  end

  defp do_length({_, rest}) do
    1 + do_length(next_grapheme(rest))
  end

  defp do_length(:no_grapheme), do: 0

  @doc """
  Returns the grapheme in the `position` of the given utf8 `string`.
  If `position` is greater than `string` length, than it returns `nil`.

  ## Examples

      String.at("elixir", 0) #=> "e"
      String.at("elixir", 1) #=> "l"
      String.at("elixir", 10) #=> nil
      String.at("elixir", -1) #=> "r"
      String.at("elixir", -10) #=> nil

  """
  @spec at(t, integer) :: grapheme | nil

  def at(string, position) when position >= 0 do
    do_at(next_grapheme(string), position, 0)
  end

  def at(string, position) when position < 0 do
    real_pos = do_length(next_grapheme(string)) - abs(position)
    case real_pos >= 0 do
      true  -> do_at(next_grapheme(string), real_pos, 0)
      false -> nil
    end
  end

  defp do_at({_ , rest}, desired_pos, current_pos) when desired_pos > current_pos do
    do_at(next_grapheme(rest), desired_pos, current_pos + 1)
  end

  defp do_at({char, _}, desired_pos, current_pos) when desired_pos == current_pos do
    char
  end

  defp do_at(:no_grapheme, _, _), do: nil
end
