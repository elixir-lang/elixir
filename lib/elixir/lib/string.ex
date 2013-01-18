defmodule String do
  @moduledoc %B"""
  A string in Elixir is a UTF-8 encoded binary.

  The functions in this module act accordingly to the
  Unicode Standard, version 6.2.0. A codepoint is a
  Unicode Character, which may be represented by one
  or more bytes. For example, the character "é" is
  represented with two bytes:

      string = "é"
      #=> "é"
      size(string)
      #=> 2

  Furthermore, this module also presents the concept of
  graphemes, which are multiple characters that may be
  "perceived as a single character" by readers. For example,
  the same "é" character written above could be represented
  by the letter "e" followed by the accent "́":

      string = "\x{0065}\x{0301}"
      #=> "é"
      size(string)
      #=> 3

  Although the example above is made of two characters, it is
  perceived by users as one.

  Graphemes can also be two characters that are interpreted
  as one by some languages. For example, some languages may
  consider "ch" as a grapheme. However, since this information
  depends on the locale, it is not taken account by this module.

  In general, while all functions in this module takes into
  consideration the Unicode Standard, it does not contain any
  of the locale specific behaviour.

  ## Integer codepoints

  Although codepoints could be represented as integers, this
  module represents all codepoints as binaries. For example:

      String.codepoints "josé" #=> ["j", "o", "s", "é"]

  There are a couple ways to retrieve a character integer
  codepoint. One may use the `?` special macro:

      ?j #=> 106
      ?é #=> 233

  Or also via pattern matching:

      << eacute :: utf8 >> = "é"
      eacute #=> 233

  As we have seen above, codepoints can be inserted into
  a string by their hexadecimal code:

      string = "jos\x{0065}\x{0301}"
      #=> "josé"

  ## Self-synchronization

  The UTF-8 encoding is self-synchronizing. This means that
  if malformed data (i.e., data that is not possible according
  to the definition of the encoding) is encountered, only one
  codepoint needs to be rejected.

  This module relies on this behaviour to ignore such invalid
  characters. For example, `String.length` is going to return
  a correct result even if an invalid codepoint is fed into.

  In the future, bang version of such functions may be
  provided which will rather raise on such invalid data.
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

  def printable?(<< h :: utf8, t :: binary >>)
      when h in ?\040..?\176
      when h in 0xA0..0xD7FF
      when h in 0xE000..0xFFFD
      when h in 0x10000..0x10FFFF do
    printable?(t)
  end

  def printable?(<<?\n, t :: binary>>), do: printable?(t)
  def printable?(<<?\r, t :: binary>>), do: printable?(t)
  def printable?(<<?\t, t :: binary>>), do: printable?(t)
  def printable?(<<?\v, t :: binary>>), do: printable?(t)
  def printable?(<<?\b, t :: binary>>), do: printable?(t)
  def printable?(<<?\f, t :: binary>>), do: printable?(t)
  def printable?(<<?\e, t :: binary>>), do: printable?(t)
  def printable?(<<?\a, t :: binary>>), do: printable?(t)

  def printable?(<<>>), do: true
  def printable?(_),    do: false

  @doc """
  Divides a string into sub string based on a pattern,
  returning a list of these sub string. The pattern can
  be a string, a list of strings or a regular expression.

  The string is split into as many parts as possible by
  default, unless the `global` option is set to false.
  If a pattern is not specified, the string is split on
  whitespace occurrences.

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

  ## Examples

      String.upcase("abcd") #=> "ABCD"
      String.upcase("ab 123 xpto") #=> "AB 123 XPTO"
      String.upcase("josé") #=> "JOSÉ"

  """
  @spec upcase(t) :: t
  defdelegate upcase(binary), to: String.Unicode

  @doc """
  Convert all characters on the given string to downcase.

  ## Examples

      String.downcase("ABCD") #=> "abcd"
      String.downcase("AB 123 XPTO") #=> "ab 123 xpto"
      String.downcase("JOSÉ") #=> "josé"

  """
  @spec downcase(t) :: t
  defdelegate downcase(binary), to: String.Unicode

  @doc """
  Convert the first character on the given string to uppercase
  and the remaining to downcase.

  ## Examples

      String.capitalize("abcd") #=> "Abcd"
      String.capitalize("ab 123 xpto") #=> "Ab 123 xpto"
      String.capitalize("josé") #=> "José"

  """
  @spec capitalize(t) :: t

  def capitalize(string) when is_binary(string) do
    case next_grapheme(string) do
      { char, rest } -> upcase(char) <> downcase(rest)
      :no_grapheme -> ""
    end
  end

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
  matching `pattern` for `replacement`. By default, it replaces
  all entries, except if the `global` option is set to false.

  If the replaced part must be used in `replacement`, then the
  position or the positions where it is to be inserted must be
  specified by using the option `insert_replaced`.

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
  Returns all codepoints in the string.

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
  the string reached its end.

  As the other functions in the String module, this
  function does not check for the validity of the codepoint.
  That said, if an invalid codepoint is found, it will
  be returned by this function.

  ## Examples

      String.next_codepoint("josé") #=> { "j", "osé" }

  """
  @spec next_codepoint(t) :: {codepoint, t} | :no_codepoint
  defdelegate next_codepoint(string), to: String.Unicode

  @doc %B"""
  Checks whether `str` is a valid codepoint.

  Note that the empty string is considered invalid, as are
  strings containing multiple codepoints.

  ## Examples

      String.valid_codepoint?("a") #=> true
      String.valid_codepoint?("ø") #=> true
      String.valid_codepoint?("\xffff") #=> false
      String.valid_codepoint?("asdf") #=> false

  """
  @spec valid_codepoint?(codepoint) :: boolean
  def valid_codepoint?(<<_ :: utf8>>), do: true
  def valid_codepoint?(_), do: false

  @doc """
  Returns unicode graphemes in the string.

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

  @doc """
  Returns a substring starting at the offset given by the first, and
  a length given by the second.
  If the offset is greater than string length, than it returns nil.

  ## Examples

      String.slice("elixir", 1, 3) #=> "lix"
      String.slice("elixir", 1, 10) #=> "lixir"
      String.slice("elixir", 10, 3) #=> nil
      String.slice("elixir", -4, 4) #=> "ixi"
      String.slice("elixir", -10, 3) #=> nil

  """
  @spec slice(t, integer, integer) :: grapheme | nil

  def slice(string, start, len) when start >= 0 do
    do_slice(next_grapheme(string), start, start + len - 1, 0, "")
  end

  def slice(string, start, len) when start < 0 do
    real_start_pos = do_length(next_grapheme(string)) - abs(start)
    case real_start_pos >= 0 do
      true -> do_slice(next_grapheme(string), real_start_pos, real_start_pos + len - 1, 0, "")
      false -> nil
    end
  end

  defp do_slice(_, start_pos, last_pos, _, _) when start_pos > last_pos do
    nil
  end

  defp do_slice({_, rest}, start_pos, last_pos, current_pos, acc) when current_pos < start_pos do
    do_slice(next_grapheme(rest), start_pos, last_pos, current_pos + 1, acc)
  end

  defp do_slice({char, rest}, start_pos, last_pos, current_pos, acc) when current_pos >= start_pos and current_pos < last_pos do
    do_slice(next_grapheme(rest), start_pos, last_pos, current_pos + 1, acc <> char)
  end

  defp do_slice({char, _}, start_pos, last_pos, current_pos, acc) when current_pos >= start_pos and current_pos == last_pos do
    acc <> char
  end

  defp do_slice(:no_grapheme, _, _, _, acc) do
    case acc do
      "" -> nil
      _ -> acc
    end
  end
end
