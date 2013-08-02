defmodule String do
  @moduledoc %B"""
  A String in Elixir is a UTF-8 encoded binary.

  ## String and binary operations

  The functions in this module act according to the
  Unicode Standard, version 6.2.0. For example,
  `capitalize/1`, `downcase/1`, `strip/1` are provided by this
  module.

  Besides this module, Elixir provides more low-level
  operations that work directly with binaries. Some
  of those can be found in the `Kernel` module, as:

  * `Kernel.binary_part/2` and `Kernelbinary_part/3` - retrieves part of the binary
  * `Kernel.bit_size/1` and `Kernel.byte_size/1` - size related functions
  * `Kernel.is_bitstring/1` and `Kernel.is_binary/1` - type checking function
  * Plus a number of conversion functions, like `Kernel.binary_to_atom/2`,
    `Kernel.binary_to_integer/2`, `Kernel.binary_to_term/1` and their opposite
    like `Kernel.integer_to_binary/2`

  Finally, the [`:binary` module](http://erlang.org/doc/man/binary.html)
  provides a few other functions that work on the byte level.

  ## Codepoints and graphemes

  As per the Unicode Standard, a codepoint is an Unicode
  Character, which may be represented by one or more bytes.
  For example, the character "é" is represented with two
  bytes:

      iex> string = "é"
      ...> byte_size(string)
      2

  Furthermore, this module also presents the concept of
  graphemes, which are multiple characters that may be
  "perceived as a single character" by readers. For example,
  the same "é" character written above could be represented
  by the letter "e" followed by the accent ́:

      iex> string = "\x{0065}\x{0301}"
      ...> byte_size(string)
      3

  Although the example above is made of two characters, it is
  perceived by users as one.

  Graphemes can also be two characters that are interpreted
  as one by some languages. For example, some languages may
  consider "ch" as a grapheme. However, since this information
  depends on the locale, it is not taken into account by this
  module.

  In general, the functions in this module rely on the Unicode
  Standard, but does not contain any of the locale specific
  behaviour.

  ## Integer codepoints

  Although codepoints could be represented as integers, this
  module represents all codepoints as strings. For example:

      iex> String.codepoints("josé")
      ["j", "o", "s", "é"]

  There are a couple of ways to retrieve a character integer
  codepoint. One may use the `?` special macro:

      iex> ?j
      106
      iex> ?é
      233

  Or also via pattern matching:

      iex> << eacute :: utf8 >> = "é"
      ...> eacute
      233

  As we have seen above, codepoints can be inserted into
  a string by their hexadecimal code:

      "jos\x{0065}\x{0301}" #=>
      "josé"

  ## Self-synchronization

  The UTF-8 encoding is self-synchronizing. This means that
  if malformed data (i.e., data that is not possible according
  to the definition of the encoding) is encountered, only one
  codepoint needs to be rejected.

  This module relies on this behaviour to ignore such invalid
  characters. For example, `length/1` is going to return
  a correct result even if an invalid codepoint is fed into it.

  In other words, this module expects invalid data to be detected
  when retrieving data from the external source. For example, a
  driver that reads strings from a database will be the one
  responsible to check the validity of the encoding.
  """

  @type t :: binary
  @type codepoint :: t
  @type grapheme :: t

  @doc """
  Checks if a string is printable considering it is encoded
  as UTF-8. Returns `true` if so, `false` otherwise.

  ## Examples

      iex> String.printable?("abc")
      true

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

  @doc %B"""
  Escape the given string. It expects one extra paremeter
  representing the string surrounds which should also be escaped.

  ## Examples

      iex> String.escape("abc", ?")
      "abc"

      iex> String.escape("a\nb", ?")
      "a\\nb"

  """
  def escape(other, char) do
    escape(other, char, <<>>)
  end

  @compile { :inline, escape: 3 }

  defp escape(<<>>, _char, binary), do: binary

  defp escape(<< char, t :: binary >>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, char >>)
  end
  defp escape(<<?#, ?{, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?#, ?{ >>)
  end
  defp escape(<<?\a, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?a >>)
  end
  defp escape(<<?\b, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?b >>)
  end
  defp escape(<<?\d, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?d >>)
  end
  defp escape(<<?\e, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?e >>)
  end
  defp escape(<<?\f, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?f >>)
  end
  defp escape(<<?\n, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?n >>)
  end
  defp escape(<<?\r, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?r >>)
  end
  defp escape(<<?\\, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?\\ >>)
  end
  defp escape(<<?\t, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?t >>)
  end
  defp escape(<<?\v, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, ?\\, ?v >>)
  end
  defp escape(<<h, t :: binary>>, char, binary) do
    escape(t, char, << binary :: binary, h >>)
  end

  @doc """
  Splits a string on substrings at each Unicode whitespace
  occurrence with leading and trailing whitespace ignored.

  ## Examples

      iex> String.split("foo bar")
      ["foo", "bar"]
      iex> String.split("foo" <> <<194, 133>> <> "bar")
      ["foo", "bar"]
      iex> String.split(" foo bar ")
      ["foo", "bar"]

  """
  @spec split(t) :: [t]
  defdelegate split(binary), to: String.Unicode

  @doc """
  Divides a string into substrings based on a pattern,
  returning a list of these substrings. The pattern can
  be a string, a list of strings or a regular expression.

  The string is split into as many parts as possible by
  default, unless the `global` option is set to `false`.

  ## Examples

      iex> String.split("a,b,c", ",")
      ["a", "b", "c"]
      iex> String.split("a,b,c", ",", global: false)
      ["a", "b,c"]

      iex> String.split("1,2 3,4", [" ", ","])
      ["1", "2", "3", "4"]

      iex> String.split("a,b,c", %r{,})
      ["a", "b", "c"]
      iex> String.split("a,b,c", %r{,}, global: false)
      ["a", "b,c"]
      iex> String.split("a,b", %r{\\.})
      ["a,b"]

  """
  @spec split(t, t | [t] | Regex.t) :: [t]
  @spec split(t, t | [t] | Regex.t, Keyword.t) :: [t]
  def split(binary, pattern, options // [])

  def split("", _pattern, _options), do: [""]

  def split(binary, pattern, options) when is_regex(pattern) do
    Regex.split(pattern, binary, global: options[:global])
  end

  def split(binary, pattern, options) do
    opts = if options[:global] != false, do: [:global], else: []
    :binary.split(binary, pattern, opts)
  end

  @doc """
  Convert all characters on the given string to uppercase.

  ## Examples

      iex> String.upcase("abcd")
      "ABCD"
      iex> String.upcase("ab 123 xpto")
      "AB 123 XPTO"
      iex> String.upcase("josé")
      "JOSÉ"

  """
  @spec upcase(t) :: t
  defdelegate upcase(binary), to: String.Unicode

  @doc """
  Convert all characters on the given string to lowercase.

  ## Examples

      iex> String.downcase("ABCD")
      "abcd"
      iex> String.downcase("AB 123 XPTO")
      "ab 123 xpto"
      iex> String.downcase("JOSÉ")
      "josé"

  """
  @spec downcase(t) :: t
  defdelegate downcase(binary), to: String.Unicode

  @doc """
  Converts the first character in the given string to
  uppercase and the remaining to lowercase.

  This relies on the titlecase information provided
  by the Unicode Standard. Note this function makes
  no attempt to capitalize all words in the string
  (usually known as titlecase).

  ## Examples

      iex> String.capitalize("abcd")
      "Abcd"
      iex> String.capitalize("ﬁn")
      "Fin"
      iex> String.capitalize("josé")
      "José"

  """
  @spec capitalize(t) :: t
  def capitalize(string) when is_binary(string) do
    { char, rest } = String.Unicode.titlecase_once(string)
    char <> downcase(rest)
  end

  @doc """
  Returns a string where trailing Unicode whitespace
  has been removed.

  ## Examples

      iex> String.rstrip("   abc  ")
      "   abc"

  """
  @spec rstrip(t) :: t
  defdelegate rstrip(binary), to: String.Unicode

  @doc """
  Returns a string where trailing `char` have been removed.

  ## Examples

      iex> String.rstrip("   abc _", ?_)
      "   abc "

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
  Returns a string where leading Unicode whitespace
  has been removed.

  ## Examples

      iex> String.lstrip("   abc  ")
      "abc  "

  """
  defdelegate lstrip(binary), to: String.Unicode

  @doc """
  Returns a string where leading `char` have been removed.

  ## Examples

      iex> String.lstrip("_  abc  _", ?_)
      "  abc  _"

  """

  @spec lstrip(t, char) :: t

  def lstrip(<<char, rest :: binary>>, char) do
    <<lstrip(rest, char) :: binary>>
  end

  def lstrip(other, _char) do
    other
  end

  @doc """
  Returns a string where leading/trailing Unicode whitespace
  has been removed.

  ## Examples

      iex> String.strip("   abc  ")
      "abc"

  """
  @spec strip(t) :: t

  def strip(string) do
    rstrip(lstrip(string))
  end

  @doc """
  Returns a string where leading/trailing `char` have been
  removed.

  ## Examples

      iex> String.strip("a  abc  a", ?a)
      "  abc  "

  """
  @spec strip(t, char) :: t

  def strip(string, char) do
    rstrip(lstrip(string, char), char)
  end

  @doc """
  Returns a new binary based on `subject` by replacing the parts
  matching `pattern` for `replacement`. By default, it replaces
  all entries, except if the `global` option is set to `false`.

  If the replaced part must be used in `replacement`, then the
  position or the positions where it is to be inserted must be
  specified by using the option `insert_replaced`.

  ## Examples

      iex> String.replace("a,b,c", ",", "-")
      "a-b-c"
      iex> String.replace("a,b,c", ",", "-", global: false)
      "a-b,c"
      iex> String.replace("a,b,c", "b", "[]", insert_replaced: 1)
      "a,[b],c"
      iex> String.replace("a,b,c", ",", "[]", insert_replaced: 2)
      "a[],b[],c"
      iex> String.replace("a,b,c", ",", "[]", insert_replaced: [1, 1])
      "a[,,]b[,,]c"

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
      opts = [{:insert_replaced, insert}|opts]
    end

    opts
  end

  @doc """
  Reverses the given string. Works on graphemes.

  ## Examples

      iex> String.reverse("abcd")
      "dcba"
      iex> String.reverse("hello world")
      "dlrow olleh"
      iex> String.reverse("hello ∂og")
      "go∂ olleh"

  """
  @spec reverse(t) :: t
  def reverse(string) do
    do_reverse(String.Unicode.next_grapheme(string), [])
  end

  defp do_reverse({grapheme, rest}, acc) do
    do_reverse(String.Unicode.next_grapheme(rest), [grapheme|acc])
  end

  defp do_reverse(:no_grapheme, acc), do: list_to_binary(acc)

  @doc """
  Returns a binary `subject` duplicated `n` times.

  ## Examples

      iex> String.duplicate("abc", 0)
      ""
      iex> String.duplicate("abc", 1)
      "abc"
      iex> String.duplicate("abc", 2)
      "abcabc"

  """
  @spec duplicate(t, pos_integer) :: t
  def duplicate(subject, n) when is_integer(n) and n >= 0 do
    :binary.copy(subject, n)
  end

  @doc """
  Returns all codepoints in the string.

  ## Examples

      iex> String.codepoints("josé")
      ["j", "o", "s", "é"]
      iex> String.codepoints("оптими зации")
      ["о","п","т","и","м","и"," ","з","а","ц","и","и"]
      iex> String.codepoints("ἅἪῼ")
      ["ἅ","Ἢ","ῼ"]

  """
  @spec codepoints(t) :: [codepoint]
  defdelegate codepoints(string), to: String.Unicode

  @doc """
  Returns the next codepoint in a String.

  The result is a tuple with the codepoint and the
  remaining of the string or `:no_codepoint` in case
  the string reached its end.

  As with other functions in the String module, this
  function does not check for the validity of the codepoint.
  That said, if an invalid codepoint is found, it will
  be returned by this function.

  ## Examples

      iex> String.next_codepoint("josé")
      { "j", "osé" }

  """
  @spec next_codepoint(t) :: {codepoint, t} | :no_codepoint
  defdelegate next_codepoint(string), to: String.Unicode

  @doc %B"""
  Checks whether `str` contains only valid characters.

  ## Examples

      iex> String.valid?("a")
      true
      iex> String.valid?("ø")
      true
      iex> String.valid?(<<0xffff :: 16>>)
      false
      iex> String.valid?("asd" <> <<0xffff :: 16>>)
      false

  """
  @spec valid?(t) :: boolean

  noncharacters = Enum.to_list(?\x{FDD0}..?\x{FDEF}) ++
    [ ?\x{0FFFE}, ?\x{0FFFF}, ?\x{1FFFE}, ?\x{1FFFF}, ?\x{2FFFE}, ?\x{2FFFF},
      ?\x{3FFFE}, ?\x{3FFFF}, ?\x{4FFFE}, ?\x{4FFFF}, ?\x{5FFFE}, ?\x{5FFFF},
      ?\x{6FFFE}, ?\x{6FFFF}, ?\x{7FFFE}, ?\x{7FFFF}, ?\x{8FFFE}, ?\x{8FFFF},
      ?\x{9FFFE}, ?\x{9FFFF}, ?\x{10FFFE}, ?\x{10FFFF} ]

  lc noncharacter inlist noncharacters do
    def valid?(<< unquote(noncharacter) :: utf8, _ :: binary >>), do: false
  end

  def valid?(<<_ :: utf8, t :: binary>>), do: valid?(t)
  def valid?(<<>>), do: true
  def valid?(_), do: false

  @doc %B"""
  Checks whether `str` is a valid character.

  All characters are codepoints, but some codepoints
  are not valid characters. They may be reserved, private,
  or other.

  More info at: http://en.wikipedia.org/wiki/Mapping_of_Unicode_characters#Noncharacters

  ## Examples

      iex> String.valid_character?("a")
      true
      iex> String.valid_character?("ø")
      true
      iex> String.valid_character?("\x{ffff}")
      false

  """
  @spec valid_character?(t) :: boolean

  def valid_character?(<<_ :: utf8>> = codepoint), do: valid?(codepoint)
  def valid_character?(_), do: false

  @doc %B"""
  Checks whether `str` is a valid codepoint.

  Note that the empty string is considered invalid, as are
  strings containing multiple codepoints.

  ## Examples

      iex> String.valid_codepoint?("a")
      true
      iex> String.valid_codepoint?("ø")
      true
      iex> String.valid_codepoint?(<<0xffff :: 16>>)
      false
      iex> String.valid_codepoint?("asdf")
      false

  """
  @spec valid_codepoint?(codepoint) :: boolean
  def valid_codepoint?(<<_ :: utf8>>), do: true
  def valid_codepoint?(_), do: false

  @doc """
  Returns unicode graphemes in the string.

  ## Examples

      iex> String.graphemes("Ā̀stute")
      ["Ā̀","s","t","u","t","e"]

  """
  @spec graphemes(t) :: [grapheme]
  defdelegate graphemes(string), to: String.Unicode

  @doc """
  Returns the next grapheme in a String.

  The result is a tuple with the grapheme and the
  remaining of the string or `:no_grapheme` in case
  the String reached its end.

  ## Examples

      iex> String.next_grapheme("josé")
      { "j", "osé" }

  """
  @spec next_grapheme(t) :: { grapheme, t } | :no_grapheme
  defdelegate next_grapheme(string), to: String.Unicode

  @doc """
  Returns the first grapheme from an utf8 string,
  nil if the string is empty.

  ## Examples

      iex> String.first("elixir")
      "e"
      iex> String.first("եոգլի")
      "ե"

  """
  @spec first(t) :: grapheme | nil
  def first(string) do
    case next_grapheme(string) do
      { char, _ } -> char
      :no_grapheme -> nil
    end
  end

  @doc """
  Returns the last grapheme from an utf8 string,
  `nil` if the string is empty.

  ## Examples

      iex> String.last("elixir")
      "r"
      iex> String.last("եոգլի")
      "ի"

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

      iex> String.length("elixir")
      6
      iex> String.length("եոգլի")
      5

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

      iex> String.at("elixir", 0)
      "e"
      iex> String.at("elixir", 1)
      "l"
      iex> String.at("elixir", 10)
      nil
      iex> String.at("elixir", -1)
      "r"
      iex> String.at("elixir", -10)
      nil

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
  If the offset is greater than string length, than it returns `nil`.

  ## Examples

      iex> String.slice("elixir", 1, 3)
      "lix"
      iex> String.slice("elixir", 1, 10)
      "lixir"
      iex> String.slice("elixir", 10, 3)
      nil
      iex> String.slice("elixir", -4, 4)
      "ixir"
      iex> String.slice("elixir", -10, 3)
      nil
      iex> String.slice("a", 0, 1500)
      "a"
      iex> String.slice("a", 1, 1500)
      ""
      iex> String.slice("a", 2, 1500)
      nil

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

  defp do_slice(:no_grapheme, start_pos, _, current_pos, acc) when start_pos == current_pos do
    acc
  end

  defp do_slice(:no_grapheme, _, _, _, acc) do
    case acc do
      "" -> nil
      _ -> acc
    end
  end

  @doc """
  Converts a string to an integer. If successful, returns a
  tuple of the form `{integer, remainder of string}`. If unsuccessful,
  returns `:error`.

  ## Examples

      iex> String.to_integer("34")
      {34,""}
      iex> String.to_integer("34.5")
      {34,".5"}
      iex> String.to_integer("three")
      :error

  """
  @spec to_integer(t) :: {integer, t} | :error

  def to_integer(string) do
    {result, remainder} = :string.to_integer(binary_to_list(string))
    case result do
      :error -> :error
      _ -> {result, list_to_binary(remainder)}
    end
  end

  @doc """
  Converts a string to a float. If successful, returns a
  tuple of the form `{float, remainder of string}`. If unsuccessful,
  returns `:error`. If given an integer value, will return
  the same value as `to_integer/1`.

  ## Examples

      iex> String.to_float("34")
      {34.0,""}
      iex> String.to_float("34.25")
      {34.25,""}
      iex> String.to_float("56.5xyz")
      {56.5,"xyz"}
      iex> String.to_float("pi")
      :error

  """
  @spec to_float(t) :: {integer, t} | :error

  def to_float(string) do
    charlist = binary_to_list(string)
    {result, remainder} = :string.to_float(charlist)
    case result do
      :error ->
        {int_result, int_remainder} = :string.to_integer(charlist)
        case int_result do
          :error -> :error
          _ -> {:erlang.float(int_result), list_to_binary(int_remainder)}
        end
      _ -> {result, list_to_binary(remainder)}
    end
  end

  @doc """
  Returns `true` if `string` starts with any of the prefixes given, otherwise
  `false`. `prefixes` can be either a single prefix or a list of prefixes.

  ## Examples

      iex> String.starts_with? "elixir", "eli"
      true
      iex> String.starts_with? "elixir", ["erlang", "elixir"]
      true
      iex> String.starts_with? "elixir", ["erlang", "ruby"]
      false

  """
  @spec starts_with?(t, t | [t]) :: boolean

  def starts_with?(string, prefixes) when is_list(prefixes) do
    Enum.any?(prefixes, do_starts_with(string, &1))
  end

  def starts_with?(string, prefix) do
    do_starts_with(string, prefix)
  end

  defp do_starts_with(string, "") when is_binary(string) do
    true
  end

  defp do_starts_with(string, prefix) when is_binary(prefix) do
    match?({0, _}, :binary.match(string, prefix))
  end

  defp do_starts_with(_, _) do
    raise ArgumentError
  end

  @doc """
  Returns `true` if `string` ends with any of the suffixes given, otherwise
  `false`. `suffixes` can be either a single suffix or a list of suffixes.

  ## Examples

      iex> String.ends_with? "language", "age"
      true
      iex> String.ends_with? "language", ["youth", "age"]
      true
      iex> String.ends_with? "language", ["youth", "elixir"]
      false

  """
  @spec ends_with?(t, t | [t]) :: boolean

  def ends_with?(string, suffixes) when is_list(suffixes) do
    Enum.any?(suffixes, do_ends_with(string, &1))
  end

  def ends_with?(string, suffix) do
    do_ends_with(string, suffix)
  end

  defp do_ends_with(string, "") when is_binary(string) do
    true
  end

  defp do_ends_with(string, suffix) when is_binary(suffix) do
    string_size = size(string)
    suffix_size = size(suffix)
    scope = {string_size - suffix_size, suffix_size}
    (suffix_size <= string_size) and (:nomatch != :binary.match(string, suffix, [scope: scope]))
  end

  defp do_ends_with(_, _) do
    raise ArgumentError
  end

  @doc """
  Returns `true` if `string` contains match, otherwise `false`.
  `matches` can be either a single string or a list of strings.

  ## Examples

      iex> String.contains? "elixir of life", "of"
      true
      iex> String.contains? "elixir of life", ["life", "death"]
      true
      iex> String.contains? "elixir of life", ["death", "mercury"]
      false

  """
  @spec contains?(t, t | [t]) :: boolean

  def contains?(string, matches) when is_list(matches) do
    Enum.any?(matches, do_contains(string, &1))
  end

  def contains?(string, match) do
    do_contains(string, match)
  end

  defp do_contains(string, "") when is_binary(string) do
    true
  end

  defp do_contains(string, match) when is_binary(match) do
    :nomatch != :binary.match(string, match)
  end

  defp do_contains(_, _) do
    raise ArgumentError
  end

  defexception UnicodeConversionError, encoded: nil, rest: nil, kind: nil do
    def message(exception) do
      "#{exception.kind} #{detail(exception.rest)}"
    end

    defp detail(rest) when is_binary(rest) do
      "encoding starting at #{inspect rest}"
    end

    defp detail([h|_]) do
      "code point #{h}"
    end
  end

  @doc """
  Converts a string into a char list converting each codepoint to its
  respective integer value.

  ## Examples

      iex> String.to_char_list("æß")
      { :ok, 'æß' }
      iex> String.to_char_list("abc")
      { :ok, 'abc' }

  """
  @spec to_char_list(String.t) :: { :ok, char_list } | { :error, list, binary } | { :incomplete, list, binary }
  def to_char_list(string) do
    case :unicode.characters_to_list(string) do
      result when is_list(result) ->
        { :ok, result }

      { :error, _, _ } = error ->
        error

      { :incomplete, _, _ } = incomplete ->
        incomplete
    end
  end

  @doc """
  Converts a string into a char list converting each codepoint to its
  respective integer value.

  In case the conversion fails or is incomplete,
  it raises a `String.UnicodeConversionError`.

  ## Examples

      iex> String.to_char_list!("æß")
      'æß'
      iex> String.to_char_list!("abc")
      'abc'

  """
  @spec to_char_list!(String.t) :: char_list | no_return
  def to_char_list!(string) do
    case :unicode.characters_to_list(string) do
      result when is_list(result) ->
        result

      { :error, encoded, rest } ->
        raise UnicodeConversionError, encoded: encoded, rest: rest, kind: :invalid

      { :incomplete, encoded, rest } ->
        raise UnicodeConversionError, encoded: encoded, rest: rest, kind: :incomplete
    end
  end

  @doc """
  Converts a list of integer codepoints to a string.

  ## Examples

      iex> String.from_char_list([0x00E6, 0x00DF])
      { :ok, "æß" }
      iex> String.from_char_list([0x0061, 0x0062, 0x0063])
      { :ok, "abc" }

  """
  @spec from_char_list(char_list) :: { :ok, String.t } | { :error, binary, binary } | { :incomplete, binary, binary }
  def from_char_list(list) do
    case :unicode.characters_to_binary(list) do
      result when is_binary(result) ->
        { :ok, result }

      { :error, _, _ } = error ->
        error

      { :incomplete, _, _ } = incomplete ->
        incomplete
    end
  end

  @doc """
  Converts a list of integer codepoints to a string.

  In case the conversion fails, it raises a `String.UnicodeConversionError`.

  ## Examples

      iex> String.from_char_list!([0x00E6, 0x00DF])
      "æß"
      iex> String.from_char_list!([0x0061, 0x0062, 0x0063])
      "abc"

  """
  @spec from_char_list!(char_list) :: String.t | no_return
  def from_char_list!(list) do
    case :unicode.characters_to_binary(list) do
      result when is_binary(result) ->
        result

      { :error, encoded, rest } ->
        raise UnicodeConversionError, encoded: encoded, rest: rest, kind: :invalid

      { :incomplete, encoded, rest } ->
        raise UnicodeConversionError, encoded: encoded, rest: rest, kind: :incomplete
    end
  end
end
