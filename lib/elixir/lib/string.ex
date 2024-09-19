import Kernel, except: [length: 1]

defmodule String do
  @moduledoc ~S"""
  Strings in Elixir are UTF-8 encoded binaries.

  Strings in Elixir are a sequence of Unicode characters,
  typically written between double quoted strings, such
  as `"hello"` and `"héllò"`.

  In case a string must have a double-quote in itself,
  the double quotes must be escaped with a backslash,
  for example: `"this is a string with \"double quotes\""`.

  You can concatenate two strings with the `<>/2` operator:

      iex> "hello" <> " " <> "world"
      "hello world"

  The functions in this module act according to
  [The Unicode Standard, Version 15.1.0](http://www.unicode.org/versions/Unicode15.1.0/).

  ## Interpolation

  Strings in Elixir also support interpolation. This allows
  you to place some value in the middle of a string by using
  the `#{}` syntax:

      iex> name = "joe"
      iex> "hello #{name}"
      "hello joe"

  Any Elixir expression is valid inside the interpolation.
  If a string is given, the string is interpolated as is.
  If any other value is given, Elixir will attempt to convert
  it to a string using the `String.Chars` protocol. This
  allows, for example, to output an integer from the interpolation:

      iex> "2 + 2 = #{2 + 2}"
      "2 + 2 = 4"

  In case the value you want to interpolate cannot be
  converted to a string, because it doesn't have a human
  textual representation, a protocol error will be raised.

  ## Escape characters

  Besides allowing double-quotes to be escaped with a backslash,
  strings also support the following escape characters:

    * `\0` - Null byte
    * `\a` - Bell
    * `\b` - Backspace
    * `\t` - Horizontal tab
    * `\n` - Line feed (New lines)
    * `\v` - Vertical tab
    * `\f` - Form feed
    * `\r` - Carriage return
    * `\e` - Command Escape
    * `\s` - Space
    * `\#` - Returns the `#` character itself, skipping interpolation
    * `\\` - Single backslash
    * `\xNN` - A byte represented by the hexadecimal `NN`
    * `\uNNNN` - A Unicode code point represented by `NNNN`
    * `\u{NNNNNN}` - A Unicode code point represented by `NNNNNN`

  Note it is generally not advised to use `\xNN` in Elixir
  strings, as introducing an invalid byte sequence would
  make the string invalid. If you have to introduce a
  character by its hexadecimal representation, it is best
  to work with Unicode code points, such as `\uNNNN`. In fact,
  understanding Unicode code points can be essential when doing
  low-level manipulations of string, so let's explore them in
  detail next.

  ## Unicode and code points

  In order to facilitate meaningful communication between computers
  across multiple languages, a standard is required so that the ones
  and zeros on one machine mean the same thing when they are transmitted
  to another. The Unicode Standard acts as an official registry of
  virtually all the characters we know: this includes characters from
  classical and historical texts, emoji, and formatting and control
  characters as well.

  Unicode organizes all of the characters in its repertoire into code
  charts, and each character is given a unique numerical index. This
  numerical index is known as a Code Point.

  In Elixir you can use a `?` in front of a character literal to reveal
  its code point:

      iex> ?a
      97
      iex> ?ł
      322

  Note that most Unicode code charts will refer to a code point by its
  hexadecimal (hex) representation, e.g. `97` translates to `0061` in hex,
  and we can represent any Unicode character in an Elixir string by
  using the `\u` escape character followed by its code point number:

      iex> "\u0061" === "a"
      true
      iex> 0x0061 = 97 = ?a
      97

  The hex representation will also help you look up information about a
  code point, e.g. [https://codepoints.net/U+0061](https://codepoints.net/U+0061)
  has a data sheet all about the lower case `a`, a.k.a. code point 97.
  Remember you can get the hex presentation of a number by calling
  `Integer.to_string/2`:

      iex> Integer.to_string(?a, 16)
      "61"

  ## UTF-8 encoded and encodings

  Now that we understand what the Unicode standard is and what code points
  are, we can finally talk about encodings. Whereas the code point is **what**
  we store, an encoding deals with **how** we store it: encoding is an
  implementation. In other words, we need a mechanism to convert the code
  point numbers into bytes so they can be stored in memory, written to disk, and such.

  Elixir uses UTF-8 to encode its strings, which means that code points are
  encoded as a series of 8-bit bytes. UTF-8 is a **variable width** character
  encoding that uses one to four bytes to store each code point. It is capable
  of encoding all valid Unicode code points. Let's see an example:

      iex> string = "héllo"
      "héllo"
      iex> String.length(string)
      5
      iex> byte_size(string)
      6

  Although the string above has 5 characters, it uses 6 bytes, as two bytes
  are used to represent the character `é`.

  ## Grapheme clusters

  This module also works with the concept of grapheme cluster
  (from now on referenced as graphemes). Graphemes can consist
  of multiple code points that may be perceived as a single character
  by readers. For example, "é" can be represented either as a single
  "e with acute" code point, as seen above in the string `"héllo"`,
  or as the letter "e" followed by a "combining acute accent"
  (two code points):

      iex> string = "\u0065\u0301"
      "é"
      iex> byte_size(string)
      3
      iex> String.length(string)
      1
      iex> String.codepoints(string)
      ["e", "́"]
      iex> String.graphemes(string)
      ["é"]

  Although it looks visually the same as before, the example above
  is made of two characters, it is perceived by users as one.

  Graphemes can also be two characters that are interpreted as one
  by some languages. For example, some languages may consider "ch"
  as a single character. However, since this information depends on
  the locale, it is not taken into account by this module.

  In general, the functions in this module rely on the Unicode
  Standard, but do not contain any of the locale specific behavior.
  More information about graphemes can be found in the [Unicode
  Standard Annex #29](https://www.unicode.org/reports/tr29/).

  For converting a binary to a different encoding and for Unicode
  normalization mechanisms, see Erlang's `:unicode` module.

  ## String and binary operations

  To act according to the Unicode Standard, many functions
  in this module run in linear time, as they need to traverse
  the whole string considering the proper Unicode code points.

  For example, `String.length/1` will take longer as
  the input grows. On the other hand, `Kernel.byte_size/1` always runs
  in constant time (i.e. regardless of the input size).

  This means often there are performance costs in using the
  functions in this module, compared to the more low-level
  operations that work directly with binaries:

    * `Kernel.binary_part/3` - retrieves part of the binary
    * `Kernel.bit_size/1` and `Kernel.byte_size/1` - size related functions
    * `Kernel.is_bitstring/1` and `Kernel.is_binary/1` - type-check function
    * Plus a number of functions for working with binaries (bytes)
      in the [`:binary` module](`:binary`)

  A `utf8` modifier is also available inside the binary syntax `<<>>`.
  It can be used to match code points out of a binary/string:

      iex> <<eacute::utf8>> = "é"
      iex> eacute
      233

  See the [*Patterns and Guards* guide](patterns-and-guards.md) and the documentation for
  [`<<>>`](`<<>>/1`) for more information on binary pattern matching.

  You can also fully convert a string into a list of integer code points,
  known as "charlists" in Elixir, by calling `String.to_charlist/1`:

      iex> String.to_charlist("héllo")
      [104, 233, 108, 108, 111]

  If you would rather see the underlying bytes of a string, instead of
  its codepoints, a common trick is to concatenate the null byte `<<0>>`
  to it:

      iex> "héllo" <> <<0>>
      <<104, 195, 169, 108, 108, 111, 0>>

  Alternatively, you can view a string's binary representation by
  passing an option to `IO.inspect/2`:

      IO.inspect("héllo", binaries: :as_binaries)
      #=> <<104, 195, 169, 108, 108, 111>>

  ## Self-synchronization

  The UTF-8 encoding is self-synchronizing. This means that
  if malformed data (i.e., data that is not possible according
  to the definition of the encoding) is encountered, only one
  code point needs to be rejected.

  This module relies on this behavior to ignore such invalid
  characters. For example, `length/1` will return
  a correct result even if an invalid code point is fed into it.

  In other words, this module expects invalid data to be detected
  elsewhere, usually when retrieving data from the external source.
  For example, a driver that reads strings from a database will be
  responsible to check the validity of the encoding. `String.chunk/2`
  can be used for breaking a string into valid and invalid parts.

  ## Compile binary patterns

  Many functions in this module work with patterns. For example,
  `String.split/3` can split a string into multiple strings given
  a pattern. This pattern can be a string, a list of strings or
  a compiled pattern:

      iex> String.split("foo bar", " ")
      ["foo", "bar"]

      iex> String.split("foo bar!", [" ", "!"])
      ["foo", "bar", ""]

      iex> pattern = :binary.compile_pattern([" ", "!"])
      iex> String.split("foo bar!", pattern)
      ["foo", "bar", ""]

  The compiled pattern is useful when the same match will
  be done over and over again. Note though that the compiled
  pattern cannot be stored in a module attribute as the pattern
  is generated at runtime and does not survive compile time.
  """

  @typedoc """
  A UTF-8 encoded binary.

  The types `String.t()` and `binary()` are equivalent to analysis tools.
  Although, for those reading the documentation, `String.t()` implies
  it is a UTF-8 encoded binary.
  """
  @type t :: binary

  @typedoc "A single Unicode code point encoded in UTF-8. It may be one or more bytes."
  @type codepoint :: t

  @typedoc "Multiple code points that may be perceived as a single character by readers"
  @type grapheme :: t

  @typedoc """
  Pattern used in functions like `replace/4` and `split/3`.

  It must be one of:

    * a string
    * an empty list
    * a list containing non-empty strings
    * a compiled search pattern created by `:binary.compile_pattern/1`

  """
  @type pattern ::
          t()
          | [nonempty_binary]
          | (compiled_search_pattern :: :binary.cp())

  @conditional_mappings [:greek, :turkic]

  @doc """
  Checks if a string contains only printable characters up to `character_limit`.

  Takes an optional `character_limit` as a second argument. If `character_limit` is `0`, this
  function will return `true`.

  ## Examples

      iex> String.printable?("abc")
      true

      iex> String.printable?("abc" <> <<0>>)
      false

      iex> String.printable?("abc" <> <<0>>, 2)
      true

      iex> String.printable?("abc" <> <<0>>, 0)
      true

  """
  @spec printable?(t, 0) :: true
  @spec printable?(t, pos_integer | :infinity) :: boolean
  def printable?(string, character_limit \\ :infinity)
      when is_binary(string) and
             (character_limit == :infinity or
                (is_integer(character_limit) and character_limit >= 0)) do
    recur_printable?(string, character_limit)
  end

  defp recur_printable?(_string, 0), do: true
  defp recur_printable?(<<>>, _character_limit), do: true

  for char <- 0x20..0x7E do
    defp recur_printable?(<<unquote(char), rest::binary>>, character_limit) do
      recur_printable?(rest, decrement(character_limit))
    end
  end

  for char <- [?\n, ?\r, ?\t, ?\v, ?\b, ?\f, ?\e, ?\d, ?\a] do
    defp recur_printable?(<<unquote(char), rest::binary>>, character_limit) do
      recur_printable?(rest, decrement(character_limit))
    end
  end

  defp recur_printable?(<<char::utf8, rest::binary>>, character_limit)
       when char in 0xA0..0xD7FF
       when char in 0xE000..0xFFFD
       when char in 0x10000..0x10FFFF do
    recur_printable?(rest, decrement(character_limit))
  end

  defp recur_printable?(_string, _character_limit) do
    false
  end

  defp decrement(:infinity), do: :infinity
  defp decrement(character_limit), do: character_limit - 1

  @doc ~S"""
  Divides a string into substrings at each Unicode whitespace
  occurrence with leading and trailing whitespace ignored.

  Groups of whitespace are treated as a single occurrence.
  Divisions do not occur on non-breaking whitespace.

  ## Examples

      iex> String.split("foo bar")
      ["foo", "bar"]

      iex> String.split("foo" <> <<194, 133>> <> "bar")
      ["foo", "bar"]

      iex> String.split(" foo   bar ")
      ["foo", "bar"]

      iex> String.split("no\u00a0break")
      ["no\u00a0break"]

  Removes empty strings, like when using `trim: true` in `String.split/3`.

      iex> String.split(" ")
      []

  """
  @spec split(t) :: [t]
  defdelegate split(binary), to: String.Break

  @doc ~S"""
  Divides a string into parts based on a pattern.

  Returns a list of these parts.

  The `pattern` may be a string, a list of strings, a regular expression, or a
  compiled pattern.

  The string is split into as many parts as possible by
  default, but can be controlled via the `:parts` option.

  Empty strings are only removed from the result if the
  `:trim` option is set to `true`.

  When the pattern used is a regular expression, the string is
  split using `Regex.split/3`.

  If the pattern cannot be found, a list containing the original
  string will be returned.

  ## Options

    * `:parts` (positive integer or `:infinity`) - the string
      is split into at most as many parts as this option specifies.
      If `:infinity`, the string will be split into all possible
      parts. Defaults to `:infinity`.

    * `:trim` (boolean) - if `true`, empty strings are removed from
      the resulting list.

  This function also accepts all options accepted by `Regex.split/3`
  if `pattern` is a regular expression.

  ## Examples

  Splitting with a string pattern:

      iex> String.split("a,b,c", ",")
      ["a", "b", "c"]

      iex> String.split("a,b,c", ",", parts: 2)
      ["a", "b,c"]

      iex> String.split(" a b c ", " ", trim: true)
      ["a", "b", "c"]

  A list of patterns:

      iex> String.split("1,2 3,4", [" ", ","])
      ["1", "2", "3", "4"]

  A regular expression:

      iex> String.split("a,b,c", ~r{,})
      ["a", "b", "c"]

      iex> String.split("a,b,c", ~r{,}, parts: 2)
      ["a", "b,c"]

      iex> String.split(" a b c ", ~r{\s}, trim: true)
      ["a", "b", "c"]

      iex> String.split("abc", ~r{b}, include_captures: true)
      ["a", "b", "c"]

  A compiled pattern:

      iex> pattern = :binary.compile_pattern([" ", ","])
      iex> String.split("1,2 3,4", pattern)
      ["1", "2", "3", "4"]

  Splitting on empty string returns graphemes:

      iex> String.split("abc", "")
      ["", "a", "b", "c", ""]

      iex> String.split("abc", "", trim: true)
      ["a", "b", "c"]

      iex> String.split("abc", "", parts: 1)
      ["abc"]

      iex> String.split("abc", "", parts: 3)
      ["", "a", "bc"]

  Splitting on an non-existing pattern returns the original string:

      iex> String.split("abc", ",")
      ["abc"]

  Be aware that this function can split within or across grapheme boundaries.
  For example, take the grapheme "é" which is made of the characters
  "e" and the acute accent. The following will split the string into two parts:

      iex> String.split(String.normalize("é", :nfd), "e")
      ["", "́"]

  However, if "é" is represented by the single character "e with acute"
  accent, then it will split the string into just one part:

      iex> String.split(String.normalize("é", :nfc), "e")
      ["é"]

  """
  @spec split(t, pattern | Regex.t(), keyword) :: [t]
  def split(string, pattern, options \\ [])

  def split(string, %Regex{} = pattern, options) when is_binary(string) and is_list(options) do
    Regex.split(pattern, string, options)
  end

  def split(string, "", options) when is_binary(string) and is_list(options) do
    parts = Keyword.get(options, :parts, :infinity)
    index = parts_to_index(parts)
    trim = Keyword.get(options, :trim, false)

    if trim == false and index != 1 do
      ["" | split_empty(string, trim, index - 1)]
    else
      split_empty(string, trim, index)
    end
  end

  def split(string, [], options) when is_binary(string) and is_list(options) do
    if string == "" and Keyword.get(options, :trim, false) do
      []
    else
      [string]
    end
  end

  def split(string, pattern, options) when is_binary(string) and is_list(options) do
    parts = Keyword.get(options, :parts, :infinity)
    trim = Keyword.get(options, :trim, false)

    case {parts, trim} do
      {:infinity, false} ->
        :binary.split(string, pattern, [:global])

      {:infinity, true} ->
        :binary.split(string, pattern, [:global, :trim_all])

      {2, false} ->
        :binary.split(string, pattern)

      _ ->
        pattern = maybe_compile_pattern(pattern)
        split_each(string, pattern, trim, parts_to_index(parts))
    end
  end

  defp parts_to_index(:infinity), do: 0
  defp parts_to_index(n) when is_integer(n) and n > 0, do: n

  defp split_empty("", true, 1), do: []
  defp split_empty(string, _, 1), do: [IO.iodata_to_binary(string)]

  defp split_empty(string, trim, count) do
    case :unicode_util.gc(string) do
      [gc] -> [grapheme_to_binary(gc) | split_empty("", trim, 1)]
      [gc | rest] -> [grapheme_to_binary(gc) | split_empty(rest, trim, count - 1)]
      [] -> split_empty("", trim, 1)
      {:error, <<byte, rest::bits>>} -> [<<byte>> | split_empty(rest, trim, count - 1)]
    end
  end

  defp split_each("", _pattern, true, 1), do: []
  defp split_each(string, _pattern, _trim, 1) when is_binary(string), do: [string]

  defp split_each(string, pattern, trim, count) do
    case do_splitter(string, pattern, trim) do
      {h, t} -> [h | split_each(t, pattern, trim, count - 1)]
      nil -> []
    end
  end

  @doc """
  Returns an enumerable that splits a string on demand.

  This is in contrast to `split/3` which splits the
  entire string upfront.

  This function does not support regular expressions
  by design. When using regular expressions, it is often
  more efficient to have the regular expressions traverse
  the string at once than in parts, like this function does.

  ## Options

    * :trim - when `true`, does not emit empty patterns

  ## Examples

      iex> String.splitter("1,2 3,4 5,6 7,8,...,99999", [" ", ","]) |> Enum.take(4)
      ["1", "2", "3", "4"]

      iex> String.splitter("abcd", "") |> Enum.take(10)
      ["", "a", "b", "c", "d", ""]

      iex> String.splitter("abcd", "", trim: true) |> Enum.take(10)
      ["a", "b", "c", "d"]

  A compiled pattern can also be given:

      iex> pattern = :binary.compile_pattern([" ", ","])
      iex> String.splitter("1,2 3,4 5,6 7,8,...,99999", pattern) |> Enum.take(4)
      ["1", "2", "3", "4"]

  """
  @spec splitter(t, pattern, keyword) :: Enumerable.t()
  def splitter(string, pattern, options \\ [])

  def splitter(string, "", options) when is_binary(string) and is_list(options) do
    if Keyword.get(options, :trim, false) do
      Stream.unfold(string, &next_grapheme/1)
    else
      Stream.unfold(:match, &do_empty_splitter(&1, string))
    end
  end

  def splitter(string, [], options) when is_binary(string) and is_list(options) do
    if string == "" and Keyword.get(options, :trim, false) do
      Stream.duplicate(string, 0)
    else
      Stream.duplicate(string, 1)
    end
  end

  def splitter(string, pattern, options) when is_binary(string) and is_list(options) do
    pattern = maybe_compile_pattern(pattern)
    trim = Keyword.get(options, :trim, false)
    Stream.unfold(string, &do_splitter(&1, pattern, trim))
  end

  defp do_empty_splitter(:match, string), do: {"", string}
  defp do_empty_splitter(:nomatch, _string), do: nil
  defp do_empty_splitter("", _), do: {"", :nomatch}
  defp do_empty_splitter(string, _), do: next_grapheme(string)

  defp do_splitter(:nomatch, _pattern, _), do: nil
  defp do_splitter("", _pattern, false), do: {"", :nomatch}
  defp do_splitter("", _pattern, true), do: nil

  defp do_splitter(bin, pattern, trim) do
    case :binary.split(bin, pattern) do
      ["", second] when trim -> do_splitter(second, pattern, trim)
      [first, second] -> {first, second}
      [first] -> {first, :nomatch}
    end
  end

  defp maybe_compile_pattern(pattern) when is_tuple(pattern), do: pattern
  defp maybe_compile_pattern(pattern), do: :binary.compile_pattern(pattern)

  @doc """
  Splits a string into two at the specified offset. When the offset given is
  negative, location is counted from the end of the string.

  The offset is capped to the length of the string. Returns a tuple with
  two elements.

  > #### Linear Access {: .warning}
  >
  > This function splits on graphemes and for such it has to linearly traverse
  > the string.
  > If you want to split a string or a binary based on the number of bytes,
  > use `Kernel.binary_part/3` instead.

  ## Examples

      iex> String.split_at("sweetelixir", 5)
      {"sweet", "elixir"}

      iex> String.split_at("sweetelixir", -6)
      {"sweet", "elixir"}

      iex> String.split_at("abc", 0)
      {"", "abc"}

      iex> String.split_at("abc", 1000)
      {"abc", ""}

      iex> String.split_at("abc", -1000)
      {"", "abc"}

  """
  @spec split_at(t, integer) :: {t, t}
  def split_at(string, position)

  def split_at(string, position)
      when is_binary(string) and is_integer(position) and position >= 0 do
    do_split_at(string, position)
  end

  def split_at(string, position)
      when is_binary(string) and is_integer(position) and position < 0 do
    position = length(string) + position

    case position >= 0 do
      true -> do_split_at(string, position)
      false -> {"", string}
    end
  end

  defp do_split_at(string, position) do
    remaining = byte_size_remaining_at(string, position)
    start = byte_size(string) - remaining
    <<left::size(^start)-binary, right::size(^remaining)-binary>> = string
    {left, right}
  end

  @doc ~S"""
  Returns `true` if `string1` is canonically equivalent to `string2`.

  It performs Normalization Form Canonical Decomposition (NFD) on the
  strings before comparing them. This function is equivalent to:

      String.normalize(string1, :nfd) == String.normalize(string2, :nfd)

  If you plan to compare multiple strings, multiple times in a row, you
  may normalize them upfront and compare them directly to avoid multiple
  normalization passes.

  ## Examples

      iex> String.equivalent?("abc", "abc")
      true

      iex> String.equivalent?("man\u0303ana", "mañana")
      true

      iex> String.equivalent?("abc", "ABC")
      false

      iex> String.equivalent?("nø", "nó")
      false

  """
  @spec equivalent?(t, t) :: boolean
  def equivalent?(string1, string2) when is_binary(string1) and is_binary(string2) do
    normalize(string1, :nfd) == normalize(string2, :nfd)
  end

  @doc """
  Converts all characters in `string` to Unicode normalization
  form identified by `form`.

  Invalid Unicode codepoints are skipped and the remaining of
  the string is converted. If you want the algorithm to stop
  and return on invalid codepoint, use `:unicode.characters_to_nfd_binary/1`,
  `:unicode.characters_to_nfc_binary/1`, `:unicode.characters_to_nfkd_binary/1`,
  and `:unicode.characters_to_nfkc_binary/1` instead.

  Normalization forms `:nfkc` and `:nfkd` should not be blindly applied
  to arbitrary text. Because they erase many formatting distinctions,
  they will prevent round-trip conversion to and from many legacy
  character sets.

  ## Forms

  The supported forms are:

    * `:nfd` - Normalization Form Canonical Decomposition.
      Characters are decomposed by canonical equivalence, and
      multiple combining characters are arranged in a specific
      order.

    * `:nfc` - Normalization Form Canonical Composition.
      Characters are decomposed and then recomposed by canonical equivalence.

    * `:nfkd` - Normalization Form Compatibility Decomposition.
      Characters are decomposed by compatibility equivalence, and
      multiple combining characters are arranged in a specific
      order.

    * `:nfkc` - Normalization Form Compatibility Composition.
      Characters are decomposed and then recomposed by compatibility equivalence.

  ## Examples

      iex> String.normalize("yêṩ", :nfd)
      "yêṩ"

      iex> String.normalize("leña", :nfc)
      "leña"

      iex> String.normalize("ﬁ", :nfkd)
      "fi"

      iex> String.normalize("fi", :nfkc)
      "fi"

  """
  @spec normalize(t, :nfd | :nfc | :nfkd | :nfkc) :: t
  def normalize(string, form)

  def normalize(string, :nfd) when is_binary(string) do
    case :unicode.characters_to_nfd_binary(string) do
      string when is_binary(string) -> string
      {:error, good, <<head, rest::binary>>} -> good <> <<head>> <> normalize(rest, :nfd)
    end
  end

  def normalize(string, :nfc) when is_binary(string) do
    case :unicode.characters_to_nfc_binary(string) do
      string when is_binary(string) -> string
      {:error, good, <<head, rest::binary>>} -> good <> <<head>> <> normalize(rest, :nfc)
    end
  end

  def normalize(string, :nfkd) when is_binary(string) do
    case :unicode.characters_to_nfkd_binary(string) do
      string when is_binary(string) -> string
      {:error, good, <<head, rest::binary>>} -> good <> <<head>> <> normalize(rest, :nfkd)
    end
  end

  def normalize(string, :nfkc) when is_binary(string) do
    case :unicode.characters_to_nfkc_binary(string) do
      string when is_binary(string) -> string
      {:error, good, <<head, rest::binary>>} -> good <> <<head>> <> normalize(rest, :nfkc)
    end
  end

  @doc """
  Converts all characters in the given string to uppercase according to `mode`.

  `mode` may be `:default`, `:ascii`, `:greek` or `:turkic`. The `:default` mode considers
  all non-conditional transformations outlined in the Unicode standard. `:ascii`
  uppercases only the letters a to z. `:greek` includes the context sensitive
  mappings found in Greek. `:turkic` properly handles the letter i with the dotless variant.

  ## Examples

      iex> String.upcase("abcd")
      "ABCD"

      iex> String.upcase("ab 123 xpto")
      "AB 123 XPTO"

      iex> String.upcase("olá")
      "OLÁ"

  The `:ascii` mode ignores Unicode characters and provides a more
  performant implementation when you know the string contains only
  ASCII characters:

      iex> String.upcase("olá", :ascii)
      "OLá"

  And `:turkic` properly handles the letter i with the dotless variant:

      iex> String.upcase("ıi")
      "II"

      iex> String.upcase("ıi", :turkic)
      "Iİ"

  Also see `downcase/2` and `capitalize/2` for other conversions.
  """
  @spec upcase(t, :default | :ascii | :greek | :turkic) :: t
  def upcase(string, mode \\ :default)

  def upcase("", _mode) do
    ""
  end

  def upcase(string, :default) when is_binary(string) do
    String.Unicode.upcase(string, [], :default)
  end

  def upcase(string, :ascii) when is_binary(string) do
    IO.iodata_to_binary(upcase_ascii(string))
  end

  def upcase(string, mode) when is_binary(string) and mode in @conditional_mappings do
    String.Unicode.upcase(string, [], mode)
  end

  defp upcase_ascii(<<char, rest::bits>>) when char >= ?a and char <= ?z,
    do: [char - 32 | upcase_ascii(rest)]

  defp upcase_ascii(<<char, rest::bits>>), do: [char | upcase_ascii(rest)]
  defp upcase_ascii(<<>>), do: []

  @doc """
  Converts all characters in the given string to lowercase according to `mode`.

  `mode` may be `:default`, `:ascii`, `:greek` or `:turkic`. The `:default` mode considers
  all non-conditional transformations outlined in the Unicode standard. `:ascii`
  lowercases only the letters A to Z. `:greek` includes the context sensitive
  mappings found in Greek. `:turkic` properly handles the letter i with the dotless variant.

  Also see `upcase/2` and `capitalize/2` for other conversions.

  ## Examples

      iex> String.downcase("ABCD")
      "abcd"

      iex> String.downcase("AB 123 XPTO")
      "ab 123 xpto"

      iex> String.downcase("OLÁ")
      "olá"

  The `:ascii` mode ignores Unicode characters and provides a more
  performant implementation when you know the string contains only
  ASCII characters:

      iex> String.downcase("OLÁ", :ascii)
      "olÁ"

  The `:greek` mode properly handles the context sensitive sigma in Greek:

      iex> String.downcase("ΣΣ")
      "σσ"

      iex> String.downcase("ΣΣ", :greek)
      "σς"

  And `:turkic` properly handles the letter i with the dotless variant:

      iex> String.downcase("Iİ")
      "ii̇"

      iex> String.downcase("Iİ", :turkic)
      "ıi"

  """
  @spec downcase(t, :default | :ascii | :greek | :turkic) :: t
  def downcase(string, mode \\ :default)

  def downcase("", _mode) do
    ""
  end

  def downcase(string, :default) when is_binary(string) do
    String.Unicode.downcase(string, [], :default)
  end

  def downcase(string, :ascii) when is_binary(string) do
    IO.iodata_to_binary(downcase_ascii(string))
  end

  def downcase(string, mode) when is_binary(string) and mode in @conditional_mappings do
    String.Unicode.downcase(string, [], mode)
  end

  defp downcase_ascii(<<char, rest::bits>>) when char >= ?A and char <= ?Z,
    do: [char + 32 | downcase_ascii(rest)]

  defp downcase_ascii(<<char, rest::bits>>), do: [char | downcase_ascii(rest)]
  defp downcase_ascii(<<>>), do: []

  @doc """
  Converts the first character in the given string to
  uppercase and the remainder to lowercase according to `mode`.

  `mode` may be `:default`, `:ascii`, `:greek` or `:turkic`. The `:default` mode
  considers all non-conditional transformations outlined in the Unicode standard.
  `:ascii` capitalizes only the letters A to Z. `:greek` includes the context
  sensitive mappings found in Greek. `:turkic` properly handles the letter `i`
  with the dotless variant.

  Also see `upcase/2` and `capitalize/2` for other conversions. If you want
  a variation of this function that does not lowercase the rest of string,
  see Erlang's `:string.titlecase/1`.

  ## Examples

      iex> String.capitalize("abcd")
      "Abcd"
      iex> String.capitalize("ABCD")
      "Abcd"

      iex> String.capitalize("ﬁn")
      "Fin"
      iex> String.capitalize("olá")
      "Olá"

  """
  @spec capitalize(t, :default | :ascii | :greek | :turkic) :: t
  def capitalize(string, mode \\ :default)

  def capitalize(<<char, rest::binary>>, :ascii) do
    char = if char >= ?a and char <= ?z, do: char - 32, else: char
    <<char>> <> downcase(rest, :ascii)
  end

  @letter_I <<0x0049::utf8>>
  @letter_i <<0x0069::utf8>>
  @letter_I_dot_above <<0x0130::utf8>>

  def capitalize(<<@letter_i, right::binary>>, mode) do
    if(mode == :turkic, do: @letter_I_dot_above, else: @letter_I) <> downcase(right, mode)
  end

  def capitalize(string, mode) when is_binary(string) do
    case :unicode_util.gc(string) do
      [gc] -> grapheme_to_binary(:string.titlecase([gc]))
      [gc, rest] -> grapheme_to_binary(:string.titlecase([gc])) <> downcase(rest, mode)
      [gc | rest] -> grapheme_to_binary(:string.titlecase([gc])) <> downcase(rest, mode)
      [] -> ""
      {:error, <<byte, rest::bits>>} -> <<byte>> <> downcase(rest, mode)
    end
  end

  @doc false
  @deprecated "Use String.trim_trailing/1 instead"
  defdelegate rstrip(binary), to: String.Break, as: :trim_trailing

  @doc false
  @deprecated "Use String.trim_trailing/2 with a binary as second argument instead"
  def rstrip(string, char) when is_integer(char) do
    replace_trailing(string, <<char::utf8>>, "")
  end

  @doc """
  Replaces all leading occurrences of `match` by `replacement` of `match` in `string`.

  Returns the string untouched if there are no occurrences.

  If `match` is `""`, this function raises an `ArgumentError` exception: this
  happens because this function replaces **all** the occurrences of `match` at
  the beginning of `string`, and it's impossible to replace "multiple"
  occurrences of `""`.

  ## Examples

      iex> String.replace_leading("hello world", "hello ", "")
      "world"
      iex> String.replace_leading("hello hello world", "hello ", "")
      "world"

      iex> String.replace_leading("hello world", "hello ", "ola ")
      "ola world"
      iex> String.replace_leading("hello hello world", "hello ", "ola ")
      "ola ola world"

  This function can replace across grapheme boundaries. See `replace/3`
  for more information and examples.
  """
  @spec replace_leading(t, t, t) :: t
  def replace_leading(string, match, replacement)
      when is_binary(string) and is_binary(match) and is_binary(replacement) do
    if match == "" do
      raise ArgumentError, "cannot use an empty string as the match to replace"
    end

    prefix_size = byte_size(match)
    suffix_size = byte_size(string) - prefix_size
    replace_leading(string, match, replacement, prefix_size, suffix_size, 0)
  end

  defp replace_leading(string, match, replacement, prefix_size, suffix_size, acc)
       when suffix_size >= 0 do
    case string do
      <<prefix::size(^prefix_size)-binary, suffix::binary>> when prefix == match ->
        replace_leading(
          suffix,
          match,
          replacement,
          prefix_size,
          suffix_size - prefix_size,
          acc + 1
        )

      _ ->
        prepend_unless_empty(duplicate(replacement, acc), string)
    end
  end

  defp replace_leading(string, _match, replacement, _prefix_size, _suffix_size, acc) do
    prepend_unless_empty(duplicate(replacement, acc), string)
  end

  @doc """
  Replaces all trailing occurrences of `match` by `replacement` in `string`.

  Returns the string untouched if there are no occurrences.

  If `match` is `""`, this function raises an `ArgumentError` exception: this
  happens because this function replaces **all** the occurrences of `match` at
  the end of `string`, and it's impossible to replace "multiple" occurrences of
  `""`.

  ## Examples

      iex> String.replace_trailing("hello world", " world", "")
      "hello"
      iex> String.replace_trailing("hello world world", " world", "")
      "hello"

      iex> String.replace_trailing("hello world", " world", " mundo")
      "hello mundo"
      iex> String.replace_trailing("hello world world", " world", " mundo")
      "hello mundo mundo"

  This function can replace across grapheme boundaries. See `replace/3`
  for more information and examples.
  """
  @spec replace_trailing(t, t, t) :: t
  def replace_trailing(string, match, replacement)
      when is_binary(string) and is_binary(match) and is_binary(replacement) do
    if match == "" do
      raise ArgumentError, "cannot use an empty string as the match to replace"
    end

    suffix_size = byte_size(match)
    prefix_size = byte_size(string) - suffix_size
    replace_trailing(string, match, replacement, prefix_size, suffix_size, 0)
  end

  defp replace_trailing(string, match, replacement, prefix_size, suffix_size, acc)
       when prefix_size >= 0 do
    case string do
      <<prefix::size(^prefix_size)-binary, suffix::binary>> when suffix == match ->
        replace_trailing(
          prefix,
          match,
          replacement,
          prefix_size - suffix_size,
          suffix_size,
          acc + 1
        )

      _ ->
        append_unless_empty(string, duplicate(replacement, acc))
    end
  end

  defp replace_trailing(string, _match, replacement, _prefix_size, _suffix_size, acc) do
    append_unless_empty(string, duplicate(replacement, acc))
  end

  @doc """
  Replaces prefix in `string` by `replacement` if it matches `match`.

  Returns the string untouched if there is no match. If `match` is an empty
  string (`""`), `replacement` is just prepended to `string`.

  ## Examples

      iex> String.replace_prefix("world", "hello ", "")
      "world"
      iex> String.replace_prefix("hello world", "hello ", "")
      "world"
      iex> String.replace_prefix("hello hello world", "hello ", "")
      "hello world"

      iex> String.replace_prefix("world", "hello ", "ola ")
      "world"
      iex> String.replace_prefix("hello world", "hello ", "ola ")
      "ola world"
      iex> String.replace_prefix("hello hello world", "hello ", "ola ")
      "ola hello world"

      iex> String.replace_prefix("world", "", "hello ")
      "hello world"

  This function can replace across grapheme boundaries. See `replace/3`
  for more information and examples.
  """
  @spec replace_prefix(t, t, t) :: t
  def replace_prefix(string, match, replacement)
      when is_binary(string) and is_binary(match) and is_binary(replacement) do
    prefix_size = byte_size(match)

    case string do
      <<prefix::size(^prefix_size)-binary, suffix::binary>> when prefix == match ->
        prepend_unless_empty(replacement, suffix)

      _ ->
        string
    end
  end

  @doc """
  Replaces suffix in `string` by `replacement` if it matches `match`.

  Returns the string untouched if there is no match. If `match` is an empty
  string (`""`), `replacement` is just appended to `string`.

  ## Examples

      iex> String.replace_suffix("hello", " world", "")
      "hello"
      iex> String.replace_suffix("hello world", " world", "")
      "hello"
      iex> String.replace_suffix("hello world world", " world", "")
      "hello world"

      iex> String.replace_suffix("hello", " world", " mundo")
      "hello"
      iex> String.replace_suffix("hello world", " world", " mundo")
      "hello mundo"
      iex> String.replace_suffix("hello world world", " world", " mundo")
      "hello world mundo"

      iex> String.replace_suffix("hello", "", " world")
      "hello world"

  This function can replace across grapheme boundaries. See `replace/3`
  for more information and examples.
  """
  @spec replace_suffix(t, t, t) :: t
  def replace_suffix(string, match, replacement)
      when is_binary(string) and is_binary(match) and is_binary(replacement) do
    suffix_size = byte_size(match)
    prefix_size = byte_size(string) - suffix_size

    case string do
      <<prefix::size(^prefix_size)-binary, suffix::binary>> when suffix == match ->
        append_unless_empty(prefix, replacement)

      _ ->
        string
    end
  end

  @compile {:inline, prepend_unless_empty: 2, append_unless_empty: 2}

  defp prepend_unless_empty("", suffix), do: suffix
  defp prepend_unless_empty(prefix, suffix), do: prefix <> suffix

  defp append_unless_empty(prefix, ""), do: prefix
  defp append_unless_empty(prefix, suffix), do: prefix <> suffix

  @doc false
  @deprecated "Use String.trim_leading/1 instead"
  defdelegate lstrip(binary), to: String.Break, as: :trim_leading

  @doc false
  @deprecated "Use String.trim_leading/2 with a binary as second argument instead"
  def lstrip(string, char) when is_integer(char) do
    replace_leading(string, <<char::utf8>>, "")
  end

  @doc false
  @deprecated "Use String.trim/1 instead"
  def strip(string) do
    trim(string)
  end

  @doc false
  @deprecated "Use String.trim/2 with a binary second argument instead"
  def strip(string, char) do
    trim(string, <<char::utf8>>)
  end

  @doc ~S"""
  Returns a string where all leading Unicode whitespaces
  have been removed.

  ## Examples

      iex> String.trim_leading("\n  abc   ")
      "abc   "

  """
  @spec trim_leading(t) :: t
  defdelegate trim_leading(string), to: String.Break

  @doc """
  Returns a string where all leading `to_trim` characters have been removed.

  ## Examples

      iex> String.trim_leading("__ abc _", "_")
      " abc _"

      iex> String.trim_leading("1 abc", "11")
      "1 abc"

  """
  @spec trim_leading(t, t) :: t
  def trim_leading(string, to_trim)
      when is_binary(string) and is_binary(to_trim) do
    replace_leading(string, to_trim, "")
  end

  @doc ~S"""
  Returns a string where all trailing Unicode whitespaces
  has been removed.

  ## Examples

      iex> String.trim_trailing("   abc\n  ")
      "   abc"

  """
  @spec trim_trailing(t) :: t
  defdelegate trim_trailing(string), to: String.Break

  @doc """
  Returns a string where all trailing `to_trim` characters have been removed.

  ## Examples

      iex> String.trim_trailing("_ abc __", "_")
      "_ abc "

      iex> String.trim_trailing("abc 1", "11")
      "abc 1"

  """
  @spec trim_trailing(t, t) :: t
  def trim_trailing(string, to_trim)
      when is_binary(string) and is_binary(to_trim) do
    replace_trailing(string, to_trim, "")
  end

  @doc ~S"""
  Returns a string where all leading and trailing Unicode whitespaces
  have been removed.

  ## Examples

      iex> String.trim("\n  abc\n  ")
      "abc"

  """
  @spec trim(t) :: t
  def trim(string) when is_binary(string) do
    string
    |> trim_leading()
    |> trim_trailing()
  end

  @doc """
  Returns a string where all leading and trailing `to_trim` characters have been
  removed.

  ## Examples

      iex> String.trim("a  abc  a", "a")
      "  abc  "

  """
  @spec trim(t, t) :: t
  def trim(string, to_trim) when is_binary(string) and is_binary(to_trim) do
    string
    |> trim_leading(to_trim)
    |> trim_trailing(to_trim)
  end

  @doc ~S"""
  Returns a new string padded with a leading filler
  which is made of elements from the `padding`.

  Passing a list of strings as `padding` will take one element of the list
  for every missing entry. If the list is shorter than the number of inserts,
  the filling will start again from the beginning of the list.
  Passing a string `padding` is equivalent to passing the list of graphemes in it.
  If no `padding` is given, it defaults to whitespace.

  When `count` is less than or equal to the length of `string`,
  given `string` is returned.

  Raises `ArgumentError` if the given `padding` contains a non-string element.

  ## Examples

      iex> String.pad_leading("abc", 5)
      "  abc"

      iex> String.pad_leading("abc", 4, "12")
      "1abc"

      iex> String.pad_leading("abc", 6, "12")
      "121abc"

      iex> String.pad_leading("abc", 5, ["1", "23"])
      "123abc"

  """
  @spec pad_leading(t, non_neg_integer, t | [t]) :: t
  def pad_leading(string, count, padding \\ [" "])

  def pad_leading(string, count, padding) when is_binary(padding) do
    pad_leading(string, count, graphemes(padding))
  end

  def pad_leading(string, count, [_ | _] = padding)
      when is_binary(string) and is_integer(count) and count >= 0 do
    pad(:leading, string, count, padding)
  end

  @doc ~S"""
  Returns a new string padded with a trailing filler
  which is made of elements from the `padding`.

  Passing a list of strings as `padding` will take one element of the list
  for every missing entry. If the list is shorter than the number of inserts,
  the filling will start again from the beginning of the list.
  Passing a string `padding` is equivalent to passing the list of graphemes in it.
  If no `padding` is given, it defaults to whitespace.

  When `count` is less than or equal to the length of `string`,
  given `string` is returned.

  Raises `ArgumentError` if the given `padding` contains a non-string element.

  ## Examples

      iex> String.pad_trailing("abc", 5)
      "abc  "

      iex> String.pad_trailing("abc", 4, "12")
      "abc1"

      iex> String.pad_trailing("abc", 6, "12")
      "abc121"

      iex> String.pad_trailing("abc", 5, ["1", "23"])
      "abc123"

  """
  @spec pad_trailing(t, non_neg_integer, t | [t]) :: t
  def pad_trailing(string, count, padding \\ [" "])

  def pad_trailing(string, count, padding) when is_binary(padding) do
    pad_trailing(string, count, graphemes(padding))
  end

  def pad_trailing(string, count, [_ | _] = padding)
      when is_binary(string) and is_integer(count) and count >= 0 do
    pad(:trailing, string, count, padding)
  end

  defp pad(kind, string, count, padding) do
    string_length = length(string)

    if string_length >= count do
      string
    else
      filler = build_filler(count - string_length, padding, padding, 0, [])

      case kind do
        :leading -> [filler | string]
        :trailing -> [string | filler]
      end
      |> IO.iodata_to_binary()
    end
  end

  defp build_filler(0, _source, _padding, _size, filler), do: filler

  defp build_filler(count, source, [], size, filler) do
    rem_filler =
      rem(count, size)
      |> build_filler(source, source, 0, [])

    filler =
      filler
      |> IO.iodata_to_binary()
      |> duplicate(div(count, size) + 1)

    [filler | rem_filler]
  end

  defp build_filler(count, source, [elem | rest], size, filler)
       when is_binary(elem) do
    build_filler(count - 1, source, rest, size + 1, [filler | elem])
  end

  defp build_filler(_count, _source, [elem | _rest], _size, _filler) do
    raise ArgumentError, "expected a string padding element, got: #{inspect(elem)}"
  end

  @doc false
  @deprecated "Use String.pad_leading/2 instead"
  def rjust(subject, length) do
    rjust(subject, length, ?\s)
  end

  @doc false
  @deprecated "Use String.pad_leading/3 with a binary padding instead"
  def rjust(subject, length, pad) when is_integer(pad) and is_integer(length) and length >= 0 do
    pad(:leading, subject, length, [<<pad::utf8>>])
  end

  @doc false
  @deprecated "Use String.pad_trailing/2 instead"
  def ljust(subject, length) do
    ljust(subject, length, ?\s)
  end

  @doc false
  @deprecated "Use String.pad_trailing/3 with a binary padding instead"
  def ljust(subject, length, pad) when is_integer(pad) and is_integer(length) and length >= 0 do
    pad(:trailing, subject, length, [<<pad::utf8>>])
  end

  @doc ~S"""
  Returns a new string created by replacing occurrences of `pattern` in
  `subject` with `replacement`.

  The `subject` is always a string.

  The `pattern` may be a string, a list of strings, a regular expression, or a
  compiled pattern.

  The `replacement` may be a string or a function that receives the matched
  pattern and must return the replacement as a string or iodata.

  By default it replaces all occurrences but this behavior can be controlled
  through the `:global` option; see the "Options" section below.

  ## Options

    * `:global` - (boolean) if `true`, all occurrences of `pattern` are replaced
      with `replacement`, otherwise only the first occurrence is
      replaced. Defaults to `true`

  ## Examples

      iex> String.replace("a,b,c", ",", "-")
      "a-b-c"

      iex> String.replace("a,b,c", ",", "-", global: false)
      "a-b,c"

  The pattern may also be a list of strings and the replacement may also
  be a function that receives the matches:

      iex> String.replace("a,b,c", ["a", "c"], fn <<char>> -> <<char + 1>> end)
      "b,b,d"

  When the pattern is a regular expression, one can give `\N` or
  `\g{N}` in the `replacement` string to access a specific capture in the
  regular expression:

      iex> String.replace("a,b,c", ~r/,(.)/, ",\\1\\g{1}")
      "a,bb,cc"

  Note that we had to escape the backslash escape character (i.e., we used `\\N`
  instead of just `\N` to escape the backslash; same thing for `\\g{N}`). By
  giving `\0`, one can inject the whole match in the replacement string.

  A compiled pattern can also be given:

      iex> pattern = :binary.compile_pattern(",")
      iex> String.replace("a,b,c", pattern, "[]")
      "a[]b[]c"

  When an empty string is provided as a `pattern`, the function will treat it as
  an implicit empty string between each grapheme and the string will be
  interspersed. If an empty string is provided as `replacement` the `subject`
  will be returned:

      iex> String.replace("ELIXIR", "", ".")
      ".E.L.I.X.I.R."

      iex> String.replace("ELIXIR", "", "")
      "ELIXIR"

  Be aware that this function can replace within or across grapheme boundaries.
  For example, take the grapheme "é" which is made of the characters
  "e" and the acute accent. The following will replace only the letter "e",
  moving the accent to the letter "o":

      iex> String.replace(String.normalize("é", :nfd), "e", "o")
      "ó"

  However, if "é" is represented by the single character "e with acute"
  accent, then it won't be replaced at all:

      iex> String.replace(String.normalize("é", :nfc), "e", "o")
      "é"

  """
  @spec replace(t, pattern | Regex.t(), t | (t -> t | iodata), keyword) :: t
  def replace(subject, pattern, replacement, options \\ [])
      when is_binary(subject) and
             (is_binary(replacement) or is_function(replacement, 1)) and
             is_list(options) do
    replace_guarded(subject, pattern, replacement, options)
  end

  defp replace_guarded(subject, %{__struct__: Regex} = regex, replacement, options) do
    Regex.replace(regex, subject, replacement, options)
  end

  defp replace_guarded(subject, "", "", _) do
    subject
  end

  defp replace_guarded(subject, [], _, _) do
    subject
  end

  defp replace_guarded(subject, "", replacement_binary, options)
       when is_binary(replacement_binary) do
    if Keyword.get(options, :global, true) do
      intersperse_bin(subject, replacement_binary, [replacement_binary])
    else
      replacement_binary <> subject
    end
  end

  defp replace_guarded(subject, "", replacement_fun, options) do
    if Keyword.get(options, :global, true) do
      intersperse_fun(subject, replacement_fun, [replacement_fun.("")])
    else
      IO.iodata_to_binary([replacement_fun.("") | subject])
    end
  end

  defp replace_guarded(subject, pattern, replacement, options) do
    if insert = Keyword.get(options, :insert_replaced) do
      IO.warn(
        "String.replace/4 with :insert_replaced option is deprecated. " <>
          "Please use :binary.replace/4 instead or pass an anonymous function as replacement"
      )

      binary_options = if Keyword.get(options, :global) != false, do: [:global], else: []
      :binary.replace(subject, pattern, replacement, [insert_replaced: insert] ++ binary_options)
    else
      matches =
        if Keyword.get(options, :global, true) do
          :binary.matches(subject, pattern)
        else
          case :binary.match(subject, pattern) do
            :nomatch -> []
            match -> [match]
          end
        end

      IO.iodata_to_binary(do_replace(subject, matches, replacement, 0))
    end
  end

  defp intersperse_bin(subject, replacement, acc) do
    case :unicode_util.gc(subject) do
      [current | rest] ->
        intersperse_bin(rest, replacement, [replacement, current | acc])

      [] ->
        reverse_characters_to_binary(acc)

      {:error, <<byte, rest::bits>>} ->
        reverse_characters_to_binary(acc) <>
          <<byte>> <> intersperse_bin(rest, replacement, [replacement])
    end
  end

  defp intersperse_fun(subject, replacement, acc) do
    case :unicode_util.gc(subject) do
      [current | rest] ->
        intersperse_fun(rest, replacement, [replacement.(""), current | acc])

      [] ->
        reverse_characters_to_binary(acc)

      {:error, <<byte, rest::bits>>} ->
        reverse_characters_to_binary(acc) <>
          <<byte>> <> intersperse_fun(rest, replacement, [replacement.("")])
    end
  end

  defp do_replace(subject, [], _, n) do
    [binary_part(subject, n, byte_size(subject) - n)]
  end

  defp do_replace(subject, [{start, length} | matches], replacement, n) do
    prefix = binary_part(subject, n, start - n)

    middle =
      if is_binary(replacement) do
        replacement
      else
        replacement.(binary_part(subject, start, length))
      end

    [prefix, middle | do_replace(subject, matches, replacement, start + length)]
  end

  @doc ~S"""
  Reverses the graphemes in given string.

  ## Examples

      iex> String.reverse("abcd")
      "dcba"

      iex> String.reverse("hello world")
      "dlrow olleh"

      iex> String.reverse("hello ∂og")
      "go∂ olleh"

  Keep in mind reversing the same string twice does
  not necessarily yield the original string:

      iex> "̀e"
      "̀e"
      iex> String.reverse("̀e")
      "è"
      iex> String.reverse(String.reverse("̀e"))
      "è"

  In the first example the accent is before the vowel, so
  it is considered two graphemes. However, when you reverse
  it once, you have the vowel followed by the accent, which
  becomes one grapheme. Reversing it again will keep it as
  one single grapheme.
  """
  @spec reverse(t) :: t
  def reverse(string) when is_binary(string) do
    do_reverse(:unicode_util.gc(string), [])
  end

  defp do_reverse([grapheme | rest], acc),
    do: do_reverse(:unicode_util.gc(rest), [grapheme | acc])

  defp do_reverse([], acc),
    do: :unicode.characters_to_binary(acc)

  defp do_reverse({:error, <<byte, rest::bits>>}, acc),
    do: :unicode.characters_to_binary(acc) <> <<byte>> <> do_reverse(:unicode_util.gc(rest), [])

  @doc """
  Returns a string `subject` repeated `n` times.

  Inlined by the compiler.

  ## Examples

      iex> String.duplicate("abc", 0)
      ""

      iex> String.duplicate("abc", 1)
      "abc"

      iex> String.duplicate("abc", 2)
      "abcabc"

  """
  @compile {:inline, duplicate: 2}
  @spec duplicate(t, non_neg_integer) :: t
  def duplicate(subject, n) when is_binary(subject) and is_integer(n) and n >= 0 do
    :binary.copy(subject, n)
  end

  @doc ~S"""
  Returns a list of code points encoded as strings.

  To retrieve code points in their natural integer
  representation, see `to_charlist/1`. For details about
  code points and graphemes, see the `String` module
  documentation.

  ## Examples

      iex> String.codepoints("olá")
      ["o", "l", "á"]

      iex> String.codepoints("оптими зации")
      ["о", "п", "т", "и", "м", "и", " ", "з", "а", "ц", "и", "и"]

      iex> String.codepoints("ἅἪῼ")
      ["ἅ", "Ἢ", "ῼ"]

      iex> String.codepoints("\u00e9")
      ["é"]

      iex> String.codepoints("\u0065\u0301")
      ["e", "́"]

  """
  @spec codepoints(t) :: [codepoint]
  def codepoints(string) when is_binary(string) do
    do_codepoints(string)
  end

  defp do_codepoints(<<codepoint::utf8, rest::bits>>) do
    [<<codepoint::utf8>> | do_codepoints(rest)]
  end

  defp do_codepoints(<<byte, rest::bits>>) do
    [<<byte>> | do_codepoints(rest)]
  end

  defp do_codepoints(<<>>), do: []

  @doc ~S"""
  Returns the next code point in a string.

  The result is a tuple with the code point and the
  remainder of the string or `nil` in case
  the string reached its end.

  As with other functions in the `String` module, `next_codepoint/1`
  works with binaries that are invalid UTF-8. If the string starts
  with a sequence of bytes that is not valid in UTF-8 encoding, the
  first element of the returned tuple is a binary with the first byte.

  ## Examples

      iex> String.next_codepoint("olá")
      {"o", "lá"}

      iex> invalid = "\x80\x80OK" # first two bytes are invalid in UTF-8
      iex> {_, rest} = String.next_codepoint(invalid)
      {<<128>>, <<128, 79, 75>>}
      iex> String.next_codepoint(rest)
      {<<128>>, "OK"}

  ## Comparison with binary pattern matching

  Binary pattern matching provides a similar way to decompose
  a string:

      iex> <<codepoint::utf8, rest::binary>> = "Elixir"
      "Elixir"
      iex> codepoint
      69
      iex> rest
      "lixir"

  though not entirely equivalent because `codepoint` comes as
  an integer, and the pattern won't match invalid UTF-8.

  Binary pattern matching, however, is simpler and more efficient,
  so pick the option that better suits your use case.
  """
  @spec next_codepoint(t) :: {codepoint, t} | nil
  def next_codepoint(<<cp::utf8, rest::binary>>), do: {<<cp::utf8>>, rest}
  def next_codepoint(<<byte, rest::binary>>), do: {<<byte>>, rest}
  def next_codepoint(<<>>), do: nil

  @doc ~S"""
  Checks whether `string` contains only valid characters.

  `algorithm` may be `:default` or `:fast_ascii`. Both algorithms are equivalent
  from a validation perspective (they will always produce the same output), but
  `:fast_ascii` can yield significant performance benefits in specific scenarios.

  If all of the following conditions are true, you may want to experiment with
  the `:fast_ascii` algorithm to see if it yields performance benefits in your
  specific scenario:

  * You are running Erlang/OTP 26 or newer on a 64 bit platform
  * You expect most of your strings to be longer than ~64 bytes
  * You expect most of your strings to contain mostly ASCII codepoints

  Note that the `:fast_ascii` algorithm does not affect correctness, you can
  expect the output of `String.valid?/2` to be the same regardless of algorithm.
  The only difference to be expected is one of performance, which can be
  expected to improve roughly linearly in string length compared to the
  `:default` algorithm.

  ## Examples

      iex> String.valid?("a")
      true

      iex> String.valid?("ø")
      true

      iex> String.valid?(<<0xFFFF::16>>)
      false

      iex> String.valid?(<<0xEF, 0xB7, 0x90>>)
      true

      iex> String.valid?("asd" <> <<0xFFFF::16>>)
      false

      iex> String.valid?("a", :fast_ascii)
      true

      iex> String.valid?(4)
      ** (FunctionClauseError) no function clause matching in String.valid?/2

  """
  @spec valid?(t, :default | :fast_ascii) :: boolean
  def valid?(string, algorithm \\ :default)

  def valid?(<<string::binary>>, :default), do: valid_utf8?(string)
  def valid?(<<string::binary>>, :fast_ascii), do: valid_utf8_fast_ascii?(string)

  defp valid_utf8?(<<_::utf8, rest::bits>>), do: valid_utf8?(rest)
  defp valid_utf8?(<<>>), do: true
  defp valid_utf8?(_), do: false

  defp valid_utf8_fast_ascii?(<<a::56, rest::bits>>)
       when Bitwise.band(0x80808080808080, a) == 0 do
    valid_utf8_fast_ascii?(rest)
  end

  defp valid_utf8_fast_ascii?(<<_::utf8, rest::bits>>), do: valid_utf8_fast_ascii?(rest)
  defp valid_utf8_fast_ascii?(<<>>), do: true
  defp valid_utf8_fast_ascii?(_), do: false

  @doc false
  @deprecated "Use String.valid?/1 instead"
  def valid_character?(string) do
    case string do
      <<_::utf8>> -> valid?(string)
      _ -> false
    end
  end

  defguardp replace_invalid_ii_of_iii(i, ii)
            when Bitwise.bor(Bitwise.bsl(i, 6), ii) in 32..863 or
                   Bitwise.bor(Bitwise.bsl(i, 6), ii) in 896..1023

  defguardp replace_invalid_ii_of_iv(i, ii)
            when Bitwise.bor(Bitwise.bsl(i, 6), ii) in 16..271

  defguardp replace_invalid_iii_of_iv(i, ii, iii)
            when Bitwise.bor(Bitwise.bor(Bitwise.bsl(i, 12), Bitwise.bsl(ii, 6)), iii) in 1024..17407

  defguardp replace_invalid_is_next(next) when Bitwise.bsr(next, 6) !== 0b10

  @doc ~S"""
  Returns a new string created by replacing all invalid bytes with `replacement` (`"�"` by default).

  ## Examples

      iex> String.replace_invalid("asd" <> <<0xFF::8>>)
      "asd�"

      iex> String.replace_invalid("nem rán bề bề")
      "nem rán bề bề"

      iex> String.replace_invalid("nem rán b" <> <<225, 187>> <> " bề")
      "nem rán b� bề"

      iex> String.replace_invalid("nem rán b" <> <<225, 187>> <> " bề", "ERROR!")
      "nem rán bERROR! bề"
  """
  @doc since: "1.16.0"
  @spec replace_invalid(binary, t) :: t
  def replace_invalid(bytes, replacement \\ "�")
      when is_binary(bytes) and is_binary(replacement) do
    do_replace_invalid(bytes, replacement, <<>>)
  end

  # Valid ASCII (for better average speed)
  defp do_replace_invalid(<<ascii::8, next::8, _::binary>> = rest, rep, acc)
       when ascii in 0..127 and replace_invalid_is_next(next) do
    <<_::8, rest::binary>> = rest
    do_replace_invalid(rest, rep, acc <> <<ascii::8>>)
  end

  # Valid UTF-8
  defp do_replace_invalid(<<grapheme::utf8, rest::binary>>, rep, acc) do
    do_replace_invalid(rest, rep, acc <> <<grapheme::utf8>>)
  end

  # 2/3 truncated sequence
  defp do_replace_invalid(<<0b1110::4, i::4, 0b10::2, ii::6>>, rep, acc)
       when replace_invalid_ii_of_iii(i, ii) do
    acc <> rep
  end

  defp do_replace_invalid(
         <<0b1110::4, i::4, 0b10::2, ii::6, next::8, _::binary>> = rest,
         rep,
         acc
       )
       when replace_invalid_ii_of_iii(i, ii) and replace_invalid_is_next(next) do
    <<_::16, rest::binary>> = rest
    do_replace_invalid(rest, rep, acc <> rep)
  end

  # 2/4
  defp do_replace_invalid(<<0b11110::5, i::3, 0b10::2, ii::6>>, rep, acc)
       when replace_invalid_ii_of_iv(i, ii) do
    acc <> rep
  end

  defp do_replace_invalid(
         <<0b11110::5, i::3, 0b10::2, ii::6, next::8, _::binary>> = rest,
         rep,
         acc
       )
       when replace_invalid_ii_of_iv(i, ii) and replace_invalid_is_next(next) do
    <<_::16, rest::binary>> = rest
    do_replace_invalid(rest, rep, acc <> rep)
  end

  # 3/4
  defp do_replace_invalid(<<0b11110::5, i::3, 0b10::2, ii::6, 0b10::2, iii::6>>, rep, acc)
       when replace_invalid_iii_of_iv(i, ii, iii) do
    acc <> rep
  end

  defp do_replace_invalid(
         <<0b11110::5, i::3, 0b10::2, ii::6, 0b10::2, iii::6, next::8, _::binary>> = rest,
         rep,
         acc
       )
       when replace_invalid_iii_of_iv(i, ii, iii) and replace_invalid_is_next(next) do
    <<_::24, rest::binary>> = rest
    do_replace_invalid(rest, rep, acc <> rep)
  end

  # Everything else
  defp do_replace_invalid(<<_, rest::binary>>, rep, acc),
    do: do_replace_invalid(rest, rep, acc <> rep)

  # Final
  defp do_replace_invalid(<<>>, _, acc), do: acc

  @doc ~S"""
  Splits the string into chunks of characters that share a common trait.

  The trait can be one of two options:

    * `:valid` - the string is split into chunks of valid and invalid
      character sequences

    * `:printable` - the string is split into chunks of printable and
      non-printable character sequences

  Returns a list of binaries each of which contains only one kind of
  characters.

  If the given string is empty, an empty list is returned.

  ## Examples

      iex> String.chunk(<<?a, ?b, ?c, 0>>, :valid)
      ["abc\0"]

      iex> String.chunk(<<?a, ?b, ?c, 0, 0xFFFF::utf16>>, :valid)
      ["abc\0", <<0xFFFF::utf16>>]

      iex> String.chunk(<<?a, ?b, ?c, 0, 0x0FFFF::utf8>>, :printable)
      ["abc", <<0, 0x0FFFF::utf8>>]

  """
  @spec chunk(t, :valid | :printable) :: [t]

  def chunk(string, trait)

  def chunk("", _), do: []

  def chunk(string, trait) when is_binary(string) and trait in [:valid, :printable] do
    {cp, _} = next_codepoint(string)
    pred_fn = make_chunk_pred(trait)
    do_chunk(string, pred_fn.(cp), pred_fn)
  end

  defp do_chunk(string, flag, pred_fn), do: do_chunk(string, [], <<>>, flag, pred_fn)

  defp do_chunk(<<>>, acc, <<>>, _, _), do: Enum.reverse(acc)

  defp do_chunk(<<>>, acc, chunk, _, _), do: Enum.reverse(acc, [chunk])

  defp do_chunk(string, acc, chunk, flag, pred_fn) do
    {cp, rest} = next_codepoint(string)

    if pred_fn.(cp) != flag do
      do_chunk(rest, [chunk | acc], cp, not flag, pred_fn)
    else
      do_chunk(rest, acc, chunk <> cp, flag, pred_fn)
    end
  end

  defp make_chunk_pred(:valid), do: &valid?/1
  defp make_chunk_pred(:printable), do: &printable?/1

  @doc ~S"""
  Returns Unicode graphemes in the string as per Extended Grapheme
  Cluster algorithm.

  The algorithm is outlined in the [Unicode Standard Annex #29,
  Unicode Text Segmentation](https://www.unicode.org/reports/tr29/).

  For details about code points and graphemes, see the `String` module documentation.

  ## Examples

      iex> String.graphemes("Ńaïve")
      ["Ń", "a", "ï", "v", "e"]

      iex> String.graphemes("\u00e9")
      ["é"]

      iex> String.graphemes("\u0065\u0301")
      ["é"]

  """
  @compile {:inline, graphemes: 1}
  @spec graphemes(t) :: [grapheme]
  def graphemes(string) when is_binary(string), do: do_graphemes(string)

  defp do_graphemes(gcs) do
    case :unicode_util.gc(gcs) do
      [gc | rest] -> [grapheme_to_binary(gc) | do_graphemes(rest)]
      [] -> []
      {:error, <<byte, rest::bits>>} -> [<<byte>> | do_graphemes(rest)]
    end
  end

  @doc """
  Returns the next grapheme in a string.

  The result is a tuple with the grapheme and the
  remainder of the string or `nil` in case
  the String reached its end.

  ## Examples

      iex> String.next_grapheme("olá")
      {"o", "lá"}

      iex> String.next_grapheme("")
      nil

  """
  @compile {:inline, next_grapheme: 1}
  @spec next_grapheme(t) :: {grapheme, t} | nil
  def next_grapheme(string) when is_binary(string) do
    case :unicode_util.gc(string) do
      [gc] -> {grapheme_to_binary(gc), <<>>}
      [gc, rest] -> {grapheme_to_binary(gc), rest}
      [gc | rest] -> {grapheme_to_binary(gc), rest}
      [] -> nil
      {:error, <<byte, rest::bits>>} -> {<<byte>>, rest}
    end
  end

  @doc """
  Returns the size (in bytes) of the next grapheme.

  The result is a tuple with the next grapheme size in bytes and
  the remainder of the string or `nil` in case the string
  reached its end.

  ## Examples

      iex> String.next_grapheme_size("olá")
      {1, "lá"}

      iex> String.next_grapheme_size("")
      nil

  """
  @spec next_grapheme_size(t) :: {pos_integer, t} | nil
  def next_grapheme_size(string) when is_binary(string) do
    case :unicode_util.gc(string) do
      [gc] -> {grapheme_byte_size(gc), <<>>}
      [gc, rest] -> {grapheme_byte_size(gc), rest}
      [gc | rest] -> {grapheme_byte_size(gc), rest}
      [] -> nil
      {:error, <<_, rest::bits>>} -> {1, rest}
    end
  end

  @doc """
  Returns the first grapheme from a UTF-8 string,
  `nil` if the string is empty.

  ## Examples

      iex> String.first("elixir")
      "e"

      iex> String.first("եոգլի")
      "ե"

      iex> String.first("")
      nil

  """
  @spec first(t) :: grapheme | nil
  def first(string) when is_binary(string) do
    case :unicode_util.gc(string) do
      [gc | _] -> grapheme_to_binary(gc)
      [] -> nil
      {:error, <<byte, _::bits>>} -> <<byte>>
    end
  end

  @doc """
  Returns the last grapheme from a UTF-8 string,
  `nil` if the string is empty.

  It traverses the whole string to find its last grapheme.

  ## Examples

      iex> String.last("")
      nil

      iex> String.last("elixir")
      "r"

      iex> String.last("եոգլի")
      "ի"

  """
  @spec last(t) :: grapheme | nil
  def last(""), do: nil
  def last(string) when is_binary(string), do: do_last(:unicode_util.gc(string), nil)

  defp do_last([gc | rest], _), do: do_last(:unicode_util.gc(rest), gc)
  defp do_last([], acc) when is_binary(acc), do: acc
  defp do_last([], acc), do: :unicode.characters_to_binary([acc])
  defp do_last({:error, <<byte, rest::bits>>}, _), do: do_last(:unicode_util.gc(rest), <<byte>>)

  @doc """
  Returns the number of Unicode graphemes in a UTF-8 string.

  ## Examples

      iex> String.length("elixir")
      6

      iex> String.length("եոգլի")
      5

  """
  @spec length(t) :: non_neg_integer
  def length(string) when is_binary(string), do: length(string, 0)

  defp length(<<byte1, byte2, rest::binary>> = binary, acc)
       when byte1 <= 127 and byte1 != ?\r and byte2 <= 127 and byte2 != ?\r do
    skip = skip_length(rest, 1)
    length(binary_part(binary, skip, byte_size(binary) - skip), acc + skip)
  end

  defp length(gcs, acc) do
    case :unicode_util.gc(gcs) do
      [_ | rest] -> length(rest, acc + 1)
      [] -> acc
      {:error, <<_, rest::bits>>} -> length(rest, acc + 1)
    end
  end

  defp skip_length(<<byte, rest::binary>>, acc)
       when byte <= 127 and byte != ?\r,
       do: skip_length(rest, acc + 1)

  defp skip_length(_binary, acc),
    do: acc

  @doc """
  Returns the grapheme at the `position` of the given UTF-8 `string`.
  If `position` is greater than `string` length, then it returns `nil`.

  > #### Linear Access {: .warning}
  >
  > This function has to linearly traverse the string.
  > If you want to access a string or a binary in constant time based on the
  > number of bytes, use `Kernel.binary_slice/3` or `:binary.at/2` instead.

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

  def at(string, position) when is_binary(string) and is_integer(position) and position >= 0 do
    do_at(string, position)
  end

  def at(string, position) when is_binary(string) and is_integer(position) and position < 0 do
    position = length(string) + position

    case position >= 0 do
      true -> do_at(string, position)
      false -> nil
    end
  end

  defp do_at(string, position) do
    left = byte_size_remaining_at(string, position)

    string
    |> binary_part(byte_size(string) - left, left)
    |> first()
  end

  @doc """
  Returns a substring starting at the offset `start`, and of the given `length`.

  This function works on Unicode graphemes. For example, slicing the first
  three characters of the string "héllo" will return "hél", which internally
  is represented by more than three bytes. Use `String.byte_slice/3` if you
  want to slice by a given number of bytes, while respecting the codepoint
  boundaries. If you want to work on raw bytes, check `Kernel.binary_part/3`
  or `Kernel.binary_slice/3` instead.

  If the offset is greater than string length, then it returns `""`.

  ## Examples

      iex> String.slice("elixir", 1, 3)
      "lix"

      iex> String.slice("elixir", 1, 10)
      "lixir"

      iex> String.slice("elixir", 10, 3)
      ""

  If the start position is negative, it is normalized
  against the string length and clamped to 0:

      iex> String.slice("elixir", -4, 4)
      "ixir"

      iex> String.slice("elixir", -10, 3)
      "eli"

  If start is more than the string length, an empty
  string is returned:

      iex> String.slice("elixir", 10, 1500)
      ""

  """
  @spec slice(t, integer, non_neg_integer) :: grapheme

  def slice(_, _, 0) do
    ""
  end

  def slice(string, start, length)
      when is_binary(string) and is_integer(start) and is_integer(length) and start >= 0 and
             length >= 0 do
    do_slice(string, start, length)
  end

  def slice(string, start, length)
      when is_binary(string) and is_integer(start) and is_integer(length) and start < 0 and
             length >= 0 do
    start = max(length(string) + start, 0)
    do_slice(string, start, length)
  end

  defp do_slice(string, start, length) do
    from_start = byte_size_remaining_at(string, start)
    rest = binary_part(string, byte_size(string) - from_start, from_start)

    from_end = byte_size_remaining_at(rest, length)
    binary_part(rest, 0, from_start - from_end)
  end

  @doc """
  Returns a substring from the offset given by the start of the
  range to the offset given by the end of the range.

  This function works on Unicode graphemes. For example, slicing the first
  three characters of the string "héllo" will return "hél", which internally
  is represented by more than three bytes. Use `String.byte_slice/3` if you
  want to slice by a given number of bytes, while respecting the codepoint
  boundaries. If you want to work on raw bytes, check `Kernel.binary_part/3`
  or `Kernel.binary_slice/3` instead.

  If the start of the range is not a valid offset for the given
  string or if the range is in reverse order, returns `""`.

  If the start or end of the range is negative, the whole string
  is traversed first in order to convert the negative indices into
  positive ones.

  ## Examples

      iex> String.slice("elixir", 1..3)
      "lix"
      iex> String.slice("elixir", 1..10)
      "lixir"

      iex> String.slice("elixir", -4..-1)
      "ixir"
      iex> String.slice("elixir", -4..6)
      "ixir"
      iex> String.slice("elixir", -100..100)
      "elixir"

  For ranges where `start > stop`, you need to explicitly
  mark them as increasing:

      iex> String.slice("elixir", 2..-1//1)
      "ixir"
      iex> String.slice("elixir", 1..-2//1)
      "lixi"

  You can use `../0` as a shortcut for `0..-1//1`, which returns
  the whole string as is:

      iex> String.slice("elixir", ..)
      "elixir"

  The step can be any positive number. For example, to
  get every 2 characters of the string:

      iex> String.slice("elixir", 0..-1//2)
      "eii"

  If the first position is after the string ends or after
  the last position of the range, it returns an empty string:

      iex> String.slice("elixir", 10..3//1)
      ""
      iex> String.slice("a", 1..1500)
      ""

  """
  @spec slice(t, Range.t()) :: t
  def slice(string, first..last//step = range) when is_binary(string) do
    # TODO: Support negative steps as a reverse on Elixir v2.0.
    cond do
      step > 0 ->
        slice_range(string, first, last, step)

      step == -1 and first > last ->
        IO.warn(
          "negative steps are not supported in String.slice/2, pass #{first}..#{last}//1 instead"
        )

        slice_range(string, first, last, 1)

      true ->
        raise ArgumentError,
              "String.slice/2 does not accept ranges with negative steps, got: #{inspect(range)}"
    end
  end

  # TODO: Remove me on v2.0
  def slice(string, %{__struct__: Range, first: first, last: last} = range)
      when is_binary(string) do
    step = if first <= last, do: 1, else: -1
    slice(string, Map.put(range, :step, step))
  end

  defp slice_range("", _, _, _), do: ""

  defp slice_range(_string, first, last, _step) when first >= 0 and last >= 0 and first > last do
    ""
  end

  defp slice_range(string, first, last, step) when first >= 0 do
    from_start = byte_size_remaining_at(string, first)
    rest = binary_part(string, byte_size(string) - from_start, from_start)

    cond do
      last == -1 ->
        slice_every(rest, byte_size(rest), step)

      last >= 0 and step == 1 ->
        from_end = byte_size_remaining_at(rest, last - first + 1)
        binary_part(rest, 0, from_start - from_end)

      last >= 0 ->
        slice_every(rest, last - first + 1, step)

      true ->
        rest
        |> slice_range_negative(0, last)
        |> slice_every(byte_size(string), step)
    end
  end

  defp slice_range(string, first, last, step) do
    string
    |> slice_range_negative(first, last)
    |> slice_every(byte_size(string), step)
  end

  defp slice_range_negative(string, first, last) do
    {reversed_bytes, length} = acc_bytes(string, [], 0)
    first = add_if_negative(first, length) |> max(0)
    last = add_if_negative(last, length)

    if first > last or first > length do
      ""
    else
      last = min(last + 1, length)
      reversed_bytes = Enum.drop(reversed_bytes, length - last)
      {length_bytes, start_bytes} = split_bytes(reversed_bytes, 0, last - first)
      binary_part(string, start_bytes, length_bytes)
    end
  end

  defp slice_every(string, _count, 1), do: string
  defp slice_every(string, count, step), do: slice_every(string, count, step, [])

  defp slice_every(string, count, to_drop, acc) when count > 0 do
    case :unicode_util.gc(string) do
      [current | rest] ->
        rest
        |> drop(to_drop)
        |> slice_every(count - to_drop, to_drop, [current | acc])

      [] ->
        reverse_characters_to_binary(acc)

      {:error, <<byte, rest::bits>>} ->
        reverse_characters_to_binary(acc) <>
          <<byte>> <> slice_every(drop(rest, to_drop), count - to_drop, to_drop, [])
    end
  end

  defp slice_every(_string, _count, _to_drop, acc) do
    reverse_characters_to_binary(acc)
  end

  defp drop(string, 1), do: string

  defp drop(string, count) do
    case :unicode_util.gc(string) do
      [_ | rest] -> drop(rest, count - 1)
      [] -> ""
      {:error, <<_, rest::bits>>} -> drop(rest, count - 1)
    end
  end

  defp acc_bytes(string, bytes, length) do
    case :unicode_util.gc(string) do
      [gc | rest] -> acc_bytes(rest, [grapheme_byte_size(gc) | bytes], length + 1)
      [] -> {bytes, length}
      {:error, <<_, rest::bits>>} -> acc_bytes(rest, [1 | bytes], length + 1)
    end
  end

  defp add_if_negative(value, to_add) when value < 0, do: value + to_add
  defp add_if_negative(value, _to_add), do: value

  defp split_bytes(rest, acc, 0), do: {acc, Enum.sum(rest)}
  defp split_bytes([], acc, _), do: {acc, 0}
  defp split_bytes([head | tail], acc, count), do: split_bytes(tail, head + acc, count - 1)

  @doc """
  Returns a substring starting at (or after) `start_bytes` and of at most
  the given `size_bytes`.

  This function works on bytes and then adjusts the string to eliminate
  truncated codepoints. This is useful when you have a string and you need
  to guarantee it does not exceed a certain amount of bytes.

  If the offset is greater than the number of bytes in the string, then it
  returns `""`. Similar to `String.slice/2`, a negative `start_bytes`
  will be adjusted to the end of the string (but in bytes).

  This function does not guarantee the string won't have invalid codepoints,
  it only guarantees to remove truncated codepoints immediately at the beginning
  or the end of the slice.

  ## Examples

  Consider the string "héllo". Let's see its representation:

      iex> inspect("héllo", binaries: :as_binaries)
      "<<104, 195, 169, 108, 108, 111>>"

  Although the string has 5 characters, it is made of 6 bytes. Now imagine
  we want to get only the first two bytes. To do so, let's use `binary_slice/3`,
  which is unaware of codepoints:

      iex> binary_slice("héllo", 0, 2)
      <<104, 195>>

  As you can see, this operation is unsafe and returns an invalid string.
  That's because we cut the string in the middle of the bytes representing
  "é". On the other hand, we could use `String.slice/3`:

      iex> String.slice("héllo", 0, 2)
      "hé"

  While the above is correct, it has 3 bytes. If you have a requirement where
  you need *at most* 2 bytes, the result would also be invalid. In such scenarios,
  you can use this function, which will slice the given bytes, but clean up
  the truncated codepoints:

      iex> String.byte_slice("héllo", 0, 2)
      "h"

  Truncated codepoints at the beginning are also cleaned up:

      iex> String.byte_slice("héllo", 2, 3)
      "llo"

  Note that, if you want to work on raw bytes, then you must use `binary_slice/3`
  instead.
  """
  @doc since: "1.17.0"
  @spec byte_slice(t, integer, non_neg_integer) :: t
  def byte_slice(string, start_bytes, size_bytes)
      when is_binary(string) and is_integer(start_bytes) and is_integer(size_bytes) and
             size_bytes >= 0 do
    total = byte_size(string)
    start_bytes = if start_bytes < 0, do: max(total + start_bytes, 0), else: start_bytes

    if start_bytes < total do
      :erlang.binary_part(string, start_bytes, total - start_bytes)
      |> invalid_prefix()
      |> invalid_suffix(size_bytes)
    else
      ""
    end
  end

  defp invalid_prefix(<<0b10::2, _::6, rest::binary>>), do: invalid_prefix(rest)
  defp invalid_prefix(rest), do: rest

  defp invalid_suffix(string, size) do
    last = invalid_suffix(string, min(size, byte_size(string)) - 1, 0)
    :erlang.binary_part(string, 0, last)
  end

  defp invalid_suffix(string, last, truncated) when last >= 0 do
    byte = :binary.at(string, last)

    cond do
      # ASCII byte, discard all truncated entries
      byte <= 127 ->
        last + 1

      # In the middle of a codepoint
      byte <= 191 ->
        invalid_suffix(string, last - 1, truncated + 1)

      # 2 bytes codepoint start
      byte <= 223 ->
        if truncated == 1, do: last + truncated + 1, else: last

      # 3 bytes codepoint start
      byte <= 239 ->
        if truncated == 2, do: last + truncated + 1, else: last

      # 4 bytes codepoint start
      byte <= 247 ->
        if truncated == 3, do: last + truncated + 1, else: last

      # Invalid codepoint, discard it, stop checking
      true ->
        last + 1
    end
  end

  defp invalid_suffix(_string, _last, _truncated), do: 0

  @doc """
  Returns `true` if `string` starts with any of the prefixes given.

  `prefix` can be either a string, a list of strings, or a compiled
  pattern.

  ## Examples

      iex> String.starts_with?("elixir", "eli")
      true
      iex> String.starts_with?("elixir", ["erlang", "elixir"])
      true
      iex> String.starts_with?("elixir", ["erlang", "ruby"])
      false

  An empty string will always match:

      iex> String.starts_with?("elixir", "")
      true
      iex> String.starts_with?("elixir", ["", "other"])
      true

  An empty list will never match:

      iex> String.starts_with?("elixir", [])
      false

      iex> String.starts_with?("", [])
      false

  """
  @spec starts_with?(t, t | [t]) :: boolean
  def starts_with?(string, prefix) when is_binary(string) and is_binary(prefix) do
    starts_with_string?(string, byte_size(string), prefix)
  end

  def starts_with?(string, prefix) when is_binary(string) and is_list(prefix) do
    string_size = byte_size(string)
    Enum.any?(prefix, &starts_with_string?(string, string_size, &1))
  end

  def starts_with?(string, prefix) when is_binary(string) do
    IO.warn("compiled patterns are deprecated in starts_with?")
    Kernel.match?({0, _}, :binary.match(string, prefix))
  end

  @compile {:inline, starts_with_string?: 3}
  defp starts_with_string?(string, string_size, prefix) when is_binary(prefix) do
    prefix_size = byte_size(prefix)

    if prefix_size <= string_size do
      prefix == binary_part(string, 0, prefix_size)
    else
      false
    end
  end

  @doc """
  Returns `true` if `string` ends with any of the suffixes given.

  `suffixes` can be either a single suffix or a list of suffixes.

  ## Examples

      iex> String.ends_with?("language", "age")
      true
      iex> String.ends_with?("language", ["youth", "age"])
      true
      iex> String.ends_with?("language", ["youth", "elixir"])
      false

  An empty suffix will always match:

      iex> String.ends_with?("language", "")
      true
      iex> String.ends_with?("language", ["", "other"])
      true

  """
  @spec ends_with?(t, t | [t]) :: boolean
  def ends_with?(string, suffix) when is_binary(string) and is_binary(suffix) do
    ends_with_string?(string, byte_size(string), suffix)
  end

  def ends_with?(string, suffix) when is_binary(string) and is_list(suffix) do
    string_size = byte_size(string)
    Enum.any?(suffix, &ends_with_string?(string, string_size, &1))
  end

  @compile {:inline, ends_with_string?: 3}
  defp ends_with_string?(string, string_size, suffix) when is_binary(suffix) do
    suffix_size = byte_size(suffix)

    if suffix_size <= string_size do
      suffix == binary_part(string, string_size - suffix_size, suffix_size)
    else
      false
    end
  end

  @doc """
  Checks if `string` matches the given regular expression.

  ## Examples

      iex> String.match?("foo", ~r/foo/)
      true

      iex> String.match?("bar", ~r/foo/)
      false

  Elixir also provides text-based match operator `=~/2` and function `Regex.match?/2` as
  alternatives to test strings against regular expressions.
  """
  @spec match?(t, Regex.t()) :: boolean
  def match?(string, regex) when is_binary(string) do
    Regex.match?(regex, string)
  end

  @doc """
  Searches if `string` contains any of the given `contents`.

  `contents` can be either a string, a list of strings,
  or a compiled pattern. If `contents` is a list, this
  function will search if any of the strings in `contents`
  are part of `string`.

  > #### Searching for a string in a list {: .tip}
  >
  > If you want to check if `string` is listed in `contents`,
  > where `contents` is a list, use `Enum.member?(contents, string)`
  > instead.

  ## Examples

      iex> String.contains?("elixir of life", "of")
      true
      iex> String.contains?("elixir of life", ["life", "death"])
      true
      iex> String.contains?("elixir of life", ["death", "mercury"])
      false

  The argument can also be a compiled pattern:

      iex> pattern = :binary.compile_pattern(["life", "death"])
      iex> String.contains?("elixir of life", pattern)
      true

  An empty string will always match:

      iex> String.contains?("elixir of life", "")
      true
      iex> String.contains?("elixir of life", ["", "other"])
      true

  An empty list will never match:

      iex> String.contains?("elixir of life", [])
      false

      iex> String.contains?("", [])
      false

  Be aware that this function can match within or across grapheme boundaries.
  For example, take the grapheme "é" which is made of the characters
  "e" and the acute accent. The following returns `true`:

      iex> String.contains?(String.normalize("é", :nfd), "e")
      true

  However, if "é" is represented by the single character "e with acute"
  accent, then it will return `false`:

      iex> String.contains?(String.normalize("é", :nfc), "e")
      false

  """
  @spec contains?(t, [t] | pattern) :: boolean
  def contains?(string, contents) when is_binary(string) and is_list(contents) do
    list_contains?(string, byte_size(string), contents, [])
  end

  def contains?(string, contents) when is_binary(string) do
    "" == contents or :binary.match(string, contents) != :nomatch
  end

  defp list_contains?(string, size, [head | tail], acc) do
    case byte_size(head) do
      0 -> true
      head_size when head_size > size -> list_contains?(string, size, tail, acc)
      _ -> list_contains?(string, size, tail, [head | acc])
    end
  end

  defp list_contains?(_string, _size, [], []),
    do: false

  defp list_contains?(string, _size, [], contents),
    do: :binary.match(string, contents) != :nomatch

  @doc """
  Converts a string into a charlist.

  Specifically, this function takes a UTF-8 encoded binary and returns a list of its integer
  code points. It is similar to `codepoints/1` except that the latter returns a list of code points as
  strings.

  In case you need to work with bytes, take a look at the
  [`:binary` module](`:binary`).

  ## Examples

      iex> String.to_charlist("foo")
      ~c"foo"

  """
  @spec to_charlist(t) :: charlist
  def to_charlist(string) when is_binary(string) do
    case :unicode.characters_to_list(string) do
      result when is_list(result) ->
        result

      {:error, encoded, rest} ->
        raise UnicodeConversionError, encoded: encoded, rest: rest, kind: :invalid

      {:incomplete, encoded, rest} ->
        raise UnicodeConversionError, encoded: encoded, rest: rest, kind: :incomplete
    end
  end

  @doc """
  Converts a string to an existing atom or creates a new one.

  Warning: this function creates atoms dynamically and atoms are
  not garbage-collected. Therefore, `string` should not be an
  untrusted value, such as input received from a socket or during
  a web request. Consider using `to_existing_atom/1` instead.

  By default, the maximum number of atoms is `1_048_576`. This limit
  can be raised or lowered using the VM option `+t`.

  The maximum atom size is of 255 Unicode code points.

  Inlined by the compiler.

  ## Examples

      iex> String.to_atom("my_atom")
      :my_atom

  """
  @spec to_atom(String.t()) :: atom
  def to_atom(string) when is_binary(string) do
    :erlang.binary_to_atom(string, :utf8)
  end

  @doc """
  Converts a string to an existing atom or raises if
  the atom does not exist.

  The maximum atom size is of 255 Unicode code points.
  Raises an `ArgumentError` if the atom does not exist.

  Inlined by the compiler.

  > #### Atoms and modules {: .info}
  >
  > Since Elixir is a compiled language, the atoms defined in a module
  > will only exist after said module is loaded, which typically happens
  > whenever a function in the module is executed. Therefore, it is
  > generally recommended to call `String.to_existing_atom/1` only to
  > convert atoms defined within the module making the function call
  > to `to_existing_atom/1`.
  >
  > To create a module name itself from a string safely,
  > it is recommended to use `Module.safe_concat/1`.

  ## Examples

      iex> _ = :my_atom
      iex> String.to_existing_atom("my_atom")
      :my_atom

  """
  @spec to_existing_atom(String.t()) :: atom
  def to_existing_atom(string) when is_binary(string) do
    :erlang.binary_to_existing_atom(string, :utf8)
  end

  @doc """
  Returns an integer whose text representation is `string`.

  `string` must be the string representation of an integer.
  Otherwise, an `ArgumentError` will be raised. If you want
  to parse a string that may contain an ill-formatted integer,
  use `Integer.parse/1`.

  Inlined by the compiler.

  ## Examples

      iex> String.to_integer("123")
      123

  Passing a string that does not represent an integer leads to an error:

      String.to_integer("invalid data")
      ** (ArgumentError) argument error

  """
  @spec to_integer(String.t()) :: integer
  def to_integer(string) when is_binary(string) do
    :erlang.binary_to_integer(string)
  end

  @doc """
  Returns an integer whose text representation is `string` in base `base`.

  Inlined by the compiler.

  ## Examples

      iex> String.to_integer("3FF", 16)
      1023

  """
  @spec to_integer(String.t(), 2..36) :: integer
  def to_integer(string, base) when is_binary(string) and is_integer(base) do
    :erlang.binary_to_integer(string, base)
  end

  @doc """
  Returns a float whose text representation is `string`.

  `string` must be the string representation of a float including leading digits and a decimal
  point. To parse a string without decimal point as a float, refer to `Float.parse/1`. Otherwise,
  an `ArgumentError` will be raised.

  Inlined by the compiler.

  ## Examples

      iex> String.to_float("2.2017764e+0")
      2.2017764

      iex> String.to_float("3.0")
      3.0

      String.to_float("3")
      ** (ArgumentError) argument error

      String.to_float(".3")
      ** (ArgumentError) argument error

  """
  @spec to_float(String.t()) :: float
  def to_float(string) when is_binary(string) do
    :erlang.binary_to_float(string)
  end

  @doc """
  Computes the bag distance between two strings.

  Returns a float value between 0 and 1 representing the bag
  distance between `string1` and `string2`.

  The bag distance is meant to be an efficient approximation
  of the distance between two strings to quickly rule out strings
  that are largely different.

  The algorithm is outlined in the "String Matching with Metric
  Trees Using an Approximate Distance" paper by Ilaria Bartolini,
  Paolo Ciaccia, and Marco Patella.

  ## Examples

      iex> String.bag_distance("abc", "")
      0.0
      iex> String.bag_distance("abcd", "a")
      0.25
      iex> String.bag_distance("abcd", "ab")
      0.5
      iex> String.bag_distance("abcd", "abc")
      0.75
      iex> String.bag_distance("abcd", "abcd")
      1.0

  """
  @spec bag_distance(t, t) :: float
  @doc since: "1.8.0"
  def bag_distance(_string, ""), do: 0.0
  def bag_distance("", _string), do: 0.0

  def bag_distance(string1, string2) when is_binary(string1) and is_binary(string2) do
    {bag1, length1} = string_to_bag(string1, %{}, 0)
    {bag2, length2} = string_to_bag(string2, %{}, 0)

    diff1 = bag_difference(bag1, bag2)
    diff2 = bag_difference(bag2, bag1)

    1 - max(diff1, diff2) / max(length1, length2)
  end

  defp string_to_bag(string, bag, length) do
    case :unicode_util.gc(string) do
      [gc | rest] -> string_to_bag(rest, bag_store(bag, gc), length + 1)
      [] -> {bag, length}
      {:error, <<byte, rest::bits>>} -> string_to_bag(rest, bag_store(bag, <<byte>>), length + 1)
    end
  end

  defp bag_store(bag, gc) do
    case bag do
      %{^gc => current} -> %{bag | gc => current + 1}
      %{} -> Map.put(bag, gc, 1)
    end
  end

  defp bag_difference(bag1, bag2) do
    Enum.sum_by(bag1, fn {char, count1} ->
      case bag2 do
        %{^char => count2} -> max(count1 - count2, 0)
        %{} -> count1
      end
    end)
  end

  @doc """
  Computes the Jaro distance (similarity) between two strings.

  Returns a float value between `0.0` (equates to no similarity) and `1.0`
  (is an exact match) representing [Jaro](https://en.wikipedia.org/wiki/Jaro-Winkler_distance)
  distance between `string1` and `string2`.

  The Jaro distance metric is designed and best suited for short
  strings such as person names. Elixir itself uses this function
  to provide the "did you mean?" functionality. For instance, when you
  are calling a function in a module and you have a typo in the
  function name, we attempt to suggest the most similar function
  name available, if any, based on the `jaro_distance/2` score.

  ## Examples

      iex> String.jaro_distance("Dwayne", "Duane")
      0.8222222222222223
      iex> String.jaro_distance("even", "odd")
      0.0
      iex> String.jaro_distance("same", "same")
      1.0

  """
  @spec jaro_distance(t, t) :: float
  def jaro_distance(string1, string2)

  def jaro_distance(string, string) when is_binary(string), do: 1.0
  def jaro_distance(_string, ""), do: 0.0
  def jaro_distance("", _string), do: 0.0

  def jaro_distance(string1, string2) when is_binary(string1) and is_binary(string2) do
    # TODO: Replace by :string.jaro_similarity/2 when we require Erlang/OTP 27+
    :elixir_utils.jaro_similarity(string1, string2)
  end

  @doc """
  Returns a keyword list that represents an edit script.

  Check `List.myers_difference/2` for more information.

  ## Examples

      iex> string1 = "fox hops over the dog"
      iex> string2 = "fox jumps over the lazy cat"
      iex> String.myers_difference(string1, string2)
      [eq: "fox ", del: "ho", ins: "jum", eq: "ps over the ", del: "dog", ins: "lazy cat"]

  """
  @doc since: "1.3.0"
  @spec myers_difference(t, t) :: [{:eq | :ins | :del, t}]
  def myers_difference(string1, string2) when is_binary(string1) and is_binary(string2) do
    graphemes(string1)
    |> List.myers_difference(graphemes(string2))
    |> Enum.map(fn {kind, chars} -> {kind, IO.iodata_to_binary(chars)} end)
  end

  @doc false
  @deprecated "Use String.to_charlist/1 instead"
  @spec to_char_list(t) :: charlist
  def to_char_list(string), do: String.to_charlist(string)

  ## Helpers

  @compile {:inline,
            codepoint_byte_size: 1,
            grapheme_byte_size: 1,
            grapheme_to_binary: 1,
            reverse_characters_to_binary: 1}

  defp byte_size_unicode(binary) when is_binary(binary), do: byte_size(binary)
  defp byte_size_unicode([head]), do: byte_size_unicode(head)
  defp byte_size_unicode([head | tail]), do: byte_size_unicode(head) + byte_size_unicode(tail)

  defp byte_size_remaining_at(unicode, 0) do
    byte_size_unicode(unicode)
  end

  defp byte_size_remaining_at(unicode, n) do
    case :unicode_util.gc(unicode) do
      [_] -> 0
      [_ | rest] -> byte_size_remaining_at(rest, n - 1)
      [] -> 0
      {:error, <<_, bin::bits>>} -> byte_size_remaining_at(bin, n - 1)
    end
  end

  defp codepoint_byte_size(cp) when cp <= 0x007F, do: 1
  defp codepoint_byte_size(cp) when cp <= 0x07FF, do: 2
  defp codepoint_byte_size(cp) when cp <= 0xFFFF, do: 3
  defp codepoint_byte_size(_), do: 4

  defp grapheme_to_binary(cp) when is_integer(cp), do: <<cp::utf8>>
  defp grapheme_to_binary(gc) when is_list(gc), do: for(cp <- gc, do: <<cp::utf8>>, into: "")

  defp grapheme_byte_size(cp) when is_integer(cp), do: codepoint_byte_size(cp)
  defp grapheme_byte_size(cps), do: grapheme_byte_size(cps, 0)

  defp grapheme_byte_size([cp | cps], acc),
    do: grapheme_byte_size(cps, acc + codepoint_byte_size(cp))

  defp grapheme_byte_size([], acc),
    do: acc

  defp reverse_characters_to_binary(acc),
    do: acc |> :lists.reverse() |> :unicode.characters_to_binary()
end
