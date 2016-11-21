import Kernel, except: [length: 1]

defmodule String do
  @moduledoc ~S"""
  A String in Elixir is a UTF-8 encoded binary.

  ## Codepoints and grapheme cluster

  The functions in this module act according to the Unicode
  Standard, version 9.0.0.

  As per the standard, a codepoint is a single Unicode Character,
  which may be represented by one or more bytes.

  For example, the codepoint "é" is two bytes:

      iex> byte_size("é")
      2

  However, this module returns the proper length:

      iex> String.length("é")
      1

  Furthermore, this module also presents the concept of grapheme cluster
  (from now on referenced as graphemes). Graphemes can consist of multiple
  codepoints that may be perceived as a single character by readers. For
  example, "é" can be represented either as a single "e with acute" codepoint
  or as the letter "e" followed by a "combining acute accent" (two codepoints):

      iex> string = "\u0065\u0301"
      iex> byte_size(string)
      3
      iex> String.length(string)
      1
      iex> String.codepoints(string)
      ["e", "́"]
      iex> String.graphemes(string)
      ["é"]

  Although the example above is made of two characters, it is
  perceived by users as one.

  Graphemes can also be two characters that are interpreted
  as one by some languages. For example, some languages may
  consider "ch" as a single character. However, since this
  information depends on the locale, it is not taken into account
  by this module.

  In general, the functions in this module rely on the Unicode
  Standard, but do not contain any of the locale specific behaviour.

  More information about graphemes can be found in the [Unicode
  Standard Annex #29](http://www.unicode.org/reports/tr29/).
  The current Elixir version implements Extended Grapheme Cluster
  algorithm.

  ## String and binary operations

  To act according to the Unicode Standard, many functions
  in this module run in linear time, as they need to traverse
  the whole string considering the proper Unicode codepoints.

  For example, `String.length/1` will take longer as
  the input grows. On the other hand, `Kernel.byte_size/1` always runs
  in constant time (i.e. regardless of the input size).

  This means often there are performance costs in using the
  functions in this module, compared to the more low-level
  operations that work directly with binaries:

    * `Kernel.binary_part/3` - retrieves part of the binary
    * `Kernel.bit_size/1` and `Kernel.byte_size/1` - size related functions
    * `Kernel.is_bitstring/1` and `Kernel.is_binary/1` - type checking function
    * Plus a number of functions for working with binaries (bytes)
      in the [`:binary` module](http://www.erlang.org/doc/man/binary.html)

  There are many situations where using the `String` module can
  be avoided in favor of binary functions or pattern matching.
  For example, imagine you have a string `prefix` and you want to
  remove this prefix from another string named `full`.

  One may be tempted to write:

      iex> take_prefix = fn full, prefix ->
      ...>   base = String.length(prefix)
      ...>   String.slice(full, base, String.length(full) - base)
      ...> end
      iex> take_prefix.("Mr. John", "Mr. ")
      "John"

  Although the function above works, it performs poorly. To
  calculate the length of the string, we need to traverse it
  fully, so we traverse both `prefix` and `full` strings, then
  slice the `full` one, traversing it again.

  A first attempt at improving it could be with ranges:

      iex> take_prefix = fn full, prefix ->
      ...>   base = String.length(prefix)
      ...>   String.slice(full, base..-1)
      ...> end
      iex> take_prefix.("Mr. John", "Mr. ")
      "John"

  While this is much better (we don't traverse `full` twice),
  it could still be improved. In this case, since we want to
  extract a substring from a string, we can use `Kernel.byte_size/1`
  and `Kernel.binary_part/3` as there is no chance we will slice in
  the middle of a codepoint made of more than one byte:

      iex> take_prefix = fn full, prefix ->
      ...>   base = byte_size(prefix)
      ...>   binary_part(full, base, byte_size(full) - base)
      ...> end
      iex> take_prefix.("Mr. John", "Mr. ")
      "John"

  Or simply use pattern matching:

      iex> take_prefix = fn full, prefix ->
      ...>   base = byte_size(prefix)
      ...>   <<_::binary-size(base), rest::binary>> = full
      ...>   rest
      ...> end
      iex> take_prefix.("Mr. John", "Mr. ")
      "John"

  On the other hand, if you want to dynamically slice a string
  based on an integer value, then using `String.slice/3` is the
  best option as it guarantees we won't incorrectly split a valid
  codepoint into multiple bytes.

  ## Integer codepoints

  Although codepoints could be represented as integers, this
  module represents all codepoints as strings. For example:

      iex> String.codepoints("olá")
      ["o", "l", "á"]

  There are a couple of ways to retrieve a character integer
  codepoint. One may use the `?` construct:

      iex> ?o
      111

      iex> ?á
      225

  Or also via pattern matching:

      iex> <<aacute::utf8>> = "á"
      iex> aacute
      225

  As we have seen above, codepoints can be inserted into
  a string by their hexadecimal code:

      "ol\u0061\u0301" #=>
      "olá"

  ## Self-synchronization

  The UTF-8 encoding is self-synchronizing. This means that
  if malformed data (i.e., data that is not possible according
  to the definition of the encoding) is encountered, only one
  codepoint needs to be rejected.

  This module relies on this behaviour to ignore such invalid
  characters. For example, `length/1` will return
  a correct result even if an invalid codepoint is fed into it.

  In other words, this module expects invalid data to be detected
  elsewhere, usually when retrieving data from the external source.
  For example, a driver that reads strings from a database will be
  responsible to check the validity of the encoding. `String.chunk/2`
  can be used for breaking a string into valid and invalid parts.

  ## Patterns

  Many functions in this module work with patterns. For example,
  `String.split/2` can split a string into multiple patterns given
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
  be done over and over again. Note though the compiled
  pattern cannot be stored in a module attribute as the pattern
  is generated at runtime and does not survive compile term.
  """

  @type t :: binary
  @type codepoint :: t
  @type grapheme :: t
  @type pattern :: t | [t] | :binary.cp

  @doc """
  Checks if a string contains only printable characters.

  ## Examples

      iex> String.printable?("abc")
      true

  """
  @spec printable?(t) :: boolean
  def printable?(string)

  for char <- 0x20..0x7E do
    def printable?(<<unquote(char), rest::binary>>) do
      printable?(rest)
    end
  end
  def printable?(<<?\n, rest::binary>>), do: printable?(rest)
  def printable?(<<?\r, rest::binary>>), do: printable?(rest)
  def printable?(<<?\t, rest::binary>>), do: printable?(rest)
  def printable?(<<?\v, rest::binary>>), do: printable?(rest)
  def printable?(<<?\b, rest::binary>>), do: printable?(rest)
  def printable?(<<?\f, rest::binary>>), do: printable?(rest)
  def printable?(<<?\e, rest::binary>>), do: printable?(rest)
  def printable?(<<?\d, rest::binary>>), do: printable?(rest)
  def printable?(<<?\a, rest::binary>>), do: printable?(rest)

  def printable?(<<char::utf8, rest::binary>>)
      when char in 0xA0..0xD7FF
      when char in 0xE000..0xFFFD
      when char in 0x10000..0x10FFFF do
    printable?(rest)
  end

  def printable?(<<>>), do: true
  def printable?(binary) when is_binary(binary), do: false

  @doc ~S"""
  Divides a string into substrings at each Unicode whitespace
  occurrence with leading and trailing whitespace ignored. Groups
  of whitespace are treated as a single occurrence. Divisions do
  not occur on non-breaking whitespace.

  ## Examples

      iex> String.split("foo bar")
      ["foo", "bar"]

      iex> String.split("foo" <> <<194, 133>> <> "bar")
      ["foo", "bar"]

      iex> String.split(" foo   bar ")
      ["foo", "bar"]

      iex> String.split("no\u00a0break")
      ["no\u00a0break"]

  """
  @spec split(t) :: [t]
  defdelegate split(binary), to: String.Break

  @doc ~S"""
  Divides a string into substrings based on a pattern.

  Returns a list of these substrings. The pattern can
  be a string, a list of strings or a regular expression.

  The string is split into as many parts as possible by
  default, but can be controlled via the `parts: pos_integer` option.
  If you pass `parts: :infinity`, it will return all possible parts
  (`:infinity` is the default).

  Empty strings are only removed from the result if the
  `trim` option is set to `true` (default is `false`).

  When the pattern used is a regular expression, the string is
  split using `Regex.split/3`. In that case this function accepts
  additional options which are documented in `Regex.split/3`.

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

  Splitting on empty patterns returns graphemes:

      iex> String.split("abc", ~r{})
      ["a", "b", "c", ""]

      iex> String.split("abc", "")
      ["a", "b", "c", ""]

      iex> String.split("abc", "", trim: true)
      ["a", "b", "c"]

      iex> String.split("abc", "", parts: 2)
      ["a", "bc"]

  A precompiled pattern can also be given:

      iex> pattern = :binary.compile_pattern([" ", ","])
      iex> String.split("1,2 3,4", pattern)
      ["1", "2", "3", "4"]

  """
  @spec split(t, pattern | Regex.t) :: [t]
  @spec split(t, pattern | Regex.t, Keyword.t) :: [t]
  def split(string, pattern, options \\ [])

  def split(string, %Regex{} = pattern, options) when is_binary(string) do
    Regex.split(pattern, string, options)
  end

  def split(string, pattern, []) when is_binary(string) and pattern != "" do
    :binary.split(string, pattern, [:global])
  end

  def split(string, pattern, options) when is_binary(string) do
    parts   = Keyword.get(options, :parts, :infinity)
    trim    = Keyword.get(options, :trim, false)
    pattern = maybe_compile_pattern(pattern)
    split_each(string, pattern, trim, parts_to_index(parts))
  end

  defp parts_to_index(:infinity),                      do: 0
  defp parts_to_index(n) when is_integer(n) and n > 0, do: n

  defp split_each("", _pattern, true, 1), do: []
  defp split_each(string, _pattern, _trim, 1) when is_binary(string), do: [string]
  defp split_each(string, pattern, trim, count) do
    case do_splitter(string, pattern, trim) do
      {h, t} -> [h | split_each(t, pattern, trim, count - 1)]
      nil    -> []
    end
  end

  @doc """
  Returns an enumerable that splits a string on demand.

  This is in contrast to `split/3` which splits all
  the string upfront.

  Note splitter does not support regular expressions
  (as it is often more efficient to have the regular
  expressions traverse the string at once than in
  multiple passes).

  ## Options

    * :trim - when `true`, does not emit empty patterns

  ## Examples

      iex> String.splitter("1,2 3,4 5,6 7,8,...,99999", [" ", ","]) |> Enum.take(4)
      ["1", "2", "3", "4"]

      iex> String.splitter("abcd", "") |> Enum.take(10)
      ["a", "b", "c", "d", ""]

      iex> String.splitter("abcd", "", trim: true) |> Enum.take(10)
      ["a", "b", "c", "d"]

  """
  @spec splitter(t, pattern, Keyword.t) :: Enumerable.t
  def splitter(string, pattern, options \\ []) do
    pattern = maybe_compile_pattern(pattern)
    trim    = Keyword.get(options, :trim, false)
    Stream.unfold(string, &do_splitter(&1, pattern, trim))
  end

  defp do_splitter(:nomatch, _pattern, _), do: nil
  defp do_splitter("", _pattern, true),    do: nil
  defp do_splitter("", _pattern, false),   do: {"", :nomatch}

  defp do_splitter(bin, "", _trim) do
    next_grapheme(bin)
  end

  defp do_splitter(bin, pattern, trim) do
    case :binary.match(bin, pattern) do
      {0, length} when trim ->
        do_splitter(:binary.part(bin, length, byte_size(bin) - length), pattern, trim)
      {pos, length} ->
        final = pos + length
        {:binary.part(bin, 0, pos),
         :binary.part(bin, final, byte_size(bin) - final)}
      :nomatch ->
        {bin, :nomatch}
    end
  end

  defp maybe_compile_pattern(""), do: ""
  defp maybe_compile_pattern(pattern) when is_tuple(pattern), do: pattern
  defp maybe_compile_pattern(pattern), do: :binary.compile_pattern(pattern)

  @doc """
  Splits a string into two at the specified offset. When the offset given is
  negative, location is counted from the end of the string.

  The offset is capped to the length of the string. Returns a tuple with
  two elements.

  Note: keep in mind this function splits on graphemes and for such it
  has to linearly traverse the string. If you want to split a string or
  a binary based on the number of bytes, use `Kernel.binary_part/3`
  instead.

  ## Examples

      iex> String.split_at "sweetelixir", 5
      {"sweet", "elixir"}

      iex> String.split_at "sweetelixir", -6
      {"sweet", "elixir"}

      iex> String.split_at "abc", 0
      {"", "abc"}

      iex> String.split_at "abc", 1000
      {"abc", ""}

      iex> String.split_at "abc", -1000
      {"", "abc"}

  """
  @spec split_at(t, integer) :: {t, t}
  def split_at(string, position)

  def split_at(string, position) when is_integer(position) and position >= 0 do
    do_split_at(string, position)
  end

  def split_at(string, position) when is_integer(position) and position < 0 do
    position = length(string) - abs(position)
    case position >= 0 do
      true  -> do_split_at(string, position)
      false -> {"", string}
    end
  end

  defp do_split_at(string, position) do
    {byte_size, rest} = String.Unicode.split_at(string, position)
    {binary_part(string, 0, byte_size), rest || ""}
  end

  @doc ~S"""
  Returns `true` if `string1` is canonically equivalent to 'string2'.

  It performs Normalization Form Canonical Decomposition (NFD) on the
  strings before comparing them. This function is equivalent to:

      String.normalize(string1, :nfd) == String.normalize(string2, :nfd)

  Therefore, if you plan to compare multiple strings, multiple times
  in a row, you may normalize them upfront and compare them directly
  to avoid multiple normalization passes.

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
  def equivalent?(string1, string2) do
    normalize(string1, :nfd) == normalize(string2, :nfd)
  end

  @doc """
  Converts all characters in `string` to Unicode normalization
  form identified by `form`.

  ## Forms

  The supported forms are:

    * `:nfd` - Normalization Form Canonical Decomposition.
      Characters are decomposed by canonical equivalence, and
      multiple combining characters are arranged in a specific
      order.

    * `:nfc` - Normalization Form Canonical Composition.
      Characters are decomposed and then recomposed by canonical equivalence.

  ## Examples

      iex> String.normalize("yêṩ", :nfd)
      "yêṩ"

      iex> String.normalize("leña", :nfc)
      "leña"

  """
  @spec normalize(t, atom) :: t
  defdelegate normalize(string, form), to: String.Normalizer

  @doc """
  Converts all characters in the given string to uppercase.

  ## Examples

      iex> String.upcase("abcd")
      "ABCD"

      iex> String.upcase("ab 123 xpto")
      "AB 123 XPTO"

      iex> String.upcase("olá")
      "OLÁ"

  """
  @spec upcase(t) :: t
  defdelegate upcase(binary), to: String.Casing

  @doc """
  Converts all characters in the given string to lowercase.

  ## Examples

      iex> String.downcase("ABCD")
      "abcd"

      iex> String.downcase("AB 123 XPTO")
      "ab 123 xpto"

      iex> String.downcase("OLÁ")
      "olá"

  """
  @spec downcase(t) :: t
  defdelegate downcase(binary), to: String.Casing

  @doc """
  Converts the first character in the given string to
  uppercase and the remainder to lowercase.

  This relies on the titlecase information provided
  by the Unicode Standard. Note this function makes
  no attempt to capitalize all words in the string
  (usually known as titlecase).

  ## Examples

      iex> String.capitalize("abcd")
      "Abcd"

      iex> String.capitalize("ﬁn")
      "Fin"

      iex> String.capitalize("olá")
      "Olá"

  """
  @spec capitalize(t) :: t
  def capitalize(string) when is_binary(string) do
    {char, rest} = String.Casing.titlecase_once(string)
    char <> downcase(rest)
  end

  @doc false
  # TODO: Deprecate by 1.5
  defdelegate rstrip(binary), to: String.Break, as: :trim_trailing

  @doc false
  # TODO: Deprecate by 1.5
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

  """
  @spec replace_leading(t, t, t) :: t | no_return
  def replace_leading(string, match, replacement)
      when is_binary(string) and is_binary(match) and is_binary(replacement) do
    if match == "" do
      raise ArgumentError, "cannot use an empty string as the match to replace"
    end

    prefix_size = byte_size(match)
    suffix_size = byte_size(string) - prefix_size
    replace_leading(string, match, replacement, prefix_size, suffix_size, 0)
  end

  defp replace_leading(string, match, replacement, prefix_size, suffix_size, acc) when suffix_size >= 0 do
    case string do
      <<prefix::size(prefix_size)-binary, suffix::size(suffix_size)-binary>> when prefix == match ->
        replace_leading(suffix, match, replacement, prefix_size, suffix_size - prefix_size, acc + 1)
      _ ->
        duplicate(replacement, acc) <> string
    end
  end

  defp replace_leading(string, _match, replacement, _prefix_size, _suffix_size, acc) do
    duplicate(replacement, acc) <> string
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

  """
  @spec replace_trailing(t, t, t) :: t | no_return
  def replace_trailing(string, match, replacement)
      when is_binary(string) and is_binary(match) and is_binary(replacement) do
    if match == "" do
      raise ArgumentError, "cannot use an empty string as the match to replace"
    end

    suffix_size = byte_size(match)
    prefix_size = byte_size(string) - suffix_size
    replace_trailing(string, match, replacement, prefix_size, suffix_size, 0)
  end

  defp replace_trailing(string, match, replacement, prefix_size, suffix_size, acc) when prefix_size >= 0 do
    case string do
      <<prefix::size(prefix_size)-binary, suffix::size(suffix_size)-binary>> when suffix == match ->
        replace_trailing(prefix, match, replacement, prefix_size - suffix_size, suffix_size, acc + 1)
      _ ->
        string <> duplicate(replacement, acc)
    end
  end

  defp replace_trailing(string, _match, replacement, _prefix_size, _suffix_size, acc) do
    string <> duplicate(replacement, acc)
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

  """
  @spec replace_prefix(t, t, t) :: t
  def replace_prefix(string, match, replacement)
      when is_binary(string) and is_binary(match) and is_binary(replacement) do
    prefix_size = byte_size(match)
    suffix_size = byte_size(string) - prefix_size

    case string do
      <<prefix::size(prefix_size)-binary, suffix::size(suffix_size)-binary>> when prefix == match ->
        replacement <> suffix
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

  """
  @spec replace_suffix(t, t, t) :: t
  def replace_suffix(string, match, replacement)
      when is_binary(string) and is_binary(match) and is_binary(replacement) do
    suffix_size = byte_size(match)
    prefix_size = byte_size(string) - suffix_size

    case string do
      <<prefix::size(prefix_size)-binary, suffix::size(suffix_size)-binary>> when suffix == match ->
        prefix <> replacement
      _ ->
        string
    end
  end

  @doc false
  # TODO: Deprecate by 1.5
  defdelegate lstrip(binary), to: String.Break, as: :trim_leading

  @doc false
  # TODO: Deprecate by 1.5
  def lstrip(string, char) when is_integer(char) do
    replace_leading(string, <<char::utf8>>, "")
  end

  @doc false
  # TODO: Deprecate by 1.5
  def strip(string) do
    trim(string)
  end

  @doc false
  # TODO: Deprecate by 1.5
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
  Returns a string where all leading `to_trim`s have been removed.

  ## Examples

      iex> String.trim_leading("__ abc _", "_")
      " abc _"

      iex> String.trim_leading("1 abc", "11")
      "1 abc"

  """
  @spec trim_leading(t, t) :: t
  def trim_leading(string, to_trim) do
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
  Returns a string where all trailing `to_trim`s have been removed.

  ## Examples

      iex> String.trim_trailing("_ abc __", "_")
      "_ abc "

      iex> String.trim_trailing("abc 1", "11")
      "abc 1"

  """
  @spec trim_trailing(t, t) :: t
  def trim_trailing(string, to_trim) do
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
  def trim(string) do
    string
    |> trim_leading()
    |> trim_trailing()
  end

  @doc """
  Returns a string where all leading and trailing `to_trim`s have been
  removed.

  ## Examples

      iex> String.trim("a  abc  a", "a")
      "  abc  "

  """
  @spec trim(t, t) :: t
  def trim(string, to_trim) do
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

  Raises `ArgumentError` if the given `padding` contains non-string element.

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

  Raises `ArgumentError` if the given `padding` contains non-string element.

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
    string_len = length(string)
    if string_len >= count do
      string
    else
      filler = build_filler(count - string_len, padding, padding, 0, [])
      case kind do
        :leading -> [filler | string]
        :trailing -> [string | filler]
      end
      |> IO.iodata_to_binary
    end
  end

  defp build_filler(0, _source, _padding, _size, filler), do: filler

  defp build_filler(count, source, [], size, filler) do
    rem_filler =
      rem(count, size)
      |> build_filler(source, source, 0, [])
    filler =
      filler
      |> IO.iodata_to_binary
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
  # TODO: Deprecate by 1.5
  def rjust(subject, len, pad \\ ?\s) when is_integer(pad) and is_integer(len) and len >= 0 do
    pad(:leading, subject, len, [<<pad::utf8>>])
  end

  @doc false
  # TODO: Deprecate by 1.5
  def ljust(subject, len, pad \\ ?\s) when is_integer(pad) and is_integer(len) and len >= 0 do
    pad(:trailing, subject, len, [<<pad::utf8>>])
  end

  @doc ~S"""
  Returns a new string created by replacing occurrences of `pattern` in
  `subject` with `replacement`.

  By default, it replaces all occurrences, unless the `global` option is
  set to `false`, where it will only replace the first one

  The `pattern` may be a string or a regular expression.

  ## Examples

      iex> String.replace("a,b,c", ",", "-")
      "a-b-c"

      iex> String.replace("a,b,c", ",", "-", global: false)
      "a-b,c"

  When the pattern is a regular expression, one can give `\N` or
  `\g{N}` in the `replacement` string to access a specific capture in the
  regular expression:

      iex> String.replace("a,b,c", ~r/,(.)/, ",\\1\\g{1}")
      "a,bb,cc"

  Notice we had to escape the backslash escape character (i.e., we used `\\N`
  instead of just `\N` to escape the backslash; same thing for `\\g{N}`). By
  giving `\0`, one can inject the whole matched pattern in the replacement
  string.

  When the pattern is a string, a developer can use the replaced part inside
  the `replacement` by using the `:insert_replaced` option and specifying the
  position(s) inside the `replacement` where the string pattern will be
  inserted:

      iex> String.replace("a,b,c", "b", "[]", insert_replaced: 1)
      "a,[b],c"

      iex> String.replace("a,b,c", ",", "[]", insert_replaced: 2)
      "a[],b[],c"

      iex> String.replace("a,b,c", ",", "[]", insert_replaced: [1, 1])
      "a[,,]b[,,]c"

  If any position given in the `:insert_replaced` option is larger than the
  replacement string, or is negative, an `ArgumentError` is raised.
  """
  @spec replace(t, pattern | Regex.t, t, Keyword.t) :: t
  def replace(subject, pattern, replacement, options \\ []) when is_binary(replacement) do
    if Regex.regex?(pattern) do
      Regex.replace(pattern, subject, replacement, global: options[:global])
    else
      opts = translate_replace_options(options)
      :binary.replace(subject, pattern, replacement, opts)
    end
  end

  defp translate_replace_options(options) do
    global =
      if Keyword.get(options, :global) != false,
        do: [:global],
        else: []

    insert =
      if insert = Keyword.get(options, :insert_replaced),
        do: [{:insert_replaced, insert}],
        else: []

    global ++ insert
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
      iex> String.reverse String.reverse("̀e")
      "è"

  In the first example the accent is before the vowel, so
  it is considered two graphemes. However, when you reverse
  it once, you have the vowel followed by the accent, which
  becomes one grapheme. Reversing it again will keep it as
  one single grapheme.
  """
  @spec reverse(t) :: t
  def reverse(string) do
    do_reverse(next_grapheme(string), [])
  end

  defp do_reverse({grapheme, rest}, acc) do
    do_reverse(next_grapheme(rest), [grapheme | acc])
  end

  defp do_reverse(nil, acc), do: IO.iodata_to_binary(acc)

  @doc """
  Returns a string `subject` duplicated `n` times.

  ## Examples

      iex> String.duplicate("abc", 0)
      ""

      iex> String.duplicate("abc", 1)
      "abc"

      iex> String.duplicate("abc", 2)
      "abcabc"

  """
  @spec duplicate(t, non_neg_integer) :: t
  def duplicate(subject, n) when is_integer(n) and n >= 0 do
    :binary.copy(subject, n)
  end

  @doc """
  Returns all codepoints in the string.

  For details about codepoints and graphemes, see the `String` module documentation.

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
  defdelegate codepoints(string), to: String.Unicode

  @doc """
  Returns the next codepoint in a string.

  The result is a tuple with the codepoint and the
  remainder of the string or `nil` in case
  the string reached its end.

  As with other functions in the String module, this
  function does not check for the validity of the codepoint.
  That said, if an invalid codepoint is found, it will
  be returned by this function.

  ## Examples

      iex> String.next_codepoint("olá")
      {"o", "lá"}

  """
  @compile {:inline, next_codepoint: 1}
  @spec next_codepoint(t) :: {codepoint, t} | nil
  defdelegate next_codepoint(string), to: String.Unicode

  @doc ~S"""
  Checks whether `string` contains only valid characters.

  ## Examples

      iex> String.valid?("a")
      true

      iex> String.valid?("ø")
      true

      iex> String.valid?(<<0xFFFF :: 16>>)
      false

      iex> String.valid?("asd" <> <<0xFFFF :: 16>>)
      false

  """
  @spec valid?(t) :: boolean
  def valid?(string)

  noncharacters = Enum.to_list(0xFDD0..0xFDEF) ++
    [0x0FFFE, 0x0FFFF, 0x1FFFE, 0x1FFFF, 0x2FFFE, 0x2FFFF,
     0x3FFFE, 0x3FFFF, 0x4FFFE, 0x4FFFF, 0x5FFFE, 0x5FFFF,
     0x6FFFE, 0x6FFFF, 0x7FFFE, 0x7FFFF, 0x8FFFE, 0x8FFFF,
     0x9FFFE, 0x9FFFF, 0x10FFFE, 0x10FFFF]

  for noncharacter <- noncharacters do
    def valid?(<<unquote(noncharacter)::utf8, _::binary>>), do: false
  end

  def valid?(<<_::utf8, t::binary>>), do: valid?(t)
  def valid?(<<>>), do: true
  def valid?(_), do: false

  @doc false
  # TODO: Remove on 2.0
  def valid_character?(string) do
    IO.warn "String.valid_character?/1 is deprecated, please use valid?/1 instead"
    case string do
      <<_::utf8>> -> valid?(string)
      _ -> false
    end
  end

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

      iex> String.chunk(<<?a, ?b, ?c, 0, 0x0FFFF::utf8>>, :valid)
      ["abc\0", <<0x0FFFF::utf8>>]

      iex> String.chunk(<<?a, ?b, ?c, 0, 0x0FFFF::utf8>>, :printable)
      ["abc", <<0, 0x0FFFF::utf8>>]

  """
  @spec chunk(t, :valid | :printable) :: [t]

  def chunk(string, trait)

  def chunk("", _), do: []

  def chunk(string, trait) when trait in [:valid, :printable] do
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

  @doc """
  Returns Unicode graphemes in the string as per Extended Grapheme
  Cluster algorithm.

  The algorithm is outlined in the [Unicode Standard Annex #29,
  Unicode Text Segmentation](http://www.unicode.org/reports/tr29/).

  For details about codepoints and graphemes, see the `String` module documentation.

  ## Examples

      iex> String.graphemes("Ńaïve")
      ["Ń", "a", "ï", "v", "e"]

      iex> String.graphemes("\u00e9")
      ["é"]

      iex> String.graphemes("\u0065\u0301")
      ["é"]

  """
  @spec graphemes(t) :: [grapheme]
  defdelegate graphemes(string), to: String.Unicode

  @compile {:inline, next_grapheme: 1, next_grapheme_size: 1}

  @doc """
  Returns the next grapheme in a string.

  The result is a tuple with the grapheme and the
  remainder of the string or `nil` in case
  the String reached its end.

  ## Examples

      iex> String.next_grapheme("olá")
      {"o", "lá"}

  """
  @spec next_grapheme(t) :: {grapheme, t} | nil
  def next_grapheme(binary) do
    case next_grapheme_size(binary) do
      {size, rest} -> {:binary.part(binary, 0, size), rest}
      nil          -> nil
    end
  end

  @doc """
  Returns the size of the next grapheme.

  The result is a tuple with the next grapheme size and
  the remainder of the string or `nil` in case the string
  reached its end.

  ## Examples

      iex> String.next_grapheme_size("olá")
      {1, "lá"}

  """
  @spec next_grapheme_size(t) :: {pos_integer, t} | nil
  defdelegate next_grapheme_size(string), to: String.Unicode

  @doc """
  Returns the first grapheme from a UTF-8 string,
  `nil` if the string is empty.

  ## Examples

      iex> String.first("elixir")
      "e"

      iex> String.first("եոգլի")
      "ե"

  """
  @spec first(t) :: grapheme | nil
  def first(string) do
    case next_grapheme(string) do
      {char, _} -> char
      nil -> nil
    end
  end

  @doc """
  Returns the last grapheme from a UTF-8 string,
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

  defp do_last(nil, last_char), do: last_char

  @doc """
  Returns the number of Unicode graphemes in a UTF-8 string.

  ## Examples

      iex> String.length("elixir")
      6

      iex> String.length("եոգլի")
      5

  """
  @spec length(t) :: non_neg_integer
  defdelegate length(string), to: String.Unicode

  @doc """
  Returns the grapheme at the `position` of the given UTF-8 `string`.
  If `position` is greater than `string` length, then it returns `nil`.

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

  def at(string, position) when is_integer(position) and position >= 0 do
    do_at(string, position)
  end

  def at(string, position) when is_integer(position) and position < 0 do
    position = length(string) - abs(position)
    case position >= 0 do
      true  -> do_at(string, position)
      false -> nil
    end
  end

  defp do_at(string, position) do
    case String.Unicode.split_at(string, position) do
      {_, nil}  -> nil
      {_, rest} -> first(rest)
    end
  end

  @doc """
  Returns a substring starting at the offset `start`, and of
  length `len`.

  If the offset is greater than string length, then it returns `""`.

  Remember this function works with Unicode graphemes and considers
  the slices to represent grapheme offsets. If you want to split
  on raw bytes, check `Kernel.binary_part/3` instead.

  ## Examples

      iex> String.slice("elixir", 1, 3)
      "lix"

      iex> String.slice("elixir", 1, 10)
      "lixir"

      iex> String.slice("elixir", 10, 3)
      ""

      iex> String.slice("elixir", -4, 4)
      "ixir"

      iex> String.slice("elixir", -10, 3)
      ""

      iex> String.slice("a", 0, 1500)
      "a"

      iex> String.slice("a", 1, 1500)
      ""

      iex> String.slice("a", 2, 1500)
      ""

  """
  @spec slice(t, integer, integer) :: grapheme

  def slice(_, _, 0) do
    ""
  end

  def slice(string, start, len) when start >= 0 and len >= 0 do
    case String.Unicode.split_at(string, start) do
      {_, nil} -> ""
      {start_bytes, rest} ->
        {len_bytes, _} = String.Unicode.split_at(rest, len)
        binary_part(string, start_bytes, len_bytes)
    end
  end

  def slice(string, start, len) when start < 0 and len >= 0 do
    start = length(string) + start
    case start >= 0 do
      true  -> slice(string, start, len)
      false -> ""
    end
  end

  @doc """
  Returns a substring from the offset given by the start of the
  range to the offset given by the end of the range.

  If the start of the range is not a valid offset for the given
  string or if the range is in reverse order, returns `""`.

  If the start or end of the range is negative, the whole string
  is traversed first in order to convert the negative indices into
  positive ones.

  Remember this function works with Unicode graphemes and considers
  the slices to represent grapheme offsets. If you want to split
  on raw bytes, check `Kernel.binary_part/3` instead.

  ## Examples

      iex> String.slice("elixir", 1..3)
      "lix"

      iex> String.slice("elixir", 1..10)
      "lixir"

      iex> String.slice("elixir", 10..3)
      ""

      iex> String.slice("elixir", -4..-1)
      "ixir"

      iex> String.slice("elixir", 2..-1)
      "ixir"

      iex> String.slice("elixir", -4..6)
      "ixir"

      iex> String.slice("elixir", -1..-4)
      ""

      iex> String.slice("elixir", -10..-7)
      ""

      iex> String.slice("a", 0..1500)
      "a"

      iex> String.slice("a", 1..1500)
      ""

  """
  @spec slice(t, Range.t) :: t

  def slice(string, range)

  def slice("", _.._), do: ""

  def slice(string, first..-1) when first >= 0 do
    case String.Unicode.split_at(string, first) do
      {_, nil} ->
        ""
      {start_bytes, _} ->
        binary_part(string, start_bytes, byte_size(string) - start_bytes)
    end
  end

  def slice(string, first..last) when first >= 0 and last >= 0 do
    if last >= first do
      slice(string, first, last - first + 1)
    else
      ""
    end
  end

  def slice(string, first..last) do
    {bytes, length} = do_acc_bytes(next_grapheme_size(string), [], 0)

    first = add_if_negative(first, length)
    last  = add_if_negative(last, length)

    if first < 0 or first > last or first > length do
      ""
    else
      last  = min(last + 1, length)
      bytes = Enum.drop(bytes, length - last)
      first = last - first
      {length_bytes, start_bytes} = Enum.split(bytes, first)
      binary_part(string, Enum.sum(start_bytes), Enum.sum(length_bytes))
    end
  end

  defp add_if_negative(value, to_add) when value < 0, do: value + to_add
  defp add_if_negative(value, _to_add), do: value

  defp do_acc_bytes({size, rest}, bytes, length) do
    do_acc_bytes(next_grapheme_size(rest), [size | bytes], length + 1)
  end

  defp do_acc_bytes(nil, bytes, length) do
    {bytes, length}
  end

  @doc """
  Returns `true` if `string` starts with any of the prefixes given.

  `prefix` can be either a single prefix or a list of prefixes.

  ## Examples

      iex> String.starts_with? "elixir", "eli"
      true
      iex> String.starts_with? "elixir", ["erlang", "elixir"]
      true
      iex> String.starts_with? "elixir", ["erlang", "ruby"]
      false

  An empty string will always match:

      iex> String.starts_with? "elixir", ""
      true
      iex> String.starts_with? "elixir", ["", "other"]
      true

  """
  @spec starts_with?(t, t | [t]) :: boolean
  def starts_with?(string, []) when is_binary(string) do
    false
  end

  def starts_with?(string, prefix) when is_binary(string) and is_list(prefix) do
    "" in prefix or Kernel.match?({0, _}, :binary.match(string, prefix))
  end

  def starts_with?(string, prefix) when is_binary(string) do
    "" == prefix or Kernel.match?({0, _}, :binary.match(string, prefix))
  end

  @doc """
  Returns `true` if `string` ends with any of the suffixes given.

  `suffixes` can be either a single suffix or a list of suffixes.

  ## Examples

      iex> String.ends_with? "language", "age"
      true
      iex> String.ends_with? "language", ["youth", "age"]
      true
      iex> String.ends_with? "language", ["youth", "elixir"]
      false

  An empty suffix will always match:

      iex> String.ends_with? "language", ""
      true
      iex> String.ends_with? "language", ["", "other"]
      true

  """
  @spec ends_with?(t, t | [t]) :: boolean
  def ends_with?(string, suffixes) when is_binary(string) and is_list(suffixes) do
    Enum.any?(suffixes, &do_ends_with(string, &1))
  end

  def ends_with?(string, suffix) when is_binary(string) do
    do_ends_with(string, suffix)
  end

  defp do_ends_with(_string, "") do
    true
  end

  defp do_ends_with(string, suffix) when is_binary(suffix) do
    string_size = byte_size(string)
    suffix_size = byte_size(suffix)
    scope = {string_size - suffix_size, suffix_size}
    (suffix_size <= string_size) and (:nomatch != :binary.match(string, suffix, [scope: scope]))
  end

  @doc """
  Checks if `string` matches the given regular expression.

  ## Examples

      iex> String.match?("foo", ~r/foo/)
      true

      iex> String.match?("bar", ~r/foo/)
      false

  """
  @spec match?(t, Regex.t) :: boolean
  def match?(string, regex) do
    Regex.match?(regex, string)
  end

  @doc """
  Checks if `string` contains any of the given `contents`.

  `contents` can be either a single string or a list of strings.

  ## Examples

      iex> String.contains? "elixir of life", "of"
      true
      iex> String.contains? "elixir of life", ["life", "death"]
      true
      iex> String.contains? "elixir of life", ["death", "mercury"]
      false

  An empty string will always match:

      iex> String.contains? "elixir of life", ""
      true
      iex> String.contains? "elixir of life", ["", "other"]
      true

  The argument can also be a precompiled pattern:

      iex> pattern = :binary.compile_pattern(["life", "death"])
      iex> String.contains? "elixir of life", pattern
      true

  """
  @spec contains?(t, pattern) :: boolean
  def contains?(string, []) when is_binary(string) do
    false
  end

  def contains?(string, contents) when is_binary(string) and is_list(contents) do
    "" in contents or :binary.match(string, contents) != :nomatch
  end

  def contains?(string, contents) when is_binary(string) do
    "" == contents or :binary.match(string, contents) != :nomatch
  end

  @doc """
  Converts a string into a charlist.

  Specifically, this functions takes a UTF-8 encoded binary and returns a list of its integer
  codepoints. It is similar to `codepoints/1` except that the latter returns a list of codepoints as
  strings.

  In case you need to work with bytes, take a look at the
  [`:binary` module](http://www.erlang.org/doc/man/binary.html).

  ## Examples

      iex> String.to_charlist("æß")
      'æß'
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
  Converts a string to an atom.

  Currently Elixir does not support the conversion of strings
  that contain Unicode codepoints greater than 0xFF.

  Inlined by the compiler.

  ## Examples

      iex> String.to_atom("my_atom")
      :my_atom

  """
  @spec to_atom(String.t) :: atom
  def to_atom(string) do
    :erlang.binary_to_atom(string, :utf8)
  end

  @doc """
  Converts a string to an existing atom.

  Currently Elixir does not support the conversion of strings
  that contain Unicode codepoints greater than 0xFF.

  Inlined by the compiler.

  ## Examples

      iex> _ = :my_atom
      iex> String.to_existing_atom("my_atom")
      :my_atom

      iex> String.to_existing_atom("this_atom_will_never_exist")
      ** (ArgumentError) argument error

  """
  @spec to_existing_atom(String.t) :: atom
  def to_existing_atom(string) do
    :erlang.binary_to_existing_atom(string, :utf8)
  end

  @doc """
  Returns an integer whose text representation is `string`.

  Inlined by the compiler.

  ## Examples

      iex> String.to_integer("123")
      123

  """
  @spec to_integer(String.t) :: integer
  def to_integer(string) do
    :erlang.binary_to_integer(string)
  end

  @doc """
  Returns an integer whose text representation is `string` in base `base`.

  Inlined by the compiler.

  ## Examples

      iex> String.to_integer("3FF", 16)
      1023

  """
  @spec to_integer(String.t, 2..36) :: integer
  def to_integer(string, base) do
    :erlang.binary_to_integer(string, base)
  end

  @doc """
  Returns a float whose text representation is `string`.

  `string` must be the string representation of a float.
  If a string representation of an integer wants to be used,
  then `Float.parse/1` should be used instead,
  otherwise an argument error will be raised.

  Inlined by the compiler.

  ## Examples

      iex> String.to_float("2.2017764e+0")
      2.2017764

      iex> String.to_float("3.0")
      3.0

  """
  @spec to_float(String.t) :: float
  def to_float(string) do
    :erlang.binary_to_float(string)
  end

  @doc """
  Returns a float value between 0 (equates to no similarity) and 1 (is an exact match)
  representing [Jaro](https://en.wikipedia.org/wiki/Jaro–Winkler_distance)
  distance between `string1` and `string2`.

  The Jaro distance metric is designed and best suited for short strings such as person names.

  ## Examples

      iex> String.jaro_distance("dwayne", "duane")
      0.8222222222222223
      iex> String.jaro_distance("even", "odd")
      0.0

  """
  @spec jaro_distance(t, t) :: float
  def jaro_distance(string1, string2)

  def jaro_distance(string, string), do: 1.0
  def jaro_distance(_string, ""), do: 0.0
  def jaro_distance("", _string), do: 0.0

  def jaro_distance(string1, string2) do
    {chars1, len1} = chars_and_length(string1)
    {chars2, len2} = chars_and_length(string2)

    case match(chars1, len1, chars2, len2) do
      {0, _trans} -> 0.0
      {comm, trans} ->
        ((comm / len1) +
         (comm / len2) +
         ((comm - trans) / comm)) / 3
    end
  end

  @compile {:inline, chars_and_length: 1}
  defp chars_and_length(string) do
    chars = graphemes(string)
    {chars, Kernel.length(chars)}
  end

  defp match(chars1, len1, chars2, len2) do
    if len1 < len2 do
      match(chars1, chars2, div(len2, 2) - 1)
    else
      match(chars2, chars1, div(len1, 2) - 1)
    end
  end

  defp match(chars1, chars2, lim) do
    match(chars1, chars2, {0, lim}, {0, 0, -1}, 0)
  end

  defp match([char | rest], chars, range, state, idx) do
    {chars, state} = submatch(char, chars, range, state, idx)

    case range do
      {lim, lim} -> match(rest, tl(chars), range, state, idx + 1)
      {pre, lim} -> match(rest, chars, {pre + 1, lim}, state, idx + 1)
    end
  end

  defp match([], _, _, {comm, trans, _}, _), do: {comm, trans}

  defp submatch(char, chars, {pre, _} = range, state, idx) do
    case detect(char, chars, range) do
      nil -> {chars, state}
      {subidx, chars} ->
        {chars, proceed(state, idx - pre + subidx)}
    end
  end

  defp detect(char, chars, {pre, lim}) do
    detect(char, chars, pre + 1 + lim, 0, [])
  end

  defp detect(_char, _chars, 0, _idx, _acc), do: nil
  defp detect(_char, [], _lim, _idx, _acc),  do: nil

  defp detect(char, [char | rest], _lim, idx, acc),
    do: {idx, Enum.reverse(acc, [nil | rest])}

  defp detect(char, [other | rest], lim, idx, acc),
    do: detect(char, rest, lim - 1, idx + 1, [other | acc])

  defp proceed({comm, trans, former}, current) do
    if current < former do
      {comm + 1, trans + 1, current}
    else
      {comm + 1, trans, current}
    end
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
  @spec myers_difference(t, t) :: [{:eq | :ins | :del, t}] | nil
  def myers_difference(string1, string2) do
    List.myers_difference(graphemes(string1), graphemes(string2))
    |> Enum.map(fn {kind, chars} ->
      {kind, IO.iodata_to_binary(chars)}
    end)
  end

  # TODO: Deprecate by v1.5
  @doc false
  @spec to_char_list(t) :: charlist
  def to_char_list(string), do: String.to_charlist(string)
end
