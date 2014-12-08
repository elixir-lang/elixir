import Kernel, except: [length: 1]

defmodule String do
  @moduledoc ~S"""
  A String in Elixir is a UTF-8 encoded binary.

  ## Codepoints and graphemes

  The functions in this module act according to the Unicode
  Standard, version 6.3.0. As per the standard, a codepoint is
  an Unicode Character, which may be represented by one or more
  bytes. For example, the character "é" is represented with two
  bytes:

      iex> byte_size("é")
      2

  However, this module returns the proper length:

      iex> String.length("é")
      1

  Furthermore, this module also presents the concept of
  graphemes, which are multiple characters that may be
  "perceived as a single character" by readers. For example,
  the same "é" character written above could be represented
  by the letter "e" followed by the accent ́:

      iex> string = "\x{0065}\x{0301}"
      iex> byte_size(string)
      3
      iex> String.length(string)
      1

  Although the example above is made of two characters, it is
  perceived by users as one.

  Graphemes can also be two characters that are interpreted
  as one by some languages. For example, some languages may
  consider "ch" as a grapheme. However, since this information
  depends on the locale, it is not taken into account by this
  module.

  In general, the functions in this module rely on the Unicode
  Standard, but do not contain any of the locale specific behaviour.

  More information about graphemes can be found in the [Unicode
  Standard Annex #29](http://www.unicode.org/reports/tr29/).
  This current Elixir version implements Extended Grapheme Cluster
  algorithm.

  ## String and binary operations

  To act accordingly to the Unicode Standard, many functions
  in this module runs in linear time, as it needs to traverse
  the whole string considering the proper Unicode codepoints.

  For example, `String.length/1` is going to take longer as
  the input grows. On the other hand, `Kernel.byte_size/1` always runs
  in constant time (i.e. regardless of the input size).

  This means often there are performance costs in using the
  functions in this module, compared to the more low-level
  operations that work directly with binaries:

    * `Kernel.binary_part/3` - retrieves part of the binary
    * `Kernel.bit_size/1` and `Kernel.byte_size/1` - size related functions
    * `Kernel.is_bitstring/1` and `Kernel.is_binary/1` - type checking function
    * Plus a number of functions for working with binaries (bytes)
      [in the `:binary` module](http://erlang.org/doc/man/binary.html)

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

  A first attempting at improving it could be with ranges:

      iex> take_prefix = fn full, prefix ->
      ...>   base = String.length(prefix)
      ...>   String.slice(full, base..-1)
      ...> end
      iex> take_prefix.("Mr. John", "Mr. ")
      "John"

  While this is much better (we don't traverse `full` twice),
  it could still be improved. In this case, since we want to
  extract a substring from a string, we can use `byte_size/1`
  and `binary_part/3` as there is no chance we will slice in
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
      ...>   <<_ :: binary-size(base), rest :: binary>> = full
      ...>   rest
      ...> end
      iex> take_prefix.("Mr. John", "Mr. ")
      "John"

  On the other hand, if you want to dynamically slice a string
  based on an integer value, then using `String.slice/3` is the
  best option as it guarantees we won't incorrectly split a valid
  codepoint in multiple bytes.

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

      iex> << eacute :: utf8 >> = "á"
      iex> eacute
      225

  As we have seen above, codepoints can be inserted into
  a string by their hexadecimal code:

      "ol\x{0061}\x{0301}" #=>
      "olá"

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
      when h in 0x20..0x7E
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
  def printable?(<<?\d, t :: binary>>), do: printable?(t)
  def printable?(<<?\a, t :: binary>>), do: printable?(t)

  def printable?(<<>>), do: true
  def printable?(b) when is_binary(b), do: false

  @doc """
  Divides a string into substrings at each Unicode whitespace
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

  @doc ~S"""
  Divides a string into substrings based on a pattern.

  Returns a list of these substrings. The pattern can
  be a string, a list of strings or a regular expression.

  The string is split into as many parts as possible by
  default, but can be controlled via the `parts: num` option.
  If you pass `parts: :infinity`, it will return all possible parts.

  Empty strings are only removed from the result if the
  `trim` option is set to `true`.

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

  Splitting on empty patterns returns codepoints:

      iex> String.split("abc", ~r{})
      ["a", "b", "c", ""]

      iex> String.split("abc", "")
      ["a", "b", "c", ""]

      iex> String.split("abc", "", trim: true)
      ["a", "b", "c"]

      iex> String.split("abc", "", parts: 2)
      ["a", "bc"]

  """
  @spec split(t, t | [t] | Regex.t) :: [t]
  @spec split(t, t | [t] | Regex.t, Keyword.t) :: [t]
  def split(string, pattern, options \\ [])

  def split(string, "", options) do
    parts = Keyword.get(options, :parts, :infinity)
    split_codepoints(string, parts_to_index(parts), Keyword.get(options, :trim, false))
  end

  def split(string, pattern, options) do
    if Regex.regex?(pattern) do
      Regex.split(pattern, string, options)
    else
      parts = Keyword.get(options, :parts, :infinity)
      trim  = Keyword.get(options, :trim, false)
      if parts == :infinity and trim == false do
        :binary.split(string, pattern, [:global])
      else
        split_parts(string, pattern, parts_to_index(parts), trim)
      end
    end
  end

  defp parts_to_index(:infinity),                      do: 0
  defp parts_to_index(n) when is_integer(n) and n > 0, do: n

  defp split_codepoints(binary, 1, _trim), do: [binary]
  defp split_codepoints(<<h :: utf8, t :: binary>>, count, trim),
    do: [<<h :: utf8>>|split_codepoints(t, count - 1, trim)]
  defp split_codepoints(<<h, t :: binary>>, count, trim),
    do: [<<h>>|split_codepoints(t, count - 1, trim)]
  defp split_codepoints(<<>>, _, true), do: []
  defp split_codepoints(<<>>, _, false), do: [""]

  defp split_parts("", _pattern, _num, true),   do: []
  defp split_parts("", _pattern, _num, _trim),  do: [""]
  defp split_parts(string, _pattern, 1, _trim), do: [string]
  defp split_parts(string, pattern, num, trim) do
    case :binary.split(string, pattern) do
      [""] when trim ->
        []
      [head] ->
        [head]
      [head, tail] ->
        if trim and head == "" do
          split_parts(tail, pattern, num, trim)
        else
          [head|split_parts(tail, pattern, num-1, trim)]
        end
    end
  end

  @doc """
  Splits a string into two at the specified offset. When the offset given is
  negative, location is counted from the end of the string.

  The offset is capped to the length of the string.

  Returns a tuple with two elements.

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
  def split_at(string, offset)

  def split_at(binary, 0), do: {"", binary}

  def split_at(binary, index) when is_integer(index) and index > 0, do:
    do_split_at(next_grapheme(binary), 0, index, "")

  def split_at(binary, index) when is_integer(index) and index < 0, do:
    do_split_at(next_grapheme(binary), 0, max(0, byte_size(binary)+index), "")

  defp do_split_at(nil, _, _, acc), do:
    {acc, ""}

  defp do_split_at({grapheme, rest}, current_pos, target_pos, acc) when current_pos < target_pos, do:
    do_split_at(next_grapheme(rest), current_pos+1, target_pos, acc <> grapheme)

  defp do_split_at({grapheme, rest}, pos, pos, acc), do:
    {acc, grapheme <> rest}

  @doc """
  Convert all characters on the given string to uppercase.

  ## Examples

      iex> String.upcase("abcd")
      "ABCD"

      iex> String.upcase("ab 123 xpto")
      "AB 123 XPTO"

      iex> String.upcase("olá")
      "OLÁ"

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

      iex> String.downcase("OLÁ")
      "olá"

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

      iex> String.capitalize("olá")
      "Olá"

  """
  @spec capitalize(t) :: t
  def capitalize(string) when is_binary(string) do
    {char, rest} = String.Unicode.titlecase_once(string)
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
  def rstrip(string, char) when char in 0..127 do
    if :binary.last(string) == char do
      rstrip(binary_part(string, 0, byte_size(string) - 1), char)
    else
      string
    end
  end

  def rstrip(string, char) when is_integer(char) do
    do_rstrip(string, "", char)
  end

  defp do_rstrip(<<char :: utf8, string :: binary>>, buffer, char) do
    <<do_rstrip(string, <<char :: utf8, buffer :: binary>>, char) :: binary>>
  end

  defp do_rstrip(<<char :: utf8, string :: binary>>, buffer, another_char) do
    <<buffer :: binary, char :: utf8, do_rstrip(string, "", another_char) :: binary>>
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

  def lstrip(<<char :: utf8, rest :: binary>>, char) when is_integer(char) do
    <<lstrip(rest, char) :: binary>>
  end

  def lstrip(other, char) when is_integer(char) do
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

  @doc ~S"""
  Returns a new string of length `len` with `subject` right justified and
  padded with `padding`. If `padding` is not present, it defaults to
  whitespace. When `len` is less than the length of `subject`, `subject` is
  returned.

  ## Examples

      iex> String.rjust("abc", 5)
      "  abc"

      iex> String.rjust("abc", 5, ?-)
      "--abc"

  """
  @spec rjust(t, non_neg_integer) :: t
  @spec rjust(t, non_neg_integer, char) :: t

  def rjust(subject, len, pad \\ ?\s) when is_integer(pad) and is_integer(len) and len >= 0 do
    justify(subject, len, pad, :right)
  end

  @doc ~S"""
  Returns a new string of length `len` with `subject` left justified and padded
  with `padding`. If `padding` is not present, it defaults to whitespace. When
  `len` is less than the length of `subject`, `subject` is returned.

  ## Examples

      iex> String.ljust("abc", 5)
      "abc  "

      iex> String.ljust("abc", 5, ?-)
      "abc--"

  """
  @spec ljust(t, non_neg_integer) :: t
  @spec ljust(t, non_neg_integer, char) :: t

  def ljust(subject, len, pad \\ ?\s) when is_integer(pad) and is_integer(len) and len >= 0 do
    justify(subject, len, pad, :left)
  end

  defp justify(subject, 0, _pad, _type), do: subject
  defp justify(subject, len, padding, type) do
    subject_len = length(subject)

    cond do
      subject_len >= len ->
        subject
      subject_len < len ->
        fill = duplicate(<<padding :: utf8>>, len - subject_len)

        case type do
          :left  -> subject <> fill
          :right -> fill <> subject
        end
    end
  end

  @doc ~S"""
  Returns a new binary based on `subject` by replacing the parts
  matching `pattern` by `replacement`. By default, it replaces
  all entries, except if the `global` option is set to `false`.

  A `pattern` may be a string or a regex.

  ## Examples

      iex> String.replace("a,b,c", ",", "-")
      "a-b-c"

      iex> String.replace("a,b,c", ",", "-", global: false)
      "a-b,c"

  The pattern can also be a regex. In those cases, one can give `\N` or
  `\g{N}` in the `replacement` string to access a specific capture in the
  regex:

      iex> String.replace("a,b,c", ~r/,(.)/, ",\\1\\1")
      "a,bb,cc"

  Notice we had to escape the escape character `\`. By giving `\0`,
  one can inject the whole matched pattern in the replacement string.

  When strings are used as a pattern, a developer can also use the
  replaced part inside the `replacement` via the `:insert_replaced` option:

      iex> String.replace("a,b,c", "b", "[]", insert_replaced: 1)
      "a,[b],c"

      iex> String.replace("a,b,c", ",", "[]", insert_replaced: 2)
      "a[],b[],c"

      iex> String.replace("a,b,c", ",", "[]", insert_replaced: [1, 1])
      "a[,,]b[,,]c"

  """
  @spec replace(t, t | Regex.t, t) :: t
  @spec replace(t, t | Regex.t, t, Keyword.t) :: t

  def replace(subject, pattern, replacement, options \\ []) when is_binary(replacement) do
    if Regex.regex?(pattern) do
      Regex.replace(pattern, subject, replacement, global: options[:global])
    else
      opts = translate_replace_options(options)
      :binary.replace(subject, pattern, replacement, opts)
    end
  end

  defp translate_replace_options(options) do
    opts = if Keyword.get(options, :global) != false, do: [:global], else: []

    if insert = Keyword.get(options, :insert_replaced) do
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
    do_reverse(next_grapheme(string), [])
  end

  defp do_reverse({grapheme, rest}, acc) do
    do_reverse(next_grapheme(rest), [grapheme|acc])
  end

  defp do_reverse(nil, acc), do: IO.iodata_to_binary(acc)

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
  @spec duplicate(t, non_neg_integer) :: t
  def duplicate(subject, n) when is_integer(n) and n >= 0 do
    :binary.copy(subject, n)
  end

  @doc """
  Returns all codepoints in the string.

  ## Examples

      iex> String.codepoints("olá")
      ["o", "l", "á"]

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
  remaining of the string or `nil` in case
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

  for noncharacter <- noncharacters do
    def valid?(<< unquote(noncharacter) :: utf8, _ :: binary >>), do: false
  end

  def valid?(<<_ :: utf8, t :: binary>>), do: valid?(t)
  def valid?(<<>>), do: true
  def valid?(_), do: false

  @doc ~S"""
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

  @doc ~S"""
  Splits the string into chunks of characters that share a common trait.

  The trait can be one of two options:

    * `:valid` – the string is split into chunks of valid and invalid character
      sequences

    * `:printable` – the string is split into chunks of printable and
      non-printable character sequences

  Returns a list of binaries each of which contains only one kind of
  characters.

  If the given string is empty, an empty list is returned.

  ## Examples

      iex> String.chunk(<<?a, ?b, ?c, 0>>, :valid)
      ["abc\0"]

      iex> String.chunk(<<?a, ?b, ?c, 0, 0x0ffff::utf8>>, :valid)
      ["abc\0", <<0x0ffff::utf8>>]

      iex> String.chunk(<<?a, ?b, ?c, 0, 0x0ffff::utf8>>, :printable)
      ["abc", <<0, 0x0ffff::utf8>>]

  """
  @spec chunk(t, :valid | :printable) :: [t]

  def chunk(string, trait)

  def chunk("", _), do: []

  def chunk(str, trait) when trait in [:valid, :printable] do
    {cp, _} = next_codepoint(str)
    pred_fn = make_chunk_pred(trait)
    do_chunk(str, pred_fn.(cp), pred_fn)
  end


  defp do_chunk(str, flag, pred_fn), do: do_chunk(str, [], <<>>, flag, pred_fn)

  defp do_chunk(<<>>, acc, <<>>, _, _), do: Enum.reverse(acc)

  defp do_chunk(<<>>, acc, chunk, _, _), do: Enum.reverse(acc, [chunk])

  defp do_chunk(str, acc, chunk, flag, pred_fn) do
    {cp, rest} = next_codepoint(str)
    if pred_fn.(cp) != flag do
      do_chunk(rest, [chunk|acc], cp, not flag, pred_fn)
    else
      do_chunk(rest, acc, chunk <> cp, flag, pred_fn)
    end
  end

  defp make_chunk_pred(:valid), do: &valid?/1
  defp make_chunk_pred(:printable), do: &printable?/1

  @doc """
  Returns unicode graphemes in the string as per Extended Grapheme
  Cluster algorithm outlined in the [Unicode Standard Annex #29,
  Unicode Text Segmentation](http://www.unicode.org/reports/tr29/).

  ## Examples

      iex> String.graphemes("Ńaïve")
      ["Ń", "a", "ï", "v", "e"]

  """
  @spec graphemes(t) :: [grapheme]
  defdelegate graphemes(string), to: String.Graphemes

  @doc """
  Returns the next grapheme in a String.

  The result is a tuple with the grapheme and the
  remaining of the string or `nil` in case
  the String reached its end.

  ## Examples

      iex> String.next_grapheme("olá")
      {"o", "lá"}

  """
  @compile {:inline, next_grapheme: 1}
  @spec next_grapheme(t) :: {grapheme, t} | nil
  defdelegate next_grapheme(string), to: String.Graphemes

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
      {char, _} -> char
      nil -> nil
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

  defp do_last(nil, last_char), do: last_char

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

  defp do_length(nil), do: 0

  @doc """
  Returns the grapheme in the `position` of the given utf8 `string`.
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
    do_at(next_grapheme(string), position, 0)
  end

  def at(string, position) when is_integer(position) and position < 0 do
    real_pos = length(string) - abs(position)
    case real_pos >= 0 do
      true  -> do_at(next_grapheme(string), real_pos, 0)
      false -> nil
    end
  end

  defp do_at({_ , rest}, desired_pos, current_pos) when desired_pos > current_pos do
    do_at(next_grapheme(rest), desired_pos, current_pos + 1)
  end

  defp do_at({char, _}, desired_pos, desired_pos) do
    char
  end

  defp do_at(nil, _, _), do: nil

  @doc """
  Returns a substring starting at the offset given by the first, and
  a length given by the second.

  If the offset is greater than string length, then it returns `""`.

  Remember this function works with unicode codepoints and consider
  the slices to represent codepoints offsets. If you want to split
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
    case do_count_bytes(next_grapheme(string), start, 0) do
      {nil, _} -> ""
      {next, start_bytes} ->
        {_, len_bytes} = do_count_bytes(next, len, 0)
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

  defp do_count_bytes(next, 0, acc), do: {next, acc}
  defp do_count_bytes(nil, _, acc),  do: {nil, acc}

  defp do_count_bytes({char, rest}, counter, acc) do
    do_count_bytes(next_grapheme(rest), counter - 1, acc + byte_size(char))
  end

  @doc """
  Returns a substring from the offset given by the start of the
  range to the offset given by the end of the range.

  If the start of the range is not a valid offset for the given
  string or if the range is in reverse order, returns `""`.

  If the start or end of the range are negative, the whole string
  is traversed first in order to convert the negative indexes into
  positive ones.

  Remember this function works with unicode codepoints and consider
  the slices to represent codepoints offsets. If you want to split
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
    case do_count_bytes(next_grapheme(string), first, 0) do
      {nil, _} -> ""
      {_, start_bytes} -> binary_part(string, start_bytes, byte_size(string) - start_bytes)
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
    {bytes, length} = do_acc_bytes(next_grapheme(string), [], 0)

    if first < 0, do: first = length + first
    if last < 0,  do: last  = length + last

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

  defp do_acc_bytes({char, rest}, bytes, length) do
    do_acc_bytes(next_grapheme(rest), [byte_size(char)|bytes], length + 1)
  end

  defp do_acc_bytes(nil, bytes, length) do
    {bytes, length}
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
    Enum.any?(prefixes, &do_starts_with(string, &1))
  end

  def starts_with?(string, prefix) do
    do_starts_with(string, prefix)
  end

  defp do_starts_with(string, "") when is_binary(string) do
    true
  end

  defp do_starts_with(string, prefix) when is_binary(prefix) do
    Kernel.match?({0, _}, :binary.match(string, prefix))
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
    Enum.any?(suffixes, &do_ends_with(string, &1))
  end

  def ends_with?(string, suffix) do
    do_ends_with(string, suffix)
  end

  defp do_ends_with(string, "") when is_binary(string) do
    true
  end

  defp do_ends_with(string, suffix) when is_binary(suffix) do
    string_size = byte_size(string)
    suffix_size = byte_size(suffix)
    scope = {string_size - suffix_size, suffix_size}
    (suffix_size <= string_size) and (:nomatch != :binary.match(string, suffix, [scope: scope]))
  end

  @doc """
  Check if `string` matches the given regular expression.

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
  Check if `string` contains any of the given `contents`.

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

  def contains?(string, contents) when is_list(contents) do
    Enum.any?(contents, &do_contains(string, &1))
  end

  def contains?(string, content) do
    do_contains(string, content)
  end

  defp do_contains(string, "") when is_binary(string) do
    true
  end

  defp do_contains(string, match) when is_binary(match) do
    :nomatch != :binary.match(string, match)
  end

  @doc """
  Converts a string into a char list.

  ## Examples

      iex> String.to_char_list("æß")
      'æß'

  Notice that this function expects a list of integers representing
  UTF-8 codepoints. If you have a raw binary, you must instead use
  [the `:binary` module](http://erlang.org/doc/man/binary.html).
  """
  @spec to_char_list(t) :: char_list
  def to_char_list(string) when is_binary(string) do
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

  Currently Elixir does not support conversions from strings
  which contains Unicode codepoints greater than 0xFF.

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

  Currently Elixir does not support conversions from strings
  which contains Unicode codepoints greater than 0xFF.

  Inlined by the compiler.

  ## Examples

      iex> :my_atom
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
  Returns a integer whose text representation is `string`.

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
end
