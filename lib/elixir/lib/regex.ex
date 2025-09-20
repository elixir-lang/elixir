# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Regex do
  @moduledoc ~S"""
  Provides regular expressions for Elixir.

  Regex is based on PCRE (Perl Compatible Regular Expressions) and
  built on top of Erlang's `:re` module. More information can be found
  in the [`:re` module documentation](`:re`).

  Regular expressions in Elixir can be created using the sigils
  `~r` (see `sigil_r/2`):

      # A simple regular expression that matches foo anywhere in the string
      ~r/foo/

      # A regular expression with case insensitive and Unicode options
      ~r/foo/iu

  A Regex is represented internally as the `Regex` struct. Therefore,
  `%Regex{}` can be used whenever there is a need to match on them.
  Keep in mind that all of the structs fields are private. And since
  regexes are compiled, there is no guarantee two regular expressions
  from the same source are equal, for example:

      ~r/(?<foo>.)(?<bar>.)/ == ~r/(?<foo>.)(?<bar>.)/

  may return `true` or `false` depending on your machine, endianness,
  available optimizations and others. You can, however, retrieve the source
  of a compiled regular expression by accessing the `source` field, and then
  compare those directly:

      ~r/(?<foo>.)(?<bar>.)/.source == ~r/(?<foo>.)(?<bar>.)/.source

  ## Escapes

  Escape sequences are split into two categories.

  ### Non-printing characters

    * `\a` - Alarm, that is, the BEL character (hex 07)
    * `\e` - Escape (hex 1B)
    * `\f` - Form feed (hex 0C)
    * `\n` - Line feed (hex 0A)
    * `\r` - Carriage return (hex 0D)
    * `\t` - Tab (hex 09)
    * `\xhh` - Character with hex code hh
    * `\x{hhh..}` - Character with hex code hhh..

  `\u` and `\U` are not supported. Other escape sequences, such as `\ddd`
  for octals, are supported but discouraged.

  ### Generic character types

    * `\d` - Any decimal digit
    * `\D` - Any character that is not a decimal digit
    * `\h` - Any horizontal whitespace character
    * `\H` - Any character that is not a horizontal whitespace character
    * `\s` - Any whitespace character
    * `\S` - Any character that is not a whitespace character
    * `\v` - Any vertical whitespace character
    * `\V` - Any character that is not a vertical whitespace character
    * `\w` - Any "word" character
    * `\W` - Any "non-word" character

  ## Modifiers

  The modifiers available when creating a Regex are:

    * `:unicode` (u) - enables Unicode specific patterns like `\p` and causes
      character classes like `\w`, `\W`, `\s`, and the like to also match on Unicode
      (see examples below in "Character classes"). It expects valid Unicode
      strings to be given on match

    * `:caseless` (i) - adds case insensitivity

    * `:dotall` (s) - causes dot to match newlines and also set newline to
      anycrlf; the new line setting can be overridden by setting `(*CR)` or
      `(*LF)` or `(*CRLF)` or `(*ANY)` according to `:re` documentation

    * `:multiline` (m) - causes `^` and `$` to mark the beginning and end of
      each line; use `\A` and `\z` to match the end or beginning of the string

    * `:extended` (x) - whitespace characters are ignored except when escaped
      or within `[..]`, and allow `#` to delimit comments

    * `:firstline` (f) - forces the unanchored pattern to match before or at the
      first newline, though the matched text may continue over the newline

    * `:ungreedy` (U) - inverts the "greediness" of the regexp
      (the previous `r` option is deprecated in favor of `U`)

  ## Captures

  Many functions in this module handle what to capture in a regex
  match via the `:capture` option. The supported values are:

    * `:all` - all captured subpatterns including the complete matching string
      (this is the default)

    * `:first` - only the first captured subpattern, which is always the
      complete matching part of the string; all explicitly captured subpatterns
      are discarded

    * `:all_but_first` - all but the first matching subpattern, i.e. all
      explicitly captured subpatterns, but not the complete matching part of
      the string

    * `:none` - does not return matching subpatterns at all

    * `:all_names` - captures all named subpattern matches in the Regex as a list
      ordered **alphabetically** by the names of the subpatterns

    * `list(binary | atom)` - a list of named captures to capture

  ## Character classes

  Regex supports several built in named character classes. These are used by
  enclosing the class name in `[: :]` inside a group. For example:

      iex> String.match?("123", ~r/^[[:alnum:]]+$/)
      true
      iex> String.match?("123 456", ~r/^[[:alnum:][:blank:]]+$/)
      true

  The supported class names are:

    * alnum - Letters and digits
    * alpha - Letters
    * blank - Space or tab only
    * cntrl - Control characters
    * digit - Decimal digits (same as \\d)
    * graph - Printing characters, excluding space
    * lower - Lowercase letters
    * print - Printing characters, including space
    * punct - Printing characters, excluding letters, digits, and space
    * space - Whitespace (the same as \s from PCRE 8.34)
    * upper - Uppercase letters
    * word  - "Word" characters (same as \w)
    * xdigit - Hexadecimal digits

  There is another character class, `ascii`, that erroneously matches
  Latin-1 characters instead of the 0-127 range specified by POSIX. This
  cannot be fixed without altering the behavior of other classes, so we
  recommend matching the range with `[\\0-\x7f]` instead.

  Note the behavior of those classes may change according to the Unicode
  and other modifiers:

      iex> String.match?("josé", ~r/^[[:lower:]]+$/)
      false
      iex> String.match?("josé", ~r/^[[:lower:]]+$/u)
      true
      iex> Regex.replace(~r/\s/, "Unicode\u00A0spaces", "-")
      "Unicode spaces"
      iex> Regex.replace(~r/\s/u, "Unicode\u00A0spaces", "-")
      "Unicode-spaces"

  """

  defstruct re_pattern: nil, source: "", opts: []

  @type t :: %__MODULE__{re_pattern: term, source: binary, opts: [term]}

  @type named_captures_opts :: [
          return: :binary | :index,
          offset: non_neg_integer()
        ]

  defmodule CompileError do
    @moduledoc """
    An exception raised when a regular expression could not be compiled.
    """

    defexception message: "regex could not be compiled"
  end

  @doc """
  Compiles the regular expression.

  The given options can either be a binary with the characters
  representing the same regex options given to the
  `~r` (see `sigil_r/2`) sigil, or a list of options, as
  expected by the Erlang's [`:re`](`:re`) module.

  It returns `{:ok, regex}` in case of success,
  `{:error, reason}` otherwise.

  ## Examples

      Regex.compile("foo")
      #=> {:ok, ~r/foo/}

      Regex.compile("foo", "i")
      #=> {:ok, ~r/foo/i}

      Regex.compile("*foo")
      #=> {:error, {~c"quantifier does not follow a repeatable item", 0}}

  """
  @spec compile(binary, binary | [term]) :: {:ok, t} | {:error, term}
  def compile(source, opts \\ "") when is_binary(source) do
    do_compile(source, opts)
  end

  defp do_compile(source, opts) when is_binary(opts) do
    case translate_options(opts, []) do
      {:error, rest} ->
        {:error, {:invalid_option, rest}}

      translated_opts ->
        do_compile(source, translated_opts)
    end
  end

  defp do_compile(source, opts) when is_list(opts) do
    case :re.compile(source, opts) do
      {:ok, re_pattern} ->
        {:ok, %Regex{re_pattern: re_pattern, source: source, opts: opts}}

      error ->
        error
    end
  end

  @doc """
  Compiles the regular expression and raises `Regex.CompileError` in case of errors.
  """
  @spec compile!(binary, binary | [term]) :: t
  def compile!(source, options \\ "") when is_binary(source) do
    case compile(source, options) do
      {:ok, regex} -> regex
      {:error, {reason, at}} -> raise Regex.CompileError, "#{reason} at position #{at}"
    end
  end

  @doc """
  Recompiles the existing regular expression if necessary.

  This checks the version stored in the regular expression
  and recompiles the regex in case of version mismatch.
  """
  # Remove me on Elixir v1.22
  @doc deprecated: "It can be removed and it has no effect"
  @doc since: "1.4.0"
  def recompile(%Regex{} = regex) do
    {:ok, regex}
  end

  @doc """
  Recompiles the existing regular expression and raises `Regex.CompileError` in case of errors.
  """
  # Remove me on Elixir v1.22
  @doc deprecated: "It can be removed and it has no effect"
  @doc since: "1.4.0"
  def recompile!(regex) do
    regex
  end

  @doc """
  Returns the version of the underlying Regex engine.
  """
  # Remove me on Elixir v1.22
  @doc deprecated: "Use :re.version() instead"
  @doc since: "1.4.0"
  def version do
    {:re.version(), :erlang.system_info(:endian)}
  end

  @doc """
  Returns a boolean indicating whether there was a match or not.

  ## Examples

      iex> Regex.match?(~r/foo/, "foo")
      true

      iex> Regex.match?(~r/foo/, "bar")
      false

  Elixir also provides text-based match operator `=~/2` and function `String.match?/2` as
  an alternative to test strings against regular expressions and
  strings.
  """
  @spec match?(t, String.t()) :: boolean
  def match?(%Regex{} = regex, string) when is_binary(string) do
    safe_run(regex, string, [{:capture, :none}]) == :match
  end

  @doc false
  @deprecated "Use Kernel.is_struct(term, Regex) or pattern match on %Regex{} instead"
  def regex?(term)
  def regex?(%Regex{}), do: true
  def regex?(_), do: false

  @doc """
  Runs the regular expression against the given string until the first match.
  It returns a list with all captures or `nil` if no match occurred.

  ## Options

    * `:return` - when set to `:index`, returns byte index and match length.
      Defaults to `:binary`.
    * `:capture` - what to capture in the result. See the ["Captures" section](#module-captures)
      to see the possible capture values.
    * `:offset` - (since v1.12.0) specifies the starting offset to match in the given string.
      Defaults to `0`.

  ## Examples

      iex> Regex.run(~r/c(d)/, "abcd")
      ["cd", "d"]

      iex> Regex.run(~r/e/, "abcd")
      nil

      iex> Regex.run(~r/c(d)/, "abcd", return: :index)
      [{2, 2}, {3, 1}]

      iex> Regex.run(~r/c(d)/, "abcd", capture: :first)
      ["cd"]

      iex> Regex.run(~r/c(?<foo>d)/, "abcd", capture: ["foo", "bar"])
      ["d", ""]

  """
  @spec run(t, binary, capture_opts) :: nil | [binary] | [{integer, integer}]
  def run(regex, string, options \\ [])

  def run(%Regex{} = regex, string, options) when is_binary(string) do
    return = Keyword.get(options, :return, :binary)
    captures = Keyword.get(options, :capture, :all)
    offset = Keyword.get(options, :offset, 0)

    case safe_run(regex, string, [{:capture, captures, return}, {:offset, offset}]) do
      :nomatch -> nil
      :match -> []
      {:match, results} -> results
    end
  end

  @doc """
  Returns the given captures as a map or `nil` if no captures are found.

  ## Options

    * `:return` - when set to `:index`, returns byte index and match length.
      Defaults to `:binary`.
    * `:offset` - (since v1.12.0) specifies the starting offset to match in the given string.
      Defaults to `0`.

  ## Examples

      iex> Regex.named_captures(~r/c(?<foo>d)/, "abcd")
      %{"foo" => "d"}

      iex> Regex.named_captures(~r/a(?<foo>b)c(?<bar>d)/, "abcd")
      %{"bar" => "d", "foo" => "b"}

      iex> Regex.named_captures(~r/a(?<foo>b)c(?<bar>d)/, "efgh")
      nil

  You can also retrieve indexes from the named captures. This is particularly
  useful if you want to know if a named capture matched or not:

      iex> Regex.named_captures(~r/a(?<foo>b)c(?<bar>d)?/, "abc", return: :index)
      %{"bar" => {-1, 0}, "foo" => {1, 1}}

  You can then use `binary_part/3` to fetch the relevant part from the given string.
  """
  @spec named_captures(t, String.t(), named_captures_opts) :: map | nil
  def named_captures(regex, string, options \\ []) when is_binary(string) do
    names = names(regex)
    options = Keyword.put(options, :capture, names)
    results = run(regex, string, options)
    if results, do: Enum.zip(names, results) |> Enum.into(%{})
  end

  @doc """
  Returns the underlying `re_pattern` in the regular expression.
  """
  @spec re_pattern(t) :: term
  def re_pattern(%Regex{re_pattern: compiled}) do
    compiled
  end

  @doc """
  Returns the regex source as a binary.

  ## Examples

      iex> Regex.source(~r/foo/)
      "foo"

  """
  @spec source(t) :: String.t()
  def source(%Regex{source: source}) do
    source
  end

  @doc """
  Returns the regex options.

  See the documentation of `Regex.compile/2` for more information.

  ## Examples

      iex> Regex.opts(~r/foo/m)
      [:multiline]

      iex> Regex.opts(Regex.compile!("foo", [:caseless]))
      [:caseless]

  """
  @spec opts(t) :: [term]
  def opts(%Regex{opts: opts}) do
    opts
  end

  @doc """
  Returns the pattern as an embeddable string.

  If the pattern was compiled with an option which cannot be represented
  as an embeddable modifier in the current version of PCRE and strict is true
  (the default) then an ArgumentError exception will be raised.

  When the `:strict` option is false the pattern will be returned as though
  any offending options had not be used and the function will not raise any
  exceptions.

  Embeddable modifiers/options are currently:

    * 'i' - `:caseless`
    * 'm' - `:multiline`
    * 's' - `:dotall, {:newline, :anycrlf}`
    * 'x' - `:extended`

  Unembeddable modifiers are:

    * 'f' - `:firstline`
    * 'U' - `:ungreedy`
    * 'u' - `:unicode, :ucp`

  Any other regex compilation option not listed here is considered unembeddable
  and will raise an exception unless the `:strict` option is false.

  ## Examples
      iex> Regex.to_embed(~r/foo/)
      "(?-imsx:foo)"

      iex> Regex.to_embed(~r/^foo/m)
      "(?m-isx:^foo)"

      iex> Regex.to_embed(~r/foo # comment/ix)
      "(?ix-ms:foo # comment\\n)"

      iex> Regex.to_embed(~r/foo/iu)
      ** (ArgumentError) regex compiled with options [:ucp, :unicode] which cannot be represented as an embedded pattern in this version of PCRE

      iex> Regex.to_embed(~r/foo/imsxu, strict: false)
      "(?imsx:foo\\n)"

  """
  @doc since: "1.19.0"
  @spec to_embed(t, strict: boolean()) :: String.t()
  def to_embed(%Regex{source: source, opts: regex_opts}, embed_opts \\ []) do
    strict = Keyword.get(embed_opts, :strict, true)

    modifiers =
      case embeddable_modifiers(regex_opts) do
        {:ok, modifiers} ->
          modifiers

        {:error, modifiers, untranslatable} ->
          if strict do
            raise ArgumentError,
                  "regex compiled with options #{inspect(untranslatable)} which cannot be " <>
                    "represented as an embedded pattern in this version of PCRE"
          else
            modifiers
          end
      end

    disabled = [?i, ?m, ?s, ?x] -- modifiers

    disabled = if disabled != [], do: "-#{disabled}", else: ""

    # Future proof option ordering consistency by sorting
    modifiers = Enum.sort(modifiers)

    nl = if Enum.member?(regex_opts, :extended), do: "\n", else: ""

    "(?#{modifiers}#{disabled}:#{source}#{nl})"
  end

  @doc """
  Returns a list of names in the regex.

  ## Examples

      iex> Regex.names(~r/(?<foo>bar)/)
      ["foo"]

  """
  @spec names(t) :: [String.t()]
  def names(%Regex{re_pattern: re_pattern}) do
    {:namelist, names} = :re.inspect(re_pattern, :namelist)
    names
  end

  @doc ~S"""
  Same as `run/3` but returns all non-overlapping matches of the regular expression.

  A list of lists is returned, where each entry in the primary list represents a
  match and each entry in the secondary list represents the captured contents.

  ## Options

    * `:return` - when set to `:index`, returns byte index and match length.
      Defaults to `:binary`.
    * `:capture` - what to capture in the result. See the ["Captures" section](#module-captures)
      to see the possible capture values.
    * `:offset` - (since v1.12.0) specifies the starting offset to match in the given string.
      Defaults to `0`.

  ## Examples

      iex> Regex.scan(~r/c(d|e)/, "abcd abce")
      [["cd", "d"], ["ce", "e"]]

      iex> Regex.scan(~r/c(?:d|e)/, "abcd abce")
      [["cd"], ["ce"]]

      iex> Regex.scan(~r/e/, "abcd")
      []

      iex> Regex.scan(~r/ab|bc|cd/, "abcd")
      [["ab"], ["cd"]]

      iex> Regex.scan(~r/ab|bc|cd/, "abbccd")
      [["ab"], ["bc"], ["cd"]]

      iex> Regex.scan(~r/\p{Sc}/u, "$, £, and €")
      [["$"], ["£"], ["€"]]

      iex> Regex.scan(~r/=+/, "=ü†ƒ8===", return: :index)
      [[{0, 1}], [{9, 3}]]

      iex> Regex.scan(~r/c(d|e)/, "abcd abce", capture: :first)
      [["cd"], ["ce"]]

  """
  @spec scan(t(), String.t(), capture_opts) :: [[String.t()]] | [[{integer(), integer()}]]
  def scan(regex, string, options \\ [])

  def scan(%Regex{} = regex, string, options) when is_binary(string) do
    return = Keyword.get(options, :return, :binary)
    captures = Keyword.get(options, :capture, :all)
    offset = Keyword.get(options, :offset, 0)
    options = [{:capture, captures, return}, :global, {:offset, offset}]

    case safe_run(regex, string, options) do
      :match -> []
      :nomatch -> []
      {:match, results} -> results
    end
  end

  defp safe_run(%Regex{re_pattern: re_pattern} = regex, string, options) do
    # TODO: Remove me when Erlang/OTP 28+ is required
    # This allows regexes precompiled on Erlang/OTP 27- to work on Erlang/OTP 28+
    with true <- :erlang.system_info(:otp_release) >= [?2, ?8],
         {:re_pattern, _, _, _, <<_::bitstring>>} <- re_pattern do
      %Regex{source: source, opts: compile_opts} = regex
      :re.run(string, source, compile_opts ++ options)
    else
      _ -> :re.run(string, re_pattern, options)
    end
  end

  @typedoc """
  Options for regex functions that capture matches.
  """
  @type capture_opts :: [
          return: :binary | :index,
          capture: :all | :first | :all_but_first | :none | :all_names | [binary() | atom()],
          offset: non_neg_integer()
        ]

  @typedoc """
  Options for `split/3`.
  """
  @type split_opts :: [
          parts: pos_integer() | :infinity,
          trim: boolean(),
          on: :first | :all | :all_but_first | :none | :all_names | [atom() | integer()],
          include_captures: boolean()
        ]

  @doc """
  Splits the given target based on the given pattern and in the given number of
  parts.

  ## Options

    * `:parts` - when specified, splits the string into the given number of
      parts. If not specified, `:parts` defaults to `:infinity`, which will
      split the string into the maximum number of parts possible based on the
      given pattern.

    * `:trim` - when `true`, removes empty strings (`""`) from the result.
      Defaults to `false`.

    * `:on` - specifies which captures to split the string on, and in what
      order. Defaults to `:first` which means captures inside the regex do not
      affect the splitting process. See the ["Captures" section](#module-captures)
      to see the possible capture values.

    * `:include_captures` - when `true`, includes in the result the matches of
      the regular expression. The matches are not counted towards the maximum
      number of parts if combined with the `:parts` option. Defaults to `false`.

  ## Examples

      iex> Regex.split(~r/-/, "a-b-c")
      ["a", "b", "c"]

      iex> Regex.split(~r/-/, "a-b-c", parts: 2)
      ["a", "b-c"]

      iex> Regex.split(~r/-/, "abc")
      ["abc"]

      iex> Regex.split(~r//, "abc")
      ["", "a", "b", "c", ""]

      iex> Regex.split(~r/a(?<second>b)c/, "abc")
      ["", ""]

      iex> Regex.split(~r/a(?<second>b)c/, "abc", on: [:second])
      ["a", "c"]

      iex> Regex.split(~r/(x)/, "Elixir", include_captures: true)
      ["Eli", "x", "ir"]

      iex> Regex.split(~r/a(?<second>b)c/, "abc", on: [:second], include_captures: true)
      ["a", "b", "c"]

      iex> Regex.split(~r/-/, "-a-b--c", trim: true)
      ["a", "b", "c"]

  """
  @spec split(t, String.t(), split_opts) :: [String.t()]
  def split(regex, string, options \\ [])

  def split(%Regex{}, "", opts) do
    if Keyword.get(opts, :trim, false) do
      []
    else
      [""]
    end
  end

  def split(%Regex{} = regex, string, opts)
      when is_binary(string) and is_list(opts) do
    on = Keyword.get(opts, :on, :first)

    case safe_run(regex, string, [:global, capture: on]) do
      {:match, matches} ->
        index = parts_to_index(Keyword.get(opts, :parts, :infinity))
        trim = Keyword.get(opts, :trim, false)
        include_captures = Keyword.get(opts, :include_captures, false)
        do_split(matches, string, 0, index, trim, include_captures)

      :match ->
        [string]

      :nomatch ->
        [string]
    end
  end

  defp parts_to_index(:infinity), do: 0
  defp parts_to_index(n) when is_integer(n) and n > 0, do: n

  defp do_split(_, string, offset, _counter, true, _with_captures)
       when byte_size(string) <= offset do
    []
  end

  defp do_split(_, string, offset, 1, _trim, _with_captures),
    do: [binary_part(string, offset, byte_size(string) - offset)]

  defp do_split([], string, offset, _counter, _trim, _with_captures),
    do: [binary_part(string, offset, byte_size(string) - offset)]

  defp do_split([[{pos, _} | h] | t], string, offset, counter, trim, with_captures)
       when pos - offset < 0 do
    do_split([h | t], string, offset, counter, trim, with_captures)
  end

  defp do_split([[] | t], string, offset, counter, trim, with_captures),
    do: do_split(t, string, offset, counter, trim, with_captures)

  defp do_split([[{pos, length} | h] | t], string, offset, counter, trim, true) do
    new_offset = pos + length
    keep = pos - offset

    <<_::binary-size(^offset), part::binary-size(^keep), match::binary-size(^length), _::binary>> =
      string

    cond do
      keep == 0 and (offset != 0 and length == 0) ->
        do_split([h | t], string, new_offset, counter - 1, trim, true)

      keep == 0 and trim ->
        [match | do_split([h | t], string, new_offset, counter - 1, trim, true)]

      true ->
        [part, match | do_split([h | t], string, new_offset, counter - 1, trim, true)]
    end
  end

  defp do_split([[{pos, length} | h] | t], string, offset, counter, trim, false) do
    new_offset = pos + length
    keep = pos - offset

    if keep == 0 and (trim or (offset != 0 and length == 0)) do
      do_split([h | t], string, new_offset, counter, trim, false)
    else
      <<_::binary-size(^offset), part::binary-size(^keep), _::binary>> = string
      [part | do_split([h | t], string, new_offset, counter - 1, trim, false)]
    end
  end

  @doc ~S"""
  Receives a regex, a binary and a replacement, returns a new
  binary where all matches are replaced by the replacement.

  The replacement can be either a string or a function that returns a string.
  The resulting string is used as a replacement for every match.

  When the replacement is a string, it allows specific captures of the match
  using brackets at the regex expression and accessing them in the replacement
  via `\N` or `\g{N}`, where `N` is the number of the capture. In case `\0` is
  used, the whole match is inserted. Note that in regexes the backslash needs
  to be escaped, hence in practice you'll need to use `\\N` and `\\g{N}`.

  When the replacement is a function, it allows specific captures too.
  The function may have arity N where each argument maps to a capture,
  with the first argument being the whole match. If the function expects more
  arguments than captures found, the remaining arguments will receive `""`.

  ## Options

    * `:global` - when `false`, replaces only the first occurrence
      (defaults to `true`)

  ## Examples

      iex> Regex.replace(~r/d/, "abc", "d")
      "abc"

      iex> Regex.replace(~r/b/, "abc", "d")
      "adc"

      iex> Regex.replace(~r/b/, "abc", "[\\0]")
      "a[b]c"

      iex> Regex.replace(~r/a(b|d)c/, "abcadc", "[\\1]")
      "[b][d]"

      iex> Regex.replace(~r/\.(\d)$/, "500.5", ".\\g{1}0")
      "500.50"

      iex> Regex.replace(~r/a(b|d)c/, "abcadc", fn _, x -> "[#{x}]" end)
      "[b][d]"

      iex> Regex.replace(~r/(\w+)@(\w+).(\w+)/, "abc@def.com", fn _full, _c1, _c2, c3 -> "TLD: #{c3}" end)
      "TLD: com"

      iex> Regex.replace(~r/a/, "abcadc", "A", global: false)
      "Abcadc"

  """
  @spec replace(t, String.t(), String.t() | (... -> String.t()), global: boolean()) ::
          String.t()
  def replace(%Regex{} = regex, string, replacement, options \\ [])
      when is_binary(string) and is_list(options) do
    opts = if Keyword.get(options, :global) != false, do: [:global], else: []
    opts = [{:capture, :all, :index} | opts]

    case safe_run(regex, string, opts) do
      :nomatch ->
        string

      {:match, [mlist | t]} when is_list(mlist) ->
        apply_list(string, precompile_replacement(replacement), [mlist | t])
        |> IO.iodata_to_binary()

      {:match, slist} ->
        apply_list(string, precompile_replacement(replacement), [slist])
        |> IO.iodata_to_binary()
    end
  end

  defp precompile_replacement(replacement) when is_function(replacement) do
    {:arity, arity} = Function.info(replacement, :arity)
    {replacement, arity}
  end

  defp precompile_replacement(""), do: []

  defp precompile_replacement(<<?\\, ?g, ?{, rest::binary>>) when byte_size(rest) > 0 do
    {ns, <<?}, rest::binary>>} = pick_int(rest)
    [List.to_integer(ns) | precompile_replacement(rest)]
  end

  defp precompile_replacement(<<?\\, ?\\, rest::binary>>) do
    [<<?\\>> | precompile_replacement(rest)]
  end

  defp precompile_replacement(<<?\\, x, rest::binary>>) when x in ?0..?9 do
    {ns, rest} = pick_int(rest)
    [List.to_integer([x | ns]) | precompile_replacement(rest)]
  end

  defp precompile_replacement(<<x, rest::binary>>) do
    case precompile_replacement(rest) do
      [head | t] when is_binary(head) ->
        [<<x, head::binary>> | t]

      other ->
        [<<x>> | other]
    end
  end

  defp pick_int(<<x, rest::binary>>) when x in ?0..?9 do
    {found, rest} = pick_int(rest)
    {[x | found], rest}
  end

  defp pick_int(bin) do
    {[], bin}
  end

  defp apply_list(string, replacement, list) do
    apply_list(string, string, 0, replacement, list)
  end

  defp apply_list(_, "", _, _, []) do
    []
  end

  defp apply_list(_, string, _, _, []) do
    string
  end

  defp apply_list(whole, string, pos, replacement, [[{mpos, _} | _] | _] = list)
       when mpos > pos do
    length = mpos - pos
    <<untouched::binary-size(^length), rest::binary>> = string
    [untouched | apply_list(whole, rest, mpos, replacement, list)]
  end

  defp apply_list(whole, string, pos, replacement, [[{pos, length} | _] = head | tail]) do
    <<_::size(^length)-binary, rest::binary>> = string
    new_data = apply_replace(whole, replacement, head)
    [new_data | apply_list(whole, rest, pos + length, replacement, tail)]
  end

  defp apply_replace(string, {fun, arity}, indexes) do
    apply(fun, get_indexes(string, indexes, arity))
  end

  defp apply_replace(_, [bin], _) when is_binary(bin) do
    bin
  end

  defp apply_replace(string, repl, indexes) do
    indexes = List.to_tuple(indexes)

    for part <- repl do
      cond do
        is_binary(part) ->
          part

        part >= tuple_size(indexes) ->
          ""

        true ->
          get_index(string, elem(indexes, part))
      end
    end
  end

  defp get_index(_string, {pos, _length}) when pos < 0 do
    ""
  end

  defp get_index(string, {pos, length}) do
    <<_::size(^pos)-binary, res::size(^length)-binary, _::binary>> = string
    res
  end

  defp get_indexes(_string, _, 0) do
    []
  end

  defp get_indexes(string, [], arity) do
    ["" | get_indexes(string, [], arity - 1)]
  end

  defp get_indexes(string, [h | t], arity) do
    [get_index(string, h) | get_indexes(string, t, arity - 1)]
  end

  @doc ~S"""
  Escapes a string to be literally matched in a regex.

  ## Examples

      iex> Regex.escape(".")
      "\\."

      iex> Regex.escape("\\what if")
      "\\\\what\\ if"

  """
  @spec escape(String.t()) :: String.t()
  def escape(string) when is_binary(string) do
    string
    |> escape(_length = 0, string)
    |> IO.iodata_to_binary()
  end

  @escapable :binary.bin_to_list(".^$*+?()[]{}|#-\\\t\n\v\f\r\s")

  defp escape(<<char, rest::binary>>, length, original) when char in @escapable do
    escape_char(rest, length, original, char)
  end

  defp escape(<<_, rest::binary>>, length, original) do
    escape(rest, length + 1, original)
  end

  defp escape(<<>>, _length, original) do
    original
  end

  defp escape_char(<<rest::binary>>, 0, _original, char) do
    [?\\, char | escape(rest, 0, rest)]
  end

  defp escape_char(<<rest::binary>>, length, original, char) do
    [binary_part(original, 0, length), ?\\, char | escape(rest, 0, rest)]
  end

  # Helpers

  # translate options to modifiers as required for emedding
  defp embeddable_modifiers(list), do: embeddable_modifiers(list, [], [])

  defp embeddable_modifiers([:dotall, {:newline, :anycrlf} | t], acc, err),
    do: embeddable_modifiers(t, [?s | acc], err)

  defp embeddable_modifiers([:caseless | t], acc, err),
    do: embeddable_modifiers(t, [?i | acc], err)

  defp embeddable_modifiers([:extended | t], acc, err),
    do: embeddable_modifiers(t, [?x | acc], err)

  defp embeddable_modifiers([:multiline | t], acc, err),
    do: embeddable_modifiers(t, [?m | acc], err)

  defp embeddable_modifiers([option | t], acc, err),
    do: embeddable_modifiers(t, acc, [option | err])

  defp embeddable_modifiers([], acc, []), do: {:ok, acc}
  defp embeddable_modifiers([], acc, err), do: {:error, acc, err}

  # translate modifiers to options

  defp translate_options(<<?s, t::binary>>, acc),
    do: translate_options(t, [:dotall, {:newline, :anycrlf} | acc])

  defp translate_options(<<?u, t::binary>>, acc), do: translate_options(t, [:unicode, :ucp | acc])
  defp translate_options(<<?i, t::binary>>, acc), do: translate_options(t, [:caseless | acc])
  defp translate_options(<<?x, t::binary>>, acc), do: translate_options(t, [:extended | acc])
  defp translate_options(<<?f, t::binary>>, acc), do: translate_options(t, [:firstline | acc])
  defp translate_options(<<?U, t::binary>>, acc), do: translate_options(t, [:ungreedy | acc])
  defp translate_options(<<?m, t::binary>>, acc), do: translate_options(t, [:multiline | acc])

  defp translate_options(<<?r, t::binary>>, acc) do
    IO.warn("the /r modifier in regular expressions is deprecated, please use /U instead")
    translate_options(t, [:ungreedy | acc])
  end

  defp translate_options(<<>>, acc), do: acc
  defp translate_options(t, _acc), do: {:error, t}

  @doc false
  def __escape__(%{__struct__: Regex} = regex) do
    # OTP 28.0 introduced refs in patterns, which can't be used in AST anymore
    # OTP 28.1 introduced :re.import/1 which allows us to work with pre-compiled binaries again

    pattern_ast =
      cond do
        # TODO: Remove this when we require Erlang/OTP 28+
        # Before OTP 28.0, patterns did not contain any refs and could be safely be escaped
        :erlang.system_info(:otp_release) < [?2, ?8] ->
          Macro.escape(regex.re_pattern)

        # OTP 28.1+ introduced the ability to export and import regexes from compiled binaries
        Code.ensure_loaded?(:re) and function_exported?(:re, :import, 1) ->
          {:ok, exported} = :re.compile(regex.source, [:export] ++ regex.opts)

          quote do
            require Regex
            Regex.__import_pattern__(unquote(Macro.escape(exported)))
          end

        # TODO: Remove this when we require Erlang/OTP 28.1+
        # OTP 28.0 works in degraded mode performance-wise, we need to recompile from the source
        true ->
          quote do
            {:ok, pattern} =
              :re.compile(unquote(Macro.escape(regex.source)), unquote(Macro.escape(regex.opts)))

            pattern
          end
      end

    quote do
      %{
        __struct__: unquote(Regex),
        re_pattern: unquote(pattern_ast),
        source: unquote(Macro.escape(regex.source)),
        opts: unquote(Macro.escape(regex.opts))
      }
    end
  end

  @doc false
  defmacro __import_pattern__(pattern) do
    if __CALLER__.context in [:match, :guard] do
      raise ArgumentError, "escaped"
    end

    quote do
      :re.import(unquote(pattern))
    end
  end
end
