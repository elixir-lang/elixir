defmodule Regex do
  @moduledoc ~S"""
  Regular expressions for Elixir built on top of the `re` module
  in the Erlang Standard Library. More information can be found
  in the [`re` documentation](http://www.erlang.org/doc/man/re.html).

  Regular expressions in Elixir can be created using `Regex.compile!/2`
  or using the special form with [`~r`](Kernel.html#sigil_r/2):

      # A simple regular expressions that matches foo anywhere in the string
      ~r/foo/

      # A regular expression with case insensitive options
      ~r/foo/i

  The `re` module provides several options, the ones available in Elixir, followed by
  their shortcut in parenthesis, are:

  * `caseless` (i) - add case insensitivity
  * `dotall` (s) - causes dot to match newlines and also set newline to anycrlf.
    The new line setting can be overridden by setting `(*CR)` or `(*LF)` or
    `(*CRLF)` or `(*ANY)` according to re documentation
  * `multiline` (m) - causes `^` and `$` to mark the beginning and end of each line.
    Use `\A` and `\z` to match the end or beginning of the string
  * `extended` (x) - whitespace characters are ignored except when escaped and
    allow `#` to delimit comments
  * `firstline` (f) - forces the unanchored pattern to match before or at the first
    newline, though the matched text may continue over the newline
  * `ungreedy` (r) - inverts the "greediness" of the regexp

  The options not available are:

  * `anchored` - not available, use `^` or `\A` instead
  * `dollar_endonly` - not available, use `\z` instead
  * `no_auto_capture` - not available, use `?:` instead
  * `newline` - not available, use `(*CR)` or `(*LF)` or `(*CRLF)` or `(*ANYCRLF)`
    or `(*ANY)` at the beginning of the regexp according to the re documentation

  ## Captures

  Many functions in this module allows what to capture in a regex
  match via the `:capture` option. The supported values are:

  * `:all` - all captured subpatterns including the complete matching string.
             This is the default;

  * `:first` - only the first captured subpattern, which is always the complete
               matching part of the string. All explicitly captured subpatterns are
               discarded;

  * `:all_but_first`- all but the first matching subpattern, i.e. all explicitly
                      captured subpatterns, but not the complete matching part of
                      the string;

  * `:none` - do not return matching subpatterns at all;

  * `:groups` - captures only named captures in the Regex;

  * `list(binary)` - a list of named captures to capture;

  """

  defrecordp :regex, Regex, [:re_pattern, :source, :options]
  @type t :: { Regex, term, binary, binary, [atom] | nil }

  defexception CompileError, message: "regex could not be compiled"

  @doc """
  Compiles the regular expression.

  The given options can either be a binary with the characters
  representing the same regex options given to the `~r` sigil,
  or a list of options, as expected by the [Erlang `re` docs](http://www.erlang.org/doc/man/re.html).

  It returns `{ :ok, regex }` in case of success,
  `{ :error, reason }` otherwise.

  ## Examples

      iex> Regex.compile("foo")
      {:ok, ~r"foo"}

      iex> Regex.compile("*foo")
      {:error, {'nothing to repeat', 0}}

  """
  @spec compile(binary, binary | [term]) :: { :ok, t } | { :error, any }
  def compile(source, options \\ "")

  def compile(source, options) when is_binary(options) do
    case translate_options(options) do
      { :error, rest } ->
        { :error, { :invalid_option, rest } }

      translated_options ->
        # Always use the unicode option, we don't have a latin1 legacy like
        # Erlang.
        compile(source, [:unicode|translated_options], options)
    end
  end

  def compile(source, options) when is_list(options) do
    compile(source, options, "")
  end

  defp compile(source, opts, doc_opts) when is_binary(source) do
    case :re.compile(source, opts) do
      { :ok, re_pattern } ->
        { :ok, regex(re_pattern: re_pattern, source: source, options: doc_opts) }
      error ->
        error
    end
  end

  @doc """
  Compiles the regular expression according to the given options.
  Fails with `Regex.CompileError` if the regex cannot be compiled.
  """
  def compile!(source, options \\ "") do
    case compile(source, options) do
      { :ok, regex } -> regex
      { :error, { reason, at } } -> raise Regex.CompileError, message: "#{reason} at position #{at}"
    end
  end

  @doc """
  Returns a boolean indicating whether there was a match or not.

  ## Examples

      iex> Regex.match?(~r/foo/, "foo")
      true

      iex> Regex.match?(~r/foo/, "bar")
      false

  """
  def match?(regex(re_pattern: compiled), string) when is_binary(string) do
    :re.run(string, compiled, [{ :capture, :none }]) == :match
  end

  @doc """
  Returns true if the given argument is a regex.

  ## Examples

      iex> Regex.regex?(~r/foo/)
      true

      iex> Regex.regex?(0)
      false

  """
  def regex?(regex()), do: true
  def regex?(_), do: false

  @doc """
  Runs the regular expression against the given string until the first match.
  It returns a list with all captures or `nil` if no match occurred.

  ## Options

  * `:return` - Set to `:index` to return indexes. Defaults to `:binary`;
  * `:capture` - What to capture in the result. Check the moduledoc for Regex
                 to see the possible capture values;

  ## Examples

      iex> Regex.run(~r/c(d)/, "abcd")
      ["cd", "d"]

      iex> Regex.run(~r/e/, "abcd")
      nil

      iex> Regex.run(~r/c(d)/, "abcd", return: :index)
      [{2,2},{3,1}]

  """
  def run(regex, string, options \\ [])

  def run(regex(re_pattern: compiled) = regex, string, options) when is_binary(string) do
    return = Keyword.get(options, :return, :binary)

    captures =
      case Keyword.get(options, :capture, :all) do
        :groups -> groups(regex)
        others  -> others
      end

    case :re.run(string, compiled, [{ :capture, captures, return }]) do
      :nomatch -> nil
      :match   -> []
      { :match, results } -> results
    end
  end

  @doc """
  Returns the given captures as a keyword list or `nil` if no captures
  are found. The option `:return` can be set to `:index` to get indexes
  back.

  ## Examples

      iex> Regex.named_captures(~r/c(?<foo>d)/, "abcd")
      [foo: "d"]

      iex> Regex.named_captures(~r/a(?<foo>b)c(?<bar>d)/, "abcd")
      [bar: "d", foo: "b"]

      iex> Regex.named_captures(~r/a(?<foo>b)c(?<bar>d)/, "efgh")
      nil

  """
  def named_captures(regex, string, options \\ []) when is_binary(string) do
    options = [capture: :groups] ++ options
    results = run(regex, string, options)
    if results, do: Enum.zip(groups(regex), results)
  end

  @doc """
  Returns the underlying `re_pattern` in the regular expression.
  """
  def re_pattern(regex(re_pattern: compiled)) do
    compiled
  end

  @doc """
  Returns the regex source as a binary.

  ## Examples

      iex> Regex.source(~r(foo))
      "foo"

  """
  def source(regex(source: source)) do
    source
  end

  @doc """
  Returns the regex options as a string.

  ## Examples

      iex> Regex.opts(~r(foo)m)
      "m"

  """
  def opts(regex(options: options)) do
    options
  end

  @doc """
  Returns a list of named groups in the regex.

  ## Examples

      iex> Regex.groups(~r/(?<foo>bar)/)
      [:foo]

  """
  def groups(regex(re_pattern: re_pattern)) do
    { :namelist, groups } = :re.inspect(re_pattern, :namelist)
    for group <- groups, do: binary_to_atom(group)
  end

  @doc """
  Same as `run/3`, but scans the target several times collecting all
  matches of the regular expression. A list of lists is returned,
  where each entry in the primary list represents a match and each
  entry in the secondary list represents the captured contents.

  ## Options

  * `:return` - Set to `:index` to return indexes. Defaults to `:binary`;
  * `:capture` - What to capture in the result. Check the moduledoc for Regex
                 to see the possible capture values;

  ## Examples

      iex> Regex.scan(~r/c(d|e)/, "abcd abce")
      [["cd", "d"], ["ce", "e"]]

      iex> Regex.scan(~r/c(?:d|e)/, "abcd abce")
      [["cd"], ["ce"]]

      iex> Regex.scan(~r/e/, "abcd")
      []

  """
  def scan(regex, string, options \\ [])

  def scan(regex(re_pattern: compiled) = regex, string, options) when is_binary(string) do
    return  = Keyword.get(options, :return, :binary)

    captures =
      case Keyword.get(options, :capture, :all) do
        :groups -> groups(regex)
        others  -> others
      end

    options = [{ :capture, captures, return }, :global]
    case :re.run(string, compiled, options) do
      :match -> []
      :nomatch -> []
      { :match, results } -> results
    end
  end

  @doc """
  Splits the given target into the number of parts specified.

  ## Options

  * `:global` - splits the string in all parts unless set to `false`,
                which leads the string to be split in 2. Defaults to true;

  * `:parts` - when specified, splits the string into maximum the number
               of given parts;

  * `:trim` - when true, remove blank strings from the result;

  ## Examples

      iex> Regex.split(~r/-/, "a-b-c")
      ["a","b","c"]

      iex> Regex.split(~r/-/, "a-b-c", [parts: 2])
      ["a","b-c"]

      iex> Regex.split(~r/-/, "abc")
      ["abc"]

      iex> Regex.split(~r//, "abc")
      ["a", "b", "c", ""]

      iex> Regex.split(~r//, "abc", trim: true)
      ["a", "b", "c"]

  """

  def split(regex, string, options \\ [])

  def split(regex(re_pattern: compiled), string, options) when is_binary(string) do
    parts =
      cond do
        Keyword.get(options, :global) == false -> 2
        p = Keyword.get(options, :parts)       -> p
        true                                   -> :infinity
      end

    opts   = [return: :binary, parts: parts]
    splits = :re.split(string, compiled, opts)

    if Keyword.get(options, :trim, false) do
      for split <- splits, split != "", do: split
    else
      splits
    end
  end

  @doc ~S"""
  Receives a regex, a binary and a replacement, returns a new
  binary where the all matches are replaced by replacement.

  Inside the replacement, you can either give `&` to access the
  whole regular expression or `\N`, where `N` is in integer to access
  a specific matching parens. You can also set `:global` to `false`
  if you want to replace just the first occurrence.

  ## Examples

      iex> Regex.replace(~r/d/, "abc", "d")
      "abc"

      iex> Regex.replace(~r/b/, "abc", "d")
      "adc"

      iex> Regex.replace(~r/b/, "abc", "[&]")
      "a[b]c"

      iex> Regex.replace(~r/b/, "abc", "[\\&]")
      "a[&]c"

      iex> Regex.replace(~r/(b)/, "abc", "[\\1]")
      "a[b]c"

  """
  def replace(regex, string, replacement, options \\ [])

  def replace(regex(re_pattern: compiled), string, replacement, options) when is_binary(string) do
    opts = if Keyword.get(options, :global) != false, do: [:global], else: []
    opts = [{ :return, :binary }|opts]
    :re.replace(string, compiled, replacement, opts)
  end

  { :ok, pattern } = :re.compile(~S"[.^$*+?()[{\\\|\s#]", [:unicode])
  @escape_pattern pattern

  @doc ~S"""
  Escapes a string to be literally matched in a regex.

  ## Examples

      iex> Regex.escape(".")
      "\\."

      iex> Regex.escape("\\what if")
      "\\\\what\\ if"

  """
  @spec escape(String.t) :: String.t
  def escape(string) when is_binary(string) do
    :re.replace(string, @escape_pattern, "\\\\&", [:global, { :return, :binary }])
  end

  # Helpers

  @doc false
  # Unescape map function used by Macro.unescape_string.
  def unescape_map(?f), do: ?\f
  def unescape_map(?n), do: ?\n
  def unescape_map(?r), do: ?\r
  def unescape_map(?t), do: ?\t
  def unescape_map(?v), do: ?\v
  def unescape_map(?a), do: ?\a
  def unescape_map(_),  do: false

  # Private Helpers

  defp translate_options(<<?g, t :: binary>>) do
    IO.write :stderr, "The /g flag for regular expressions is no longer needed\n#{Exception.format_stacktrace}"
    translate_options(t)
  end

  defp translate_options(<<?i, t :: binary>>), do: [:caseless|translate_options(t)]
  defp translate_options(<<?x, t :: binary>>), do: [:extended|translate_options(t)]
  defp translate_options(<<?f, t :: binary>>), do: [:firstline|translate_options(t)]
  defp translate_options(<<?r, t :: binary>>), do: [:ungreedy|translate_options(t)]
  defp translate_options(<<?s, t :: binary>>), do: [:dotall, {:newline, :anycrlf}|translate_options(t)]
  defp translate_options(<<?m, t :: binary>>), do: [:multiline|translate_options(t)]
  defp translate_options(<<>>), do: []
  defp translate_options(rest), do: { :error, rest }
end
