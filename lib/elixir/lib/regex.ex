defmodule Regex do
  @moduledoc %B"""
  Regular expressions for Elixir built on top of the re module
  in the Erlang Standard Library. More information can be found
  on re documentation: http://www.erlang.org/doc/man/re.html

  Regular expressions in Elixir can be created using Regex.compile!
  or using the special form with `%r`:

      # A simple regular expressions that matches foo anywhere in the string
      %r/foo/

      # A regular expression with case insensitive options and handle unicode chars
      %r/foo/iu

  The re module provides several options, the one available in Elixir, followed by
  their shortcut in parenthesis, are:

  * unicode (u) - enable unicode specific patterns like \p
  * caseless (i) - add case insensitivity
  * dotall (s) - causes dot to match newlines and also set newline to anycrlf.
    The new line setting can be overwritten by setting `(*CR)` or `(*LF)` or
    `(*CRLF)` or `(*ANY)` according to re documentation
  * multiline (m) - causes `^` and `$` to mark the beginning and end of each line.
    You need to use `\A` and `\z` to match the end or beginning of the string
  * extended (x) - whitespace characters are ignored except when escaped and
    allow `#` to delimit comments
  * firstline (f) - forces the unanchored pattern to match before or at the first
    newline, though the matched text may continue over the newline
  * ungreedy (r) - invert the "greediness" of the regexp
  * groups (g) - compile with info about groups available

  The options not available are:

  * anchored - not available, use `^` or `\A` instead
  * dollar_endonly - not available, use `\z` instead
  * no_auto_capture - not available, use `?:` instead
  * newline - not available, use `(*CR)` or `(*LF)` or `(*CRLF)` or `(*ANYCRLF)`
    or `(*ANY)` at the beginning of the regexp according to the re documentation

  Most of the functions in this module accept either a binary or a char list
  as subject. The result is based on the argument (a binary will return
  a binary, a char list will return a char list).
  """

  defrecordp :regex, [:re_pattern, :source, :options, :groups]
  @type t :: { Regex, term, term, term, term }

  defexception CompileError, message: "regex could not be compiled"

  @doc """
  Compiles the regular expression according to the given options.

  It returns `{ :ok, regex }` in case of success,
  `{ :error, reason }` otherwise.
  """
  def compile(source, options // "") do
    source  = to_binary(source)
    options = to_binary(options)
    opts    = translate_options(options)
    re_opts = opts -- [:groups]
    groups  = if opts != re_opts, do: parse_groups(source)

    case :re.compile(source, re_opts) do
      { :ok, re_pattern } ->
        { :ok, regex(re_pattern: re_pattern, source: source, options: options, groups: groups) }
      error ->
        error
    end
  end

  @doc """
  Compiles the regular expression according to the given options.
  Fails with `Regex.CompileError` if the regex cannot be compiled.
  """
  def compile!(source, options // "") do
    case compile(source, options) do
      { :ok, regex } -> regex
      { :error, { reason, at } } -> raise Regex.CompileError, message: "#{reason} at position #{at}"
    end
  end

  @doc """
  Runs the regular expression against the given string
  and returns the index (zero indexes) where the first
  match occurs, nil otherwise.

  ## Examples

      Regex.index %r/c(d)/, "abcd"  #=> 3
      Regex.index %r/e/, "abcd"     #=> nil

  """
  def index(regex(re_pattern: compiled), string) do
    case :re.run(string, compiled, [{ :capture, :first, :index }]) do
      :nomatch -> nil
      { :match, [{index,_}] } -> index
    end
  end

  @doc """
  Returns a boolean if there was a match or not.

  ## Examples

      Regex.match? %r/foo/, "foo" #=> true
      Regex.match? %r/foo/, "bar" #=> false

  """
  def match?(regex(re_pattern: compiled), string) do
    :re.run(string, compiled, [{ :capture, :none }]) == :match
  end

  @doc """
  Runs the regular expression against the given string.
  It returns a list with all matches or nil if no match ocurred.

  ## Examples

      Regex.run %r/c(d)/, "abcd"  #=> ["cd", "d"]
      Regex.run %r/e/, "abcd"     #=> nil

  """
  def run(regex, string, options // [])

  def run(regex(re_pattern: compiled, groups: groups), string, options) do
    return = Keyword.get(options, :return, return_for(string))

    captures =
      case Keyword.get(options, :capture, :all) do
        :groups -> groups || raise "regex was not compiled with g"
        others  -> others
      end

    case :re.run(string, compiled, [{ :capture, captures, return }]) do
      :nomatch -> nil
      { :match, results } -> results
    end
  end

  @doc """
  Returns the given captures as a list of tuples.

  ## Examples

      Regex.captures %r/c(?<foo>d)/g, "abcd"  #=> [{:foo, ["d"]}]

  """
  def captures(regex(groups: groups) = regex, string, options // []) do
    unless captures = Keyword.get(options, :capture) do
      captures = if groups do
        Enum.sort(groups)
      else
        raise "Regex was not compiled with g"
      end
      options  = Keyword.put(options, :capture, captures)
    end
    results = run(regex, string, options)
    if results, do: Enum.zip captures, results
  end

  @doc """
  Returns the underlying re_pattern in the regular expression.
  """
  def re_pattern(regex(re_pattern: compiled)) do
    compiled
  end

  @doc """
  Returns the regex source as binary.

  ## Examples

      Regex.source %r(foo) #=> "foo"

  """
  def source(regex(source: source)) do
    source
  end

  @doc """
  Returns the regex options as a list.

  ## Examples

      Regex.opts %r(foo)m #=> 'm'

  """
  def opts(regex(options: options)) do
    options
  end

  @doc """
  Returns list of named groups in regex.

  ## Examples

      Regex.groups %r/(?<foo>foo)/g #=> ["foo"]

  """
  def groups(regex(groups: groups)) do
    groups
  end

  @doc """
  Same as run, but scans the target several times collecting all matches of
  the regular expression. A list is returned with each match. If the item in
  the list is a binary, it means there were no captures. If the item is another
  list, each element in this secondary list is a capture.

  ## Examples

      Regex.scan %r/c(d|e)/, "abcd abce"   #=> [["d"], ["e"]]
      Regex.scan %r/c(?:d|e)/, "abcd abce" #=> ["cd", "ce"]
      Regex.scan %r/e/, "abcd"             #=> []

  """
  def scan(regex, string, options // [])

  def scan(regex(re_pattern: compiled), string, options) do
    return  = Keyword.get(options, :return, return_for(string))
    options = [{ :capture, :all, return }, :global]
    case :re.run(string, compiled, options) do
      :nomatch -> []
      { :match, results } -> flatten_result(results)
    end
  end

  @doc """
  Split the given target in the number of parts specified.
  If no ammount of parts is given, it defaults to :infinity.
  """

  def split(regex, string, options // [])

  def split(regex(re_pattern: compiled), string, options) do
    parts =
      cond do
        Keyword.get(options, :global) == false -> 2
        p = Keyword.get(options, :parts)       -> p
        true                                   -> :infinity
      end

    return = Keyword.get(options, :return, return_for(string))
    opts   = [return: return, parts: parts]
    :re.split(string, compiled, opts)
  end

  @doc %B"""
  Receives a regex, a binary and a replacement and returns a new
  binary where the all matches are replaced by replacement.

  Inside the replacement, you can either give "&" to access the
  whole regular expression or \N, where N is in integer to access
  a specific matching parens. You can also set global to false
  if you want to replace just the first occurrence.

  ## Examples

      Regex.replace(%r/d/, "abc", "d")       #=> "abc"
      Regex.replace(%r/b/, "abc", "d")       #=> "adc"
      Regex.replace(%r/b/, "abc", "[&]")     #=> "a[b]c"
      Regex.replace(%r/b/, "abc", "[\\&]")   #=> "a[&]c"
      Regex.replace(%r/(b)/, "abc", "[\\1]") #=> "a[b]c"

  """
  def replace(regex(re_pattern: compiled), string, replacement, options // []) do
    opts   = if Keyword.get(options, :global) != false, do: [:global], else: []
    return = Keyword.get(options, :return, return_for(string))
    opts   = [{ :return, return }|opts]
    :re.replace(string, compiled, replacement, opts)
  end

  # Helpers

  @doc false
  # Unescape map function used by Macro.unescape_binary.
  def unescape_map(?f), do: ?\f
  def unescape_map(?n), do: ?\n
  def unescape_map(?r), do: ?\r
  def unescape_map(?t), do: ?\t
  def unescape_map(?v), do: ?\v
  def unescape_map(_),  do: false

  # Private Helpers

  defp return_for(element) when is_binary(element), do: :binary
  defp return_for(element) when is_list(element),   do: :list

  defp translate_options(<<?u, t :: binary>>), do: [:unicode|translate_options(t)]
  defp translate_options(<<?i, t :: binary>>), do: [:caseless|translate_options(t)]
  defp translate_options(<<?x, t :: binary>>), do: [:extended|translate_options(t)]
  defp translate_options(<<?f, t :: binary>>), do: [:firstline|translate_options(t)]
  defp translate_options(<<?r, t :: binary>>), do: [:ungreedy|translate_options(t)]
  defp translate_options(<<?s, t :: binary>>), do: [:dotall,{:newline,:anycrlf}|translate_options(t)]
  defp translate_options(<<?m, t :: binary>>), do: [:multiline|translate_options(t)]
  defp translate_options(<<?g, t :: binary>>), do: [:groups|translate_options(t)]
  defp translate_options(<<>>), do: []

  defp flatten_result(results) do
    lc result inlist results do
      case result do
        [t] -> t
        [h|t] -> t
      end
    end
  end

  defp parse_groups(source) do
    options = [:global, {:capture, ['G'], :binary}]
    {:ok, pattern} = :re.compile(%B"\(\?<(?<G>[^>]*)>")
    case :re.run(source, pattern, options) do
      :nomatch -> []
      { :match, results } ->
        lc [group] inlist results, do: binary_to_atom(group)
    end
  end
end
