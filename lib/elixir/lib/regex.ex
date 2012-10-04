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

  * unicode (u) - used when you want to match against specific unicode characters
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
    case Erlang.re.compile(source, re_opts) do
      { :ok, compiled } ->
        { :ok, { Regex, compiled, source, options, groups } }
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
  def index({ Regex, compiled, _, _, _ }, string) do
    case Erlang.re.run(string, compiled, [{ :capture, :first, :index }]) do
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
  def match?({ Regex, compiled, _, _, _ }, string) do
    Erlang.re.run(string, compiled, [{ :capture, :none }]) == :match
  end

  @doc """
  Runs the regular expression against the given string.
  It returns a list with all matches or nil if no match ocurred.

  ## Examples

      Regex.run %r/c(d)/, "abcd"  #=> ["cd", "d"]
      Regex.run %r/e/, "abcd"     #=> nil

  """
  def run(regex, string, options // [])
  def run({ Regex, compiled, _, _, groups }, string, options) do
    return = Keyword.get(options, :return, return_for(string))

    captures =
      case Keyword.get(options, :capture, :all) do
        :groups -> groups || raise "Regex was not compiled with g"
        others  -> others
      end

    case Erlang.re.run(string, compiled, [{ :capture, captures, return }]) do
      :nomatch -> nil
      { :match, results } -> results
    end
  end

  @doc """
  Returns the given captures as a list of tuples.

  ## Examples

      Regex.captures %r/c(?<foo>d)/g, "abcd"  #=> [{:foo, ["d"]}]

  """
  def captures({ Regex, _, _, _, groups } = regex, string, options // []) do
    unless captures = Keyword.get(options, :capture) do
      captures = if groups do
        List.sort(groups)
      else
        raise "Regex was not compiled with g"
      end
      options  = Keyword.put(options, :capture, captures)
    end
    results = run(regex, string, options)
    if results, do: List.zip captures, results
  end

  @doc """
  Returns a list with the match indexes in the given string.
  The matches are tuples where the first element is the index
  (zero indexed) the match happened and the second is the length
  of the match.

  ## Examples

      Regex.indexes %r/c(d)/, "abcd"  #=> [{2,2},{3,1}]
      Regex.indexes %r/e/, "abcd"     #=> nil

  """
  def indexes({ Regex, compiled, _, _, _ }, string) do
    IO.write "[WARNING] Regex.indexes is deprecated, please use Regex.run with return: :index as option instead\n#{Exception.formatted_stacktrace}"
    case Erlang.re.run(string, compiled, [{ :capture, :all, :index }]) do
      :nomatch -> nil
      { :match, results } -> results
    end
  end

  @doc """
  Returns the underlying re_pattern in the regular expression.
  """
  def re_pattern({ Regex, compiled, _, _, _ }) do
    compiled
  end

  @doc """
  Returns the regex source as binary.

  ## Examples

      Regex.source %r(foo) #=> "foo"

  """
  def source({ Regex, _, source, _, _ }) do
    source
  end

  @doc """
  Returns the regex options as a list.

  ## Examples

      Regex.opts %r(foo)m #=> 'm'

  """
  def opts({ Regex, _, _, opts, _ }) do
    opts
  end

  @doc """
  Returns list of named groups in regex.

  ## Examples

      Regex.groups %r/(?<foo>foo)/g #=> ["foo"]

  """
  def groups({ Regex, _, _, _, groups }) do
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
  def scan({ Regex, compiled, _, _, _ }, string, options) do
    return = options[:return] || return_for(string)
    options = [{ :capture, :all, return }, :global]
    case Erlang.re.run(string, compiled, options) do
      :nomatch -> []
      { :match, results } -> flatten_result(results)
    end
  end

  @doc """
  Split the given target in the number of parts specified.
  If no ammount of parts is given, it defaults to :infinity.
  """

  def split(regex, string, options // [])

  def split(regex, string, options) when is_integer(options) or is_atom(options) do
    IO.write "[WARNING] Passing an integer or atom to Regex.split/3 is deprecated, pass a :parts option instead\n#{Exception.formatted_stacktrace}"
    split(regex, string, parts: options)
  end

  def split({ Regex, compiled, _, _, _ }, string, options) do
    parts =
      cond do
        options[:global] == false -> 2
        p = options[:parts]       -> p
        true                      -> :infinity
      end

    return = options[:return] || return_for(string)
    opts   = [{ :return, return }, { :parts, parts }]
    Erlang.re.split(string, compiled, opts)
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
  def replace({ Regex, compiled, _, _, _ }, string, replacement, options // []) do
    opts   = if options[:global] != false, do: [:global], else: []
    return = options[:return] || return_for(string)
    opts   = [{ :return, return }|opts]
    Erlang.re.replace(string, compiled, replacement, opts)
  end

  @doc false
  def replace_all({ Regex, compiled, _, _, _ }, string, replacement) do
    IO.write "[WARNING] Regex.replace_all is deprecated, simply use Regex.replace instead\n#{Exception.formatted_stacktrace}"
    Erlang.re.replace(string, compiled, replacement, [{ :return, return_for(string) }, :global])
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
    case Erlang.re.run(source, pattern, options) do
      :nomatch -> []
      { :match, results } ->
        lc [group] inlist results, do: binary_to_atom(group)
    end
  end
end
