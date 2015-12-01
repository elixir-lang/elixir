defmodule Regex do
  @moduledoc ~S"""
  Provides regular expressions for Elixir. Built on top of Erlang's `:re`
  module.

  As the `:re` module, Regex is based on PCRE
  (Perl Compatible Regular Expressions). More information can be
  found in the [`:re` module documentation](http://www.erlang.org/doc/man/re.html).

  Regular expressions in Elixir can be created using `Regex.compile!/2`
  or using the special form with [`~r`](Kernel.html#sigil_r/2) or [`~R`](Kernel.html#sigil_R/2):

      # A simple regular expressions that matches foo anywhere in the string
      ~r/foo/

      # A regular expression with case insensitive and unicode options
      ~r/foo/iu

  A Regex is represented internally as the `Regex` struct. Therefore,
  `%Regex{}` can be used whenever there is a need to match on them.

  ## Modifiers

  The modifiers available when creating a Regex are:

    * `unicode` (u) - enables unicode specific patterns like `\p` and change
      modifiers like `\w`, `\W`, `\s` and friends to also match on unicode.
      It expects valid unicode strings to be given on match

    * `caseless` (i) - add case insensitivity

    * `dotall` (s) - causes dot to match newlines and also set newline to
      anycrlf; the new line setting can be overridden by setting `(*CR)` or
      `(*LF)` or `(*CRLF)` or `(*ANY)` according to re documentation

    * `multiline` (m) - causes `^` and `$` to mark the beginning and end of
      each line; use `\A` and `\z` to match the end or beginning of the string

    * `extended` (x) - whitespace characters are ignored except when escaped
      and allow `#` to delimit comments

    * `firstline` (f) - forces the unanchored pattern to match before or at the
      first newline, though the matched text may continue over the newline

    * `ungreedy` (U) - inverts the "greediness" of the regexp
      (the previous `r` option is deprecated in favor of `U`)

  The options not available are:

    * `anchored` - not available, use `^` or `\A` instead
    * `dollar_endonly` - not available, use `\z` instead
    * `no_auto_capture` - not available, use `?:` instead
    * `newline` - not available, use `(*CR)` or `(*LF)` or `(*CRLF)` or
      `(*ANYCRLF)` or `(*ANY)` at the beginning of the regexp according to the
      re documentation

  ## Captures

  Many functions in this module handle what to capture in a regex
  match via the `:capture` option. The supported values are:

    * `:all` - all captured subpatterns including the complete matching string
      (this is the default)

    * `:first` - only the first captured subpattern, which is always the
      complete matching part of the string; all explicitly captured subpatterns
      are discarded

    * `:all_but_first`- all but the first matching subpattern, i.e. all
      explicitly captured subpatterns, but not the complete matching part of
      the string

    * `:none` - do not return matching subpatterns at all

    * `:all_names` - captures all names in the Regex

    * `list(binary)` - a list of named captures to capture

  """

  defstruct re_pattern: nil, source: "", opts: ""

  @type t :: %__MODULE__{re_pattern: term, source: binary, opts: binary}

  defmodule CompileError do
    defexception message: "regex could not be compiled"
  end

  @doc """
  Compiles the regular expression.

  The given options can either be a binary with the characters
  representing the same regex options given to the `~r` sigil,
  or a list of options, as expected by the Erlang's [`:re` module](http://www.erlang.org/doc/man/re.html).

  It returns `{:ok, regex}` in case of success,
  `{:error, reason}` otherwise.

  ## Examples

      iex> Regex.compile("foo")
      {:ok, ~r"foo"}

      iex> Regex.compile("*foo")
      {:error, {'nothing to repeat', 0}}

  """
  @spec compile(binary, binary | [term]) :: {:ok, t} | {:error, any}
  def compile(source, options \\ "")

  def compile(source, options) when is_binary(options) do
    case translate_options(options, []) do
      {:error, rest} ->
        {:error, {:invalid_option, rest}}

      translated_options ->
        compile(source, translated_options, options)
    end
  end

  def compile(source, options) when is_list(options) do
    compile(source, options, "")
  end

  defp compile(source, opts, doc_opts) when is_binary(source) do
    case :re.compile(source, opts) do
      {:ok, re_pattern} ->
        {:ok, %Regex{re_pattern: re_pattern, source: source, opts: doc_opts}}
      error ->
        error
    end
  end

  @doc """
  Compiles the regular expression according to the given options.
  Fails with `Regex.CompileError` if the regex cannot be compiled.
  """
  @spec compile(binary, binary | [term]) :: t
  def compile!(source, options \\ "") do
    case compile(source, options) do
      {:ok, regex} -> regex
      {:error, {reason, at}} -> raise Regex.CompileError, message: "#{reason} at position #{at}"
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
  @spec match?(t, String.t) :: boolean
  def match?(%Regex{re_pattern: compiled}, string) when is_binary(string) do
    :re.run(string, compiled, [{:capture, :none}]) == :match
  end

  @doc """
  Returns `true` if the given `term` is a regex.
  Otherwise returns `false`.

  ## Examples

      iex> Regex.regex?(~r/foo/)
      true

      iex> Regex.regex?(0)
      false

  """
  @spec regex?(t) :: true
  @spec regex?(any) :: false
  def regex?(term)
  def regex?(%Regex{}), do: true
  def regex?(_), do: false

  @doc """
  Runs the regular expression against the given string until the first match.
  It returns a list with all captures or `nil` if no match occurred.

  ## Options

    * `:return`  - set to `:index` to return indexes. Defaults to `:binary`.
    * `:capture` - what to capture in the result. Check the moduledoc for `Regex`
      to see the possible capture values.

  ## Examples

      iex> Regex.run(~r/c(d)/, "abcd")
      ["cd", "d"]

      iex> Regex.run(~r/e/, "abcd")
      nil

      iex> Regex.run(~r/c(d)/, "abcd", return: :index)
      [{2, 2}, {3, 1}]

  """
  @spec run(t, binary, [term]) :: nil | [binary] | [{integer, integer}]
  def run(regex, string, options \\ [])

  def run(%Regex{re_pattern: compiled}, string, options) when is_binary(string) do
    return   = Keyword.get(options, :return, :binary)
    captures = Keyword.get(options, :capture, :all)

    case :re.run(string, compiled, [{:capture, captures, return}]) do
      :nomatch -> nil
      :match   -> []
      {:match, results} -> results
    end
  end

  @doc """
  Returns the given captures as a map or `nil` if no captures are
  found. The option `:return` can be set to `:index` to get indexes
  back.

  ## Examples

      iex> Regex.named_captures(~r/c(?<foo>d)/, "abcd")
      %{"foo" => "d"}

      iex> Regex.named_captures(~r/a(?<foo>b)c(?<bar>d)/, "abcd")
      %{"bar" => "d", "foo" => "b"}

      iex> Regex.named_captures(~r/a(?<foo>b)c(?<bar>d)/, "efgh")
      nil

  """
  @spec named_captures(t, String.t, [term]) :: map | nil
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

      iex> Regex.source(~r(foo))
      "foo"

  """
  @spec source(t) :: String.t
  def source(%Regex{source: source}) do
    source
  end

  @doc """
  Returns the regex options as a string.

  ## Examples

      iex> Regex.opts(~r(foo)m)
      "m"

  """
  @spec opts(t) :: String.t
  def opts(%Regex{opts: opts}) do
    opts
  end

  @doc """
  Returns a list of names in the regex.

  ## Examples

      iex> Regex.names(~r/(?<foo>bar)/)
      ["foo"]

  """
  @spec names(t) :: [String.t]
  def names(%Regex{re_pattern: re_pattern}) do
    {:namelist, names} = :re.inspect(re_pattern, :namelist)
    names
  end

  @doc """
  Same as `run/3`, but scans the target several times collecting all
  matches of the regular expression.

  A list of lists is returned, where each entry in the primary list represents a
  match and each entry in the secondary list represents the captured contents.

  ## Options

    * `:return`  - set to `:index` to return indexes. Defaults to `:binary`.
    * `:capture` - what to capture in the result. Check the moduledoc for `Regex`
      to see the possible capture values.

  ## Examples

      iex> Regex.scan(~r/c(d|e)/, "abcd abce")
      [["cd", "d"], ["ce", "e"]]

      iex> Regex.scan(~r/c(?:d|e)/, "abcd abce")
      [["cd"], ["ce"]]

      iex> Regex.scan(~r/e/, "abcd")
      []

  """
  @spec scan(t, String.t, [term]) :: [[String.t]]
  def scan(regex, string, options \\ [])

  def scan(%Regex{re_pattern: compiled}, string, options) when is_binary(string) do
    return   = Keyword.get(options, :return, :binary)
    captures = Keyword.get(options, :capture, :all)
    options  = [{:capture, captures, return}, :global]

    case :re.run(string, compiled, options) do
      :match -> []
      :nomatch -> []
      {:match, results} -> results
    end
  end

  @doc """
  Splits the given target based on the given pattern and in the given number of
  parts.

  ## Options

    * `:parts` - when specified, splits the string into the given number of
      parts. If not specified, `:parts` defaults to `:infinity`, which will
      split the string into the maximum number of parts possible based on the
      given pattern.

    * `:trim` - when `true`, removes empty strings (`""`) from the result.

    * `:on` - specifies which captures to split the string on, and in what
      order. Defaults to `:first` which means captures inside the regex do not
      affect the splitting process.

  ## Examples

      iex> Regex.split(~r/-/, "a-b-c")
      ["a", "b", "c"]

      iex> Regex.split(~r/-/, "a-b-c", [parts: 2])
      ["a", "b-c"]

      iex> Regex.split(~r/-/, "abc")
      ["abc"]

      iex> Regex.split(~r//, "abc")
      ["a", "b", "c", ""]

      iex> Regex.split(~r/a(?<second>b)c/, "abc")
      ["", ""]

      iex> Regex.split(~r/a(?<second>b)c/, "abc", on: [:second])
      ["a", "c"]

  """
  @spec split(t, String.t, [term]) :: [String.t]
  def split(regex, string, options \\ [])

  def split(%Regex{}, "", opts) do
    if Keyword.get(opts, :trim, false) do
      []
    else
      [""]
    end
  end

  def split(%Regex{re_pattern: compiled}, string, opts) when is_binary(string) and is_list(opts) do
    on = Keyword.get(opts, :on, :first)
    case :re.run(string, compiled, [:global, capture: on]) do
      {:match, matches} ->
        do_split(matches, string, 0,
                 parts_to_index(Keyword.get(opts, :parts, :infinity)),
                 Keyword.get(opts, :trim, false))
      :match ->
        [string]
      :nomatch ->
        [string]
    end
  end

  defp parts_to_index(:infinity),                      do: 0
  defp parts_to_index(n) when is_integer(n) and n > 0, do: n

  defp do_split(_, string, offset, _counter, true) when byte_size(string) <= offset,
    do: []

  defp do_split(_, string, offset, 1, _trim),
    do: [binary_part(string, offset, byte_size(string) - offset)]

  defp do_split([], string, offset, _counter, _trim),
    do: [binary_part(string, offset, byte_size(string) - offset)]

  defp do_split([[{pos, _}|h]|t], string, offset, counter, trim) when pos - offset < 0,
    do: do_split([h|t], string, offset, counter, trim)

  defp do_split([[]|t], string, offset, counter, trim),
    do: do_split(t, string, offset, counter, trim)

  defp do_split([[{pos, length}|h]|t], string, offset, counter, trim) do
    new_offset = pos + length
    keep = pos - offset

    if keep == 0 and (length == 0 or trim) do
      do_split([h|t], string, new_offset, counter, trim)
    else
      <<_::binary-size(offset), part::binary-size(keep), _::binary>> = string
      [part|do_split([h|t], string, new_offset, counter - 1, trim)]
    end
  end

  @doc ~S"""
  Receives a regex, a binary and a replacement, returns a new
  binary where all matches are replaced by the replacement.

  The replacement can be either a string or a function. The string
  is used as a replacement for every match and it allows specific
  captures to be accessed via `\\N` or `\g{N}`, where `N` is the
  capture. In case `\\0` is used, the whole match is inserted.

  When the replacement is a function, the function may have arity
  N where each argument maps to a capture, with the first argument
  being the whole match. If the function expects more arguments
  than captures found, the remaining arguments will receive `""`.

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

      iex> Regex.replace(~r/a/, "abcadc", "A", global: false)
      "Abcadc"

  """
  @spec replace(t, String.t, String.t | (... -> String.t), [term]) :: String.t
  def replace(regex, string, replacement, options \\ [])

  def replace(regex, string, replacement, options)
      when is_binary(string) and is_binary(replacement) and is_list(options) do
    do_replace(regex, string, precompile_replacement(replacement), options)
  end

  def replace(regex, string, replacement, options)
      when is_binary(string) and is_function(replacement) and is_list(options)  do
    {:arity, arity} = :erlang.fun_info(replacement, :arity)
    do_replace(regex, string, {replacement, arity}, options)
  end

  defp do_replace(%Regex{re_pattern: compiled}, string, replacement, options) do
    opts = if Keyword.get(options, :global) != false, do: [:global], else: []
    opts = [{:capture, :all, :index}|opts]

    case :re.run(string, compiled, opts) do
      :nomatch ->
        string
      {:match, [mlist|t]} when is_list(mlist) ->
        apply_list(string, replacement, [mlist|t]) |> IO.iodata_to_binary
      {:match, slist} ->
        apply_list(string, replacement, [slist]) |> IO.iodata_to_binary
    end
  end

  defp precompile_replacement(""),
    do: []

  defp precompile_replacement(<<?\\, ?g, ?{, rest::binary>>) when byte_size(rest) > 0 do
    {ns, <<?}, rest::binary>>} = pick_int(rest)
    [List.to_integer(ns) | precompile_replacement(rest)]
  end

  defp precompile_replacement(<<?\\, ?\\, rest::binary>>) do
    [<<?\\>> | precompile_replacement(rest)]
  end

  defp precompile_replacement(<<?\\, x, rest::binary>>) when x in ?0..?9 do
    {ns, rest} = pick_int(rest)
    [List.to_integer([x|ns]) | precompile_replacement(rest)]
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
    {[x|found], rest}
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

  defp apply_list(whole, string, pos, replacement, [[{mpos, _} | _] | _] = list) when mpos > pos do
    length = mpos - pos
    <<untouched::binary-size(length), rest::binary>> = string
    [untouched | apply_list(whole, rest, mpos, replacement, list)]
  end

  defp apply_list(whole, string, pos, replacement, [[{pos, length} | _] = head | tail]) do
    <<_::size(length)-binary, rest::binary>> = string
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

  defp get_index(_string, {pos, _len}) when pos < 0 do
    ""
  end

  defp get_index(string, {pos, len}) do
    <<_::size(pos)-binary, res::size(len)-binary, _::binary>> = string
    res
  end

  defp get_indexes(_string, _, 0) do
    []
  end

  defp get_indexes(string, [], arity) do
    [""|get_indexes(string, [], arity - 1)]
  end

  defp get_indexes(string, [h|t], arity) do
    [get_index(string, h)|get_indexes(string, t, arity - 1)]
  end

  {:ok, pattern} = :re.compile(~S"[.^$*+?()[{\\\|\s#]", [:unicode])
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
    :re.replace(string, @escape_pattern, "\\\\&", [:global, {:return, :binary}])
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

  defp translate_options(<<?u, t::binary>>, acc), do: translate_options(t, [:unicode, :ucp|acc])
  defp translate_options(<<?i, t::binary>>, acc), do: translate_options(t, [:caseless|acc])
  defp translate_options(<<?x, t::binary>>, acc), do: translate_options(t, [:extended|acc])
  defp translate_options(<<?f, t::binary>>, acc), do: translate_options(t, [:firstline|acc])
  defp translate_options(<<?U, t::binary>>, acc), do: translate_options(t, [:ungreedy|acc])
  defp translate_options(<<?s, t::binary>>, acc), do: translate_options(t, [:dotall, {:newline, :anycrlf}|acc])
  defp translate_options(<<?m, t::binary>>, acc), do: translate_options(t, [:multiline|acc])

  # TODO: Deprecate by 1.2
  # TODO: Remove by 2.0
  defp translate_options(<<?r, t::binary>>, acc), do: translate_options(t, [:ungreedy|acc])

  defp translate_options(<<>>, acc), do: acc
  defp translate_options(rest, _acc), do: {:error, rest}
end
