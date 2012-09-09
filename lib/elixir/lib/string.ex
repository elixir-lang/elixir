defmodule String do
  @moduledoc """
  A string in Elixir is a utf-8 binary. This module
  contains function to work with utf-8 data and its
  codepoints.

  For working with raw binaries, use Erlang's :binary
  module.
  """

  @doc """
  Divides a string into sub string based on a pattern,
  returning a list of these sub string. The pattern can
  be a string, a list of strings or a regular expression.

  The string is split into two parts by default, unless
  `global` option is true. If a pattern is not specified,
  the string is split on whitespace occurrences.

  It returns a list with the original string if the pattern
  can't be matched.

  ## Examples

    String.split("a,b,c", ",")  #=> ["a", "b,c"]
    String.split("a,b,c", ",", global: true)  #=> ["a", "b,c"]
    String.split("foo bar")     #=> ["foo", "bar"]
    String.split("1,2 3,4", [" ", ","]) #=> ["1", "2 3,4"]
    String.split("1,2 3,4", [" ", ","], global: true) #=> ["1", "2", "3", "4"]
    String.split("a,b", ".")    #=> ["a,b"]

    String.split("a,b,c", %r{,})  #=> ["a", "b,c"]
    String.split("a,b,c", %r{,}, global: true) #=> ["a", "b", "c"]
    String.split("a,b", %r{\.})   #=> ["a,b"]

  """
  def split(binary, pattern // " ", options // [])

  def split(binary, pattern, options) when is_regex(pattern) do
    parts = if options[:global], do: :infinity, else: 2
    Regex.split(pattern, binary, parts: parts)
  end

  def split(binary, pattern, options) do
    options_list = []
    if options[:global] do
      options_list = [:global|options_list]
    end
    :binary.split(binary, pattern, options_list)
  end
end