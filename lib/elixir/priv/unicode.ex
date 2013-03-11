# This file has its own compilation step because
# it needs to parse String.Unicode data and
# compile a digested module.
defmodule String.Unicode do
  @moduledoc false

  def version, do: {6,2,0}

  to_binary = fn
    "" ->
      nil
    codepoints ->
      codepoints = :binary.split(codepoints, " ", [:global])
      Enum.reduce codepoints, "", fn(codepoint, acc) ->
        acc <> << binary_to_integer(codepoint, 16) :: utf8 >>
      end
  end

  data_path = Path.expand("../UnicodeData.txt", __FILE__)

  { codes, whitespace } = Enum.reduce File.iterator!(data_path), { [], [] }, fn(line, { cacc, wacc }) ->
    [ codepoint, _name, _category,
      _class, bidi, _decomposition,
      _numeric_1, _numeric_2, _numeric_3,
      _bidi_mirror, _unicode_1, _iso,
      upper, lower, title ] = :binary.split(line, ";", [:global])

    title = :binary.part(title, 0, size(title) - 1)

    cond do
      upper != "" or lower != "" or title != "" ->
        { [{ to_binary.(codepoint), to_binary.(upper), to_binary.(lower), to_binary.(title) } | cacc], wacc }
      bidi in ["B", "S", "WS"] ->
        { cacc, [to_binary.(codepoint) | wacc] }
      true ->
        { cacc, wacc }
    end
  end

  special_path = Path.expand("../SpecialCasing.txt", __FILE__)

  codes = Enum.reduce File.iterator!(special_path), codes, fn(line, acc) ->
    [ codepoint, lower, title, upper, _comment ] = :binary.split(line, "; ", [:global])
    key = to_binary.(codepoint)
    :lists.keystore(key, 1, acc, { key, to_binary.(upper), to_binary.(lower), to_binary.(title) })
  end

  seqs_path = Path.expand("../NamedSequences.txt", __FILE__)

  seqs = Enum.map File.iterator!(seqs_path), fn(line) ->
    [ _name, codepoints ] = :binary.split(line, ";", [:global])
    codepoints = :binary.split(codepoints, " ", [:global])
    codepoints = Enum.map codepoints, Regex.replace(%r/\s+/, &1, "")
    codepoints = Enum.filter codepoints, fn(x) -> size(x) > 0 end
    Enum.map codepoints, to_binary.(&1)
  end

  # Downcase

  lc { codepoint, _upper, lower, _title } inlist codes, lower && lower != codepoint do
    def downcase(unquote(codepoint) <> t) do
      unquote(lower) <> downcase(t)
    end
  end

  def downcase(<< h, t :: binary >>) do
    << h >> <> downcase(t)
  end

  def downcase(<< >>) do
    << >>
  end

  # Upcase

  lc { codepoint, upper, _lower, _title } inlist codes, upper && upper != codepoint do
    def upcase(unquote(codepoint) <> t) do
      unquote(upper) <> upcase(t)
    end
  end

  def upcase(<< h, t :: binary >>) do
    << h >> <> upcase(t)
  end

  def upcase(<< >>) do
    << >>
  end

  # Titlecase once

  lc { codepoint, _upper, _lower, title } inlist codes, title && title != codepoint do
    def titlecase_once(unquote(codepoint) <> t) do
      { unquote(title), t }
    end
  end

  def titlecase_once(<< h, t :: binary >>) do
    { <<h>>, t }
  end

  def titlecase_once(<< >>) do
    { <<>>, <<>> }
  end

  # Strip

  def lstrip(""), do: ""

  lc char inlist whitespace do
    args  = quote do: [unquote(char) <> rest]
    exprs = quote do: lstrip(rest)
    def :lstrip, args, [], do: exprs
  end

  def lstrip(other) when is_binary(other), do: other

  def rstrip(""), do: ""

  def rstrip(string) when is_binary(string) do
    do_rstrip(string, "")
  end

  lc char inlist whitespace do
    defp do_rstrip(unquote(char) <> rest, buffer) do
      do_rstrip(rest, unquote(char) <> buffer)
    end
  end

  defp do_rstrip(<< char, string :: binary >>, buffer) do
    << buffer :: binary, char, do_rstrip(string, "") :: binary >>
  end

  defp do_rstrip(<<>>, _), do: <<>>

  # Graphemes

  lc codepoints inlist seqs do
    def next_grapheme(<< unquote_splicing(codepoints), t :: binary >>) do
      { << unquote_splicing(codepoints) >>, t }
    end
  end

  def next_grapheme(<<>>) do
    :no_grapheme
  end

  def next_grapheme(binary) when is_binary(binary) do
    case next_codepoint(binary) do
      :no_codepoint -> :no_grapheme
      other -> other
    end
  end

  def graphemes(binary) when is_binary(binary) do
    do_graphemes(next_grapheme(binary))
  end

  defp do_graphemes({ c, rest }) do
    [c|do_graphemes(next_grapheme(rest))]
  end

  defp do_graphemes(:no_grapheme) do
    []
  end

  # Codepoints

  def next_codepoint(<< cp :: utf8, rest :: binary >>) do
    { <<cp :: utf8>>, rest }
  end

  def next_codepoint(<< cp, rest :: binary >>) do
    { <<cp>>, rest }
  end

  def next_codepoint(<<>>) do
    :no_codepoint
  end

  def codepoints(binary) when is_binary(binary) do
    do_codepoints(next_codepoint(binary))
  end

  defp do_codepoints({ c, rest }) do
    [c|do_codepoints(next_codepoint(rest))]
  end

  defp do_codepoints(:no_codepoint) do
    []
  end
end
