# This file has its compilation step because
# it needs to parse String.Unicode data and
# compile a digested module.
defmodule String.Unicode do
  @moduledoc false

  def version, do: {6,2,0}

  to_binary = fn(codepoint) ->
    :unicode.characters_to_binary([binary_to_integer(codepoint, 16)])
  end

  data_path = File.expand_path("../UnicodeData.txt", __FILE__)

  codes = Enum.reduce File.iterator!(data_path), [], fn(line, acc) ->
    [ codepoint, _name, _category,
      _class, _bidi, _decomposition,
      _numeric_1, _numeric_2, _numeric_3,
      _bidi_mirror, _unicode_1, _iso,
      upper, lower, _title ] = :binary.split(line, ";", [:global])

    if upper != "" or lower != "" do
      [{ to_binary.(codepoint), upper, lower }|acc]
    else
      acc
    end
  end

  seqs_path = File.expand_path("../NamedSequences.txt", __FILE__)

  seqs = Enum.map(File.iterator!(seqs_path), fn(line) ->
    [ _name, codepoints ] = :binary.split(line, ";", [:global])
    codepoints = Enum.filter(:binary.split(codepoints, " ", [:global, :trim]), 
                             fn(x) -> size(x) > 0 end)
    Enum.map(codepoints, fn(x) -> to_binary.(x) end)
  end)

  # Downcase

  lc { codepoint, _upper, lower } inlist codes, lower != "" do
    lower = to_binary.(lower)
    args  = quote do: [unquote(codepoint) <> t]
    code  = quote do: unquote(lower) <> downcase(t)
    def :downcase, args, [], do: code
  end

  def downcase(<< h, t :: binary >>) do
    << h >> <> downcase(t)
  end

  def downcase(<< >>) do
    << >>
  end

  # Upcase

  lc { codepoint, upper, _lower } inlist codes, upper != "" do
    upper = to_binary.(upper)
    args  = quote do: [unquote(codepoint) <> t]
    code  = quote do: unquote(upper) <> upcase(t)
    def :upcase, args, [], do: code
  end

  def upcase(<< h, t :: binary >>) do
    << h >> <> upcase(t)
  end

  def upcase(<< >>) do
    << >>
  end

  # Sequences

  lc codepoints inlist seqs do
    seq_args  = quote do: [<< unquote_splicing(codepoints), t :: binary >>]
    seq_code  = quote do: {<< unquote_splicing(codepoints) >>, t}
    def :sequence, seq_args, [], do: seq_code  
  end

  def sequence(binary) when is_binary(binary) and byte_size(binary) > 0 do
    case codepoint(binary) do
      :no_codepoint -> :no_sequence
      other -> other
    end
  end

  def sequence(<< >>) do
    :no_sequence
  end

  def sequences(binary) when is_binary(binary) and byte_size(binary) > 0 do
    case sequence(binary) do 
      { c, rest } -> [c|sequences(rest)]
      :no_sequence -> []
    end
  end

  def sequences(<< >>) do
    []
  end


  # Private implementation which returns the first codepoint
  # of any given utf8 string and the rest of it
  # If an empty string is given, :no_codepoint is returned.
  @doc false
  def codepoint(<<194, char, rest :: binary>>)
    when char in 161..191,
    do: { <<194, char>>, rest }


  def codepoint(<<first, char, rest :: binary>>)
    when first in 195..223 and char in 128..191,
    do: { <<first, char>>, rest }


  def codepoint(<<first, second, char, rest :: binary>>)
    when first == 224 and second in 160..191 and char in 128..191,
    do: { <<first, second, char>>, rest }


  def codepoint(<<first, second, char, rest :: binary>>)
    when first in 225..239 and second in 128..191 and char in 128..191,
    do: { <<first, second, char>>, rest }


  def codepoint(<<other, rest :: binary>>), do: { <<other>>, rest }


  def codepoint(<<>>), do: :no_codepoint
end