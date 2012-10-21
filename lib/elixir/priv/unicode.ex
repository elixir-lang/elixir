# This file has its compilation step because
# it needs to parse String.Unicode data and
# compile a digested module.
defmodule String.Unicode do
  @moduledoc false

  def version, do: {6,2,0}

  to_binary = fn(codepoint) ->
    :unicode.characters_to_binary([binary_to_integer(codepoint, 16)])
  end

  path = File.expand_path("../UnicodeData.txt", __FILE__)

  codes = Enum.reduce File.iterator!(path), [], fn(line, acc) ->
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
end