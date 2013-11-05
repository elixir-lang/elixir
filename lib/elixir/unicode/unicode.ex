# This file has its own compilation step because it needs to parse
# String.Unicode and String.Graphemes data and compile digested modules.

defmodule String.Unicode do
  @moduledoc false
  def version, do: {6,3,0}

  to_binary = fn
    "" ->
      nil
    codepoints ->
      codepoints = :binary.split(codepoints, " ", [:global])
      Enum.reduce codepoints, "", fn(codepoint, acc) ->
        acc <> << binary_to_integer(codepoint, 16) :: utf8 >>
      end
  end

  data_path = Path.join(__DIR__, "UnicodeData.txt")

  { codes, whitespace } = Enum.reduce File.stream!(data_path), { [], [] }, fn(line, { cacc, wacc }) ->
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

  special_path = Path.join(__DIR__, "SpecialCasing.txt")

  codes = Enum.reduce File.stream!(special_path), codes, fn(line, acc) ->
    [ codepoint, lower, title, upper, _comment ] = :binary.split(line, "; ", [:global])
    key = to_binary.(codepoint)
    :lists.keystore(key, 1, acc, { key, to_binary.(upper), to_binary.(lower), to_binary.(title) })
  end

  # Downcase

  def downcase(string), do: do_downcase(string) |> iolist_to_binary

  lc { codepoint, _upper, lower, _title } inlist codes, lower && lower != codepoint do
    defp do_downcase(unquote(codepoint) <> rest) do
      unquote(:binary.bin_to_list(lower)) ++ downcase(rest)
    end
  end

  defp do_downcase(<< char, rest :: binary >>) do
    [char|do_downcase(rest)]
  end

  defp do_downcase(""), do: []

  # Upcase

  def upcase(string), do: do_upcase(string) |> iolist_to_binary

  lc { codepoint, upper, _lower, _title } inlist codes, upper && upper != codepoint do
    defp do_upcase(unquote(codepoint) <> rest) do
      unquote(:binary.bin_to_list(upper)) ++ do_upcase(rest)
    end
  end

  defp do_upcase(<< char, rest :: binary >>) do
    [char|do_upcase(rest)]
  end

  defp do_upcase(""), do: []

  # Titlecase once

  def titlecase_once(""), do: { "", "" }

  lc { codepoint, _upper, _lower, title } inlist codes, title && title != codepoint do
    def titlecase_once(unquote(codepoint) <> rest) do
      { unquote(title), rest }
    end
  end

  def titlecase_once(<< char, rest :: binary >>) do
    { << char >>, rest }
  end

  # Strip

  def lstrip(""), do: ""

  lc codepoint inlist whitespace do
    def lstrip(unquote(codepoint) <> rest) do
      lstrip(rest)
    end
  end

  def lstrip(other) when is_binary(other), do: other

  def rstrip(string) when is_binary(string) do
    do_rstrip(string, [], [])
  end

  lc codepoint inlist whitespace do
    c = :binary.bin_to_list(codepoint) |> :lists.reverse

    defp do_rstrip(unquote(codepoint) <> rest, acc1, acc2) do
      do_rstrip(rest, unquote(c) ++ (acc1 || acc2), acc2)
    end
  end

  defp do_rstrip(<< char, rest :: binary >>, nil, acc2) do
    do_rstrip(rest, nil, [char|acc2])
  end

  defp do_rstrip(<< char, rest :: binary >>, acc1, _acc2) do
    do_rstrip(rest, nil, [char|acc1])
  end

  defp do_rstrip(<<>>, _acc1, acc2), do: acc2 |> :lists.reverse |> iolist_to_binary

  # Split

  def split(""), do: [""]

  def split(string) when is_binary(string) do
    :lists.reverse do_split(string, "", [])
  end

  lc codepoint inlist whitespace do
    defp do_split(unquote(codepoint) <> rest, buffer, acc) do
      do_split(rest, "", add_buffer_to_acc(buffer, acc))
    end
  end

  defp do_split(<< char, rest :: binary >>, buffer, acc) do
    do_split(rest, << buffer :: binary, char >>, acc)
  end

  defp do_split(<<>>, buffer, acc) do
    add_buffer_to_acc(buffer, acc)
  end

  @compile { :inline, add_buffer_to_acc: 2 }

  defp add_buffer_to_acc("", acc),     do: acc
  defp add_buffer_to_acc(buffer, acc), do: [buffer|acc]

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

defmodule String.Graphemes do
  @moduledoc false

  cluster_path = Path.join(__DIR__, "GraphemeBreakProperty.txt")
  regex = %r/(?:^([0-9A-F]+)(?:\.\.([0-9A-F]+))?)\s+;\s(\w+)/m

  to_range = fn
    first, ""   ->
      [<< binary_to_integer(first, 16) :: utf8 >>]
    first, last ->
      range = binary_to_integer(first, 16)..binary_to_integer(last, 16)
      Enum.map(range, fn(int) -> << int :: utf8 >> end)
  end

  cluster = Enum.reduce File.stream!(cluster_path), HashDict.new, fn(line, dict) ->
    [ _full, first, last, class ] = Regex.run(regex, line)

    # Skip surrogates
    if first == "D800" and last == "DFFF" do
      dict
    else
      list = to_range.(first, last)
      Dict.update(dict, class, list, &(&1 ++ list))
    end
  end

  # There is no codepoint marked as Prepend by Unicode 6.3.0
  if cluster["Prepend"] do
    raise "It seems this new unicode version has added Prepend items. " <>
          "Please remove this error and uncomment the code below."
  end

  # Don't break CRLF
  def next_grapheme(<< ?\n, ?\r, rest :: binary >>) do
    { "\n\r", rest }
  end

  # Break on control
  lc codepoint inlist cluster["CR"] ++ cluster["LF"] ++ cluster["Control"] do
    def next_grapheme(<< unquote(codepoint), rest :: binary >>) do
      { << unquote(codepoint) >>, rest }
    end
  end

  # Break on Prepend*
  # lc codepoint inlist cluster["Prepend"] do
  #   def next_grapheme(<< unquote(codepoint), rest :: binary >>) do
  #     next_prepend(<< unquote(codepoint) >>, rest)
  #   end
  # end

  # Handle Hangul L
  lc codepoint inlist cluster["L"] do
    def next_grapheme(<< unquote(codepoint), rest :: binary >>) do
      next_hangul_l(<< unquote(codepoint) >>, rest)
    end
  end

  # Handle Hangul T
  lc codepoint inlist cluster["T"] do
    def next_grapheme(<< unquote(codepoint), rest :: binary >>) do
      next_hangul_t(<< unquote(codepoint) >>, rest)
    end
  end

  # Handle Regional
  lc codepoint inlist cluster["Regional_Indicator"] do
    def next_grapheme(<< unquote(codepoint), rest :: binary >>) do
      next_regional(<< unquote(codepoint) >>, rest)
    end
  end

  # Handle extended entries
  def next_grapheme(<< cp :: utf8, rest :: binary >>) do
    next_extend(<< cp :: utf8 >>, rest)
  end

  def next_grapheme(<< cp, rest :: binary >>) do
    { <<cp>>, rest }
  end

  def next_grapheme(<<>>) do
    :no_grapheme
  end

  # Handle Hangul L
  lc codepoint inlist cluster["L"] do
    defp next_hangul_l(head, << unquote(codepoint), rest :: binary >>) do
      next_hangul_l(head <> unquote(codepoint), rest)
    end
  end

  lc codepoint inlist cluster["LV"] do
    defp next_hangul_l(head, << unquote(codepoint), rest :: binary >>) do
      next_hangul_v(head <> unquote(codepoint), rest)
    end
  end

  lc codepoint inlist cluster["LVT"] do
    defp next_hangul_l(head, << unquote(codepoint), rest :: binary >>) do
      next_hangul_t(head <> unquote(codepoint), rest)
    end
  end

  defp next_hangul_l(head, tail) do
    next_hangul_v(head, tail)
  end

  # Handle Hangul V
  lc codepoint inlist cluster["V"] do
    defp next_hangul_v(head, << unquote(codepoint), rest :: binary >>) do
      next_hangul_v(head <> unquote(codepoint), rest)
    end
  end

  defp next_hangul_v(head, tail) do
    next_hangul_t(head, tail)
  end

  # Handle Hangul T
  lc codepoint inlist cluster["T"] do
    defp next_hangul_t(head, << unquote(codepoint), rest :: binary >>) do
      next_hangul_t(head <> unquote(codepoint), rest)
    end
  end

  defp next_hangul_t(head, tail) do
    next_extend(head, tail)
  end

  # Handle regional
  lc codepoint inlist cluster["Regional_Indicator"] do
    defp next_regional(head, << unquote(codepoint), rest :: binary >>) do
      next_regional(head <> unquote(codepoint), rest)
    end
  end

  defp next_regional(head, tail) do
    next_extend(head, tail)
  end

  # Handle Extend+SpacingMark
  lc codepoint inlist cluster["Extend"] ++ cluster["SpacingMark"]  do
    defp next_extend(head, << unquote(codepoint), rest :: binary >>) do
      next_extend(head <> unquote(codepoint), rest)
    end
  end

  defp next_extend(head, tail) do
    { head, tail }
  end

  # Handle Prepend
  # lc codepoint inlist cluster["Prepend"] do
  #   defp next_prepend(<< unquote(codepoint), rest :: binary >>) do
  #     next_prepend(head <> unquote(codepoint), rest)
  #   end
  # end
  #
  # defp next_prepend(head, tail) do
  #   { head, tail }
  # end

  def graphemes(binary) when is_binary(binary) do
    do_graphemes(next_grapheme(binary))
  end

  defp do_graphemes({ c, rest }) do
    [c|do_graphemes(next_grapheme(rest))]
  end

  defp do_graphemes(:no_grapheme) do
    []
  end
end
