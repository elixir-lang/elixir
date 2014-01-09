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
    def next_grapheme(<< unquote(codepoint), rest :: binary >> = string) do
      { :binary.part(string, 0, unquote(size(codepoint))), rest }
    end
  end

  # Break on Prepend*
  # lc codepoint inlist cluster["Prepend"] do
  #   def next_grapheme(<< unquote(codepoint), rest :: binary >> = string) do
  #     next_prepend(rest, string, unquote(size(codepoint)))
  #   end
  # end

  # Handle Hangul L
  lc codepoint inlist cluster["L"] do
    def next_grapheme(<< unquote(codepoint), rest :: binary >> = string) do
      next_hangul_l(rest, string, unquote(size(codepoint)))
    end
  end

  # Handle Hangul T
  lc codepoint inlist cluster["T"] do
    def next_grapheme(<< unquote(codepoint), rest :: binary >> = string) do
      next_hangul_t(rest, string, unquote(size(codepoint)))
    end
  end

  # Handle Regional
  lc codepoint inlist cluster["Regional_Indicator"] do
    def next_grapheme(<< unquote(codepoint), rest :: binary >> = string) do
      next_regional(rest, string, unquote(size(codepoint)))
    end
  end

  # Handle extended entries
  def next_grapheme(<< cp :: utf8, rest :: binary >> = string) do
    next_extend(rest, string, byte_size(<< cp :: utf8 >>))
  end

  def next_grapheme(<< cp, rest :: binary >>) do
    { <<cp>>, rest }
  end

  def next_grapheme(<<>>) do
    :no_grapheme
  end

  # Handle Hangul L
  lc codepoint inlist cluster["L"] do
    defp next_hangul_l(<< unquote(codepoint), rest :: binary >>, string, size) do
      next_hangul_l(rest, string, size + unquote(size(codepoint)))
    end
  end

  lc codepoint inlist cluster["LV"] do
    defp next_hangul_l(<< unquote(codepoint), rest :: binary >>, string, size) do
      next_hangul_v(rest, string, size + unquote(size(codepoint)))
    end
  end

  lc codepoint inlist cluster["LVT"] do
    defp next_hangul_l(<< unquote(codepoint), rest :: binary >>, string, size) do
      next_hangul_t(rest, string, size + unquote(size(codepoint)))
    end
  end

  defp next_hangul_l(rest, string, size) do
    next_hangul_v(rest, string, size)
  end

  # Handle Hangul V
  lc codepoint inlist cluster["V"] do
    defp next_hangul_v(<< unquote(codepoint), rest :: binary >>, string, size) do
      next_hangul_v(rest, string, size + unquote(size(codepoint)))
    end
  end

  defp next_hangul_v(rest, string, size) do
    next_hangul_t(rest, string, size)
  end

  # Handle Hangul T
  lc codepoint inlist cluster["T"] do
    defp next_hangul_t(<< unquote(codepoint), rest :: binary >>, string, size) do
      next_hangul_t(rest, string, size + unquote(size(codepoint)))
    end
  end

  defp next_hangul_t(rest, string, size) do
    next_extend(rest, string, size)
  end

  # Handle regional
  lc codepoint inlist cluster["Regional_Indicator"] do
    defp next_regional(<< unquote(codepoint), rest :: binary >>, string, size) do
      next_regional(rest, string, size + unquote(size(codepoint)))
    end
  end

  defp next_regional(rest, string, size) do
    next_extend(rest, string, size)
  end

  # Handle Extend+SpacingMark
  lc codepoint inlist cluster["Extend"] ++ cluster["SpacingMark"]  do
    defp next_extend(<< unquote(codepoint), rest :: binary >>, string, size) do
      next_extend(rest, string, size + unquote(size(codepoint)))
    end
  end

  defp next_extend(rest, string, size) do
    { :binary.part(string, 0, size), rest }
  end

  # Handle Prepend
  # lc codepoint inlist cluster["Prepend"] do
  #   defp next_prepend(<< unquote(codepoint), rest :: binary >>, string, size) do
  #     next_prepend(rest, string, size + unquote(size(codepoint)))
  #   end
  # end
  #
  # defp next_prepend(rest, string, size) do
  #   { :binary.part(string, 0, size), rest }
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
