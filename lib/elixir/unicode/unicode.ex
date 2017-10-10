# How to update the Unicode files
#
# 1. Update CompositionExclusions.txt by copying original as is
# 2. Update GraphemeBreakProperty.txt by copying original as is
# 3. Update PropList.txt by copying original as is
# 4. Update GraphemeBreakTest.txt by copying original as is
# 5. Update SpecialCasing.txt by removing comments and conditional mappings from original
# 6. Update String.Unicode.version/0 and on String module docs
# 7. make unicode
# 8. elixir lib/elixir/unicode/graphemes_test.exs
#
defmodule String.Unicode do
  @moduledoc false
  def version, do: {10, 0, 0}

  cluster_path = Path.join(__DIR__, "GraphemeBreakProperty.txt")
  regex = ~r/(?:^([0-9A-F]+)(?:\.\.([0-9A-F]+))?)\s+;\s(\w+)/m

  cluster =
    Enum.reduce(File.stream!(cluster_path), %{}, fn line, acc ->
      case Regex.run(regex, line, capture: :all_but_first) do
        ["D800", "DFFF", _class] ->
          acc

        [first, "", class] ->
          codepoint = <<String.to_integer(first, 16)::utf8>>
          Map.update(acc, class, [codepoint], &[<<String.to_integer(first, 16)::utf8>> | &1])

        [first, last, class] ->
          range = String.to_integer(first, 16)..String.to_integer(last, 16)
          codepoints = Enum.map(range, fn int -> <<int::utf8>> end)
          Map.update(acc, class, codepoints, &(codepoints ++ &1))

        nil ->
          acc
      end
    end)

  # Don't break CRLF
  def next_grapheme_size(<<?\r, ?\n, rest::binary>>) do
    {2, rest}
  end

  # Break on control
  for codepoint <- cluster["CR"] ++ cluster["LF"] ++ cluster["Control"] do
    def next_grapheme_size(<<unquote(codepoint), rest::binary>>) do
      {unquote(byte_size(codepoint)), rest}
    end
  end

  # Break on Prepend*
  for codepoint <- cluster["Prepend"] do
    def next_grapheme_size(<<unquote(codepoint), rest::binary>>) do
      next_prepend_size(rest, unquote(byte_size(codepoint)))
    end
  end

  # Handle Regional
  for codepoint <- cluster["Regional_Indicator"] do
    def next_grapheme_size(<<unquote(codepoint), rest::binary>>) do
      next_regional_size(rest, unquote(byte_size(codepoint)))
    end
  end

  # Handle Hangul L
  for codepoint <- cluster["L"] do
    def next_grapheme_size(<<unquote(codepoint), rest::binary>>) do
      next_hangul_l_size(rest, unquote(byte_size(codepoint)))
    end
  end

  # Handle Hangul V
  for codepoint <- cluster["LV"] ++ cluster["V"] do
    def next_grapheme_size(<<unquote(codepoint), rest::binary>>) do
      next_hangul_v_size(rest, unquote(byte_size(codepoint)))
    end
  end

  # Handle Hangul T
  for codepoint <- cluster["LVT"] ++ cluster["T"] do
    def next_grapheme_size(<<unquote(codepoint), rest::binary>>) do
      next_hangul_t_size(rest, unquote(byte_size(codepoint)))
    end
  end

  # Handle E_Base
  for codepoint <- cluster["E_Base"] ++ cluster["E_Base_GAZ"] do
    def next_grapheme_size(<<unquote(codepoint), rest::binary>>) do
      next_extend_size(rest, unquote(byte_size(codepoint)), :e_base)
    end
  end

  # Handle ZWJ
  for codepoint <- cluster["ZWJ"] do
    def next_grapheme_size(<<unquote(codepoint), rest::binary>>) do
      next_extend_size(rest, unquote(byte_size(codepoint)), :zwj)
    end
  end

  # Handle extended entries
  def next_grapheme_size(<<cp::utf8, rest::binary>>) do
    case cp do
      x when x <= 0x007F -> next_extend_size(rest, 1, :other)
      x when x <= 0x07FF -> next_extend_size(rest, 2, :other)
      x when x <= 0xFFFF -> next_extend_size(rest, 3, :other)
      _ -> next_extend_size(rest, 4, :other)
    end
  end

  def next_grapheme_size(<<_, rest::binary>>) do
    {1, rest}
  end

  def next_grapheme_size(<<>>) do
    nil
  end

  # Handle hanguls
  defp next_hangul_l_size(rest, size) do
    case next_hangul(rest, size) do
      {:l, rest, size} -> next_hangul_l_size(rest, size)
      {:v, rest, size} -> next_hangul_v_size(rest, size)
      {:lv, rest, size} -> next_hangul_v_size(rest, size)
      {:lvt, rest, size} -> next_hangul_t_size(rest, size)
      _ -> next_extend_size(rest, size, :other)
    end
  end

  defp next_hangul_v_size(rest, size) do
    case next_hangul(rest, size) do
      {:v, rest, size} -> next_hangul_v_size(rest, size)
      {:t, rest, size} -> next_hangul_t_size(rest, size)
      _ -> next_extend_size(rest, size, :other)
    end
  end

  defp next_hangul_t_size(rest, size) do
    case next_hangul(rest, size) do
      {:t, rest, size} -> next_hangul_t_size(rest, size)
      _ -> next_extend_size(rest, size, :other)
    end
  end

  for codepoint <- cluster["L"] do
    defp next_hangul(<<unquote(codepoint), rest::binary>>, size) do
      {:l, rest, size + unquote(byte_size(codepoint))}
    end
  end

  for codepoint <- cluster["V"] do
    defp next_hangul(<<unquote(codepoint), rest::binary>>, size) do
      {:v, rest, size + unquote(byte_size(codepoint))}
    end
  end

  for codepoint <- cluster["T"] do
    defp next_hangul(<<unquote(codepoint), rest::binary>>, size) do
      {:t, rest, size + unquote(byte_size(codepoint))}
    end
  end

  for codepoint <- cluster["LV"] do
    defp next_hangul(<<unquote(codepoint), rest::binary>>, size) do
      {:lv, rest, size + unquote(byte_size(codepoint))}
    end
  end

  for codepoint <- cluster["LVT"] do
    defp next_hangul(<<unquote(codepoint), rest::binary>>, size) do
      {:lvt, rest, size + unquote(byte_size(codepoint))}
    end
  end

  defp next_hangul(_, _) do
    false
  end

  # Handle regional
  for codepoint <- cluster["Regional_Indicator"] do
    defp next_regional_size(<<unquote(codepoint), rest::binary>>, size) do
      next_extend_size(rest, size + unquote(byte_size(codepoint)), :other)
    end
  end

  defp next_regional_size(rest, size) do
    next_extend_size(rest, size, :other)
  end

  # Handle Extend+SpacingMark+ZWJ
  for codepoint <- cluster["Extend"] do
    defp next_extend_size(<<unquote(codepoint), rest::binary>>, size, marker) do
      next_extend_size(rest, size + unquote(byte_size(codepoint)), keep_ebase(marker))
    end
  end

  for codepoint <- cluster["SpacingMark"] do
    defp next_extend_size(<<unquote(codepoint), rest::binary>>, size, _marker) do
      next_extend_size(rest, size + unquote(byte_size(codepoint)), :other)
    end
  end

  for codepoint <- cluster["ZWJ"] do
    defp next_extend_size(<<unquote(codepoint), rest::binary>>, size, _marker) do
      next_extend_size(rest, size + unquote(byte_size(codepoint)), :zwj)
    end
  end

  for codepoint <- cluster["E_Modifier"] do
    defp next_extend_size(<<unquote(codepoint), rest::binary>>, size, :e_base) do
      next_extend_size(rest, size + unquote(byte_size(codepoint)), :other)
    end
  end

  for codepoint <- cluster["Glue_After_Zwj"] do
    defp next_extend_size(<<unquote(codepoint), rest::binary>>, size, :zwj) do
      next_extend_size(rest, size + unquote(byte_size(codepoint)), :other)
    end
  end

  for codepoint <- cluster["E_Base_GAZ"] do
    defp next_extend_size(<<unquote(codepoint), rest::binary>>, size, :zwj) do
      next_extend_size(rest, size + unquote(byte_size(codepoint)), :e_base)
    end
  end

  defp next_extend_size(rest, size, _) do
    {size, rest}
  end

  defp keep_ebase(:e_base), do: :e_base
  defp keep_ebase(_), do: :other

  # Handle Prepend
  for codepoint <- cluster["Prepend"] do
    defp next_prepend_size(<<unquote(codepoint), rest::binary>>, size) do
      next_prepend_size(rest, size + unquote(byte_size(codepoint)))
    end
  end

  # However, if we see a control character, we have to break it
  for codepoint <- cluster["CR"] ++ cluster["LF"] ++ cluster["Control"] do
    defp next_prepend_size(<<unquote(codepoint), _::binary>> = rest, size) do
      {size, rest}
    end
  end

  defp next_prepend_size(rest, size) do
    case next_grapheme_size(rest) do
      {more, rest} -> {more + size, rest}
      nil -> {size, rest}
    end
  end

  # Graphemes

  def graphemes(binary) when is_binary(binary) do
    do_graphemes(next_grapheme_size(binary), binary)
  end

  defp do_graphemes({size, rest}, binary) do
    [:binary.part(binary, 0, size) | do_graphemes(next_grapheme_size(rest), rest)]
  end

  defp do_graphemes(nil, _) do
    []
  end

  # Length

  def length(string) when is_binary(string) do
    do_length(next_grapheme_size(string), 0)
  end

  defp do_length({_, rest}, acc) do
    do_length(next_grapheme_size(rest), acc + 1)
  end

  defp do_length(nil, acc), do: acc

  # Split at

  def split_at(string, pos) do
    do_split_at(string, 0, pos, 0)
  end

  defp do_split_at(string, acc, desired_pos, current_pos) when desired_pos > current_pos do
    case next_grapheme_size(string) do
      {count, rest} -> do_split_at(rest, acc + count, desired_pos, current_pos + 1)
      nil -> {acc, nil}
    end
  end

  defp do_split_at(string, acc, desired_pos, desired_pos) do
    {acc, string}
  end

  # Codepoints

  def next_codepoint(<<cp::utf8, rest::binary>>) do
    {<<cp::utf8>>, rest}
  end

  def next_codepoint(<<cp, rest::binary>>) do
    {<<cp>>, rest}
  end

  def next_codepoint(<<>>) do
    nil
  end

  def codepoints(binary) when is_binary(binary) do
    do_codepoints(next_codepoint(binary))
  end

  defp do_codepoints({c, rest}) do
    [c | do_codepoints(next_codepoint(rest))]
  end

  defp do_codepoints(nil) do
    []
  end
end
