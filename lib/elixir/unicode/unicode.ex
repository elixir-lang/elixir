to_binary = fn
  "" ->
    nil
  codepoints ->
    codepoints = :binary.split(codepoints, " ", [:global])
    Enum.reduce codepoints, "", fn(codepoint, acc) ->
      acc <> <<String.to_integer(codepoint, 16)::utf8>>
    end
end

defmodule String.Unicode do
  @moduledoc false
  def version, do: {8, 0, 0}

  # WhiteSpace.txt is extracted from Unicode's PropList.txt (just the White_Space property)
  prop_path = Path.join(__DIR__, "WhiteSpace.txt")

  whitespace = Enum.reduce File.stream!(prop_path), [], fn(line, acc) ->
    case line |> :binary.split(";") |> hd do
      <<first::4-bytes, "..", last::4-bytes, _::binary>> ->
        first = String.to_integer(first, 16)
        last = String.to_integer(last, 16)
        Enum.map(first..last, fn int -> <<int::utf8>> end) ++ acc
      <<single::4-bytes, _::binary>> ->
        [<<String.to_integer(single, 16)::utf8>> | acc]
    end
  end

  data_path = Path.join(__DIR__, "UnicodeData.txt")

  {codes, breakable_whitespace} = Enum.reduce File.stream!(data_path), {[], whitespace}, fn(line, {cacc, wacc}) ->
    [codepoint, _name, _category,
     _class, _bidi, decomposition,
     _numeric_1, _numeric_2, _numeric_3,
     _bidi_mirror, _unicode_1, _iso,
     upper, lower, title] = :binary.split(line, ";", [:global])

    title = :binary.part(title, 0, byte_size(title) - 1)

    cond do
      upper != "" or lower != "" or title != "" ->
        {[{to_binary.(codepoint),
           to_binary.(upper),
           to_binary.(lower),
           to_binary.(title)} | cacc],
         wacc}
      String.starts_with?(decomposition, "<noBreak>") ->
        {cacc, List.delete(wacc, to_binary.(codepoint))}
      true ->
        {cacc, wacc}
    end
  end

  special_path = Path.join(__DIR__, "SpecialCasing.txt")

  codes = Enum.reduce File.stream!(special_path), codes, fn(line, acc) ->
    [codepoint, lower, title, upper, _] = :binary.split(line, "; ", [:global])
    key = to_binary.(codepoint)
    :lists.keystore(key, 1, acc, {key,
                                  to_binary.(upper),
                                  to_binary.(lower),
                                  to_binary.(title)})
  end

  # Downcase

  def downcase(string), do: downcase(string, "")

  for {codepoint, _upper, lower, _title} <- codes, lower && lower != codepoint do
    defp downcase(unquote(codepoint) <> rest, acc) do
      downcase(rest, acc <> unquote(lower))
    end
  end

  defp downcase(<<char, rest::binary>>, acc) do
    downcase(rest, <<acc::binary, char>>)
  end

  defp downcase("", acc), do: acc

  # Upcase

  def upcase(string), do: upcase(string, "")

  for {codepoint, upper, _lower, _title} <- codes, upper && upper != codepoint do
    defp upcase(unquote(codepoint) <> rest, acc) do
      upcase(rest, acc <> unquote(upper))
    end
  end

  defp upcase(<<char, rest::binary>>, acc) do
    upcase(rest, <<acc::binary, char>>)
  end

  defp upcase("", acc), do: acc

  # Titlecase once

  def titlecase_once(""), do: {"", ""}

  for {codepoint, _upper, _lower, title} <- codes, title && title != codepoint do
    def titlecase_once(unquote(codepoint) <> rest) do
      {unquote(title), rest}
    end
  end

  def titlecase_once(<<char, rest::binary>>) do
    {<<char>>, rest}
  end

  # Strip

  def lstrip(string)

  def lstrip(""), do: ""

  for codepoint <- whitespace do
    def lstrip(unquote(codepoint) <> rest) do
      lstrip(rest)
    end
  end

  def lstrip(string) when is_binary(string), do: string

  @whitespace_max_size 3
  for codepoint <- whitespace do
    # We need to increment @whitespace_max_size as well
    # as the small table (_s) if we add a new entry here.
    case byte_size(codepoint) do
      3 ->
        defp do_rstrip_l(unquote(codepoint)), do: -3
      2 ->
        defp do_rstrip_l(<<_, unquote(codepoint)>>), do: -2

        defp do_rstrip_s(unquote(codepoint)), do: <<>>
      1 ->
        defp do_rstrip_l(<<unquote(codepoint), unquote(codepoint), unquote(codepoint)>>), do: -3
        defp do_rstrip_l(<<_, unquote(codepoint), unquote(codepoint)>>), do: -2
        defp do_rstrip_l(<<_, _, unquote(codepoint)>>), do: -1

        defp do_rstrip_s(<<x, unquote(codepoint)>>), do: do_rstrip_s(<<x>>)
        defp do_rstrip_s(unquote(codepoint)), do: <<>>
    end
  end

  defp do_rstrip_l(_), do: 0
  defp do_rstrip_s(o), do: o

  def rstrip(string) when is_binary(string) do
    rstrip(string, byte_size(string))
  end

  defp rstrip(string, size) when size < @whitespace_max_size do
    do_rstrip_s(string)
  end

  defp rstrip(string, size) do
    trail = binary_part(string, size, -@whitespace_max_size)
    case do_rstrip_l(trail) do
      0 -> string
      x -> rstrip(binary_part(string, 0, size + x), size + x)
    end
  end

  # Split

  def split(""), do: []

  def split(string) when is_binary(string) do
    :lists.reverse do_split(string, "", [])
  end

  for codepoint <- breakable_whitespace do
    defp do_split(unquote(codepoint) <> rest, buffer, acc) do
      do_split(rest, "", add_buffer_to_acc(buffer, acc))
    end
  end

  defp do_split(<<char, rest::binary>>, buffer, acc) do
    do_split(rest, <<buffer::binary, char>>, acc)
  end

  defp do_split(<<>>, buffer, acc) do
    add_buffer_to_acc(buffer, acc)
  end

  @compile {:inline, add_buffer_to_acc: 2}

  defp add_buffer_to_acc("", acc),     do: acc
  defp add_buffer_to_acc(buffer, acc), do: [buffer|acc]

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
    [c|do_codepoints(next_codepoint(rest))]
  end

  defp do_codepoints(nil) do
    []
  end
end

defmodule String.Graphemes do
  @moduledoc false

  cluster_path = Path.join(__DIR__, "GraphemeBreakProperty.txt")
  regex = ~r/(?:^([0-9A-F]+)(?:\.\.([0-9A-F]+))?)\s+;\s(\w+)/m

  cluster = Enum.reduce File.stream!(cluster_path), %{}, fn(line, dict) ->
    [_full, first, last, class] = Regex.run(regex, line)

    codepoints =
      case {first, last} do
        {"D800", "DFFF"} ->
          []
        {first, ""} ->
          [<<String.to_integer(first, 16)::utf8>>]
        {first, last} ->
          range = String.to_integer(first, 16)..String.to_integer(last, 16)
          Enum.map(range, fn int -> <<int::utf8>> end)
      end

    Map.update(dict, class, codepoints, &(&1 ++ codepoints))
  end

  # There is no codepoint marked as Prepend by Unicode 6.3.0
  if cluster["Prepend"] do
    raise "it seems this new unicode version has added Prepend items. " <>
          "Please remove this error and uncomment the code below"
  end

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
  # for codepoint <- cluster["Prepend"] do
  #   def next_grapheme_size(<<unquote(codepoint), rest::binary>>) do
  #     next_prepend_size(rest, unquote(byte_size(codepoint)))
  #   end
  # end

  # Handle Hangul L
  for codepoint <- cluster["L"] do
    def next_grapheme_size(<<unquote(codepoint), rest::binary>>) do
      next_hangul_l_size(rest, unquote(byte_size(codepoint)))
    end
  end

  # Handle Hangul T
  for codepoint <- cluster["T"] do
    def next_grapheme_size(<<unquote(codepoint), rest::binary>>) do
      next_hangul_t_size(rest, unquote(byte_size(codepoint)))
    end
  end

  # Handle Regional
  for codepoint <- cluster["Regional_Indicator"] do
    def next_grapheme_size(<<unquote(codepoint), rest::binary>>) do
      next_regional_size(rest, unquote(byte_size(codepoint)))
    end
  end

  # Handle extended entries

  def next_grapheme_size(<<cp::utf8, rest::binary>>) do
    case cp do
      x when x <= 0x007F -> next_extend_size(rest, 1)
      x when x <= 0x07FF -> next_extend_size(rest, 2)
      x when x <= 0xFFFF -> next_extend_size(rest, 3)
      _                  -> next_extend_size(rest, 4)
    end
  end

  def next_grapheme_size(<<_, rest::binary>>) do
    {1, rest}
  end

  def next_grapheme_size(<<>>) do
    nil
  end

  # Handle Hangul L
  for codepoint <- cluster["L"] do
    defp next_hangul_l_size(<<unquote(codepoint), rest::binary>>, size) do
      next_hangul_l_size(rest, size + unquote(byte_size(codepoint)))
    end
  end

  for codepoint <- cluster["LV"] do
    defp next_hangul_l_size(<<unquote(codepoint), rest::binary>>, size) do
      next_hangul_v_size(rest, size + unquote(byte_size(codepoint)))
    end
  end

  for codepoint <- cluster["LVT"] do
    defp next_hangul_l_size(<<unquote(codepoint), rest::binary>>, size) do
      next_hangul_t_size(rest, size + unquote(byte_size(codepoint)))
    end
  end

  defp next_hangul_l_size(rest, size) do
    next_hangul_v_size(rest, size)
  end

  # Handle Hangul V
  for codepoint <- cluster["V"] do
    defp next_hangul_v_size(<<unquote(codepoint), rest::binary>>, size) do
      next_hangul_v_size(rest, size + unquote(byte_size(codepoint)))
    end
  end

  defp next_hangul_v_size(rest, size) do
    next_hangul_t_size(rest, size)
  end

  # Handle Hangul T
  for codepoint <- cluster["T"] do
    defp next_hangul_t_size(<<unquote(codepoint), rest::binary>>, size) do
      next_hangul_t_size(rest, size + unquote(byte_size(codepoint)))
    end
  end

  defp next_hangul_t_size(rest, size) do
    next_extend_size(rest, size)
  end

  # Handle regional
  for codepoint <- cluster["Regional_Indicator"] do
    defp next_regional_size(<<unquote(codepoint), rest::binary>>, size) do
      next_regional_size(rest, size + unquote(byte_size(codepoint)))
    end
  end

  defp next_regional_size(rest, size) do
    next_extend_size(rest, size)
  end

  # Handle Extend+SpacingMark
  for codepoint <- cluster["Extend"] ++ cluster["SpacingMark"]  do
    defp next_extend_size(<<unquote(codepoint), rest::binary>>, size) do
      next_extend_size(rest, size + unquote(byte_size(codepoint)))
    end
  end

  defp next_extend_size(rest, size) do
    {size, rest}
  end

  # Handle Prepend
  # for codepoint <- cluster["Prepend"] do
  #   defp next_prepend_size(<<unquote(codepoint), rest::binary>>, size) do
  #     next_prepend_size(rest, size + unquote(byte_size(codepoint)))
  #   end
  # end
  #
  # defp next_prepend_size(rest, size) do
  #   {size, rest}
  # end

  ## Tight-loop implementations

  def graphemes(binary) when is_binary(binary) do
    do_graphemes(next_grapheme_size(binary), binary)
  end

  defp do_graphemes({size, rest}, binary) do
    [:binary.part(binary, 0, size)|do_graphemes(next_grapheme_size(rest), rest)]
  end

  defp do_graphemes(nil, _) do
    []
  end

  def length(string) do
    do_length(next_grapheme_size(string), 0)
  end

  defp do_length({_, rest}, acc) do
    do_length(next_grapheme_size(rest), acc + 1)
  end

  defp do_length(nil, acc), do: acc

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
end

defmodule String.Normalizer do
  @moduledoc false

  decomposition_path = Path.join(__DIR__, "Decomposition.txt")

  decompositions = Enum.reduce File.stream!(decomposition_path), [], fn(line, acc) ->
    [key, first, second, third, fourth, _] = :binary.split(line, ";", [:global])
    decomposition = to_binary.(first) <> (to_binary.(second) || "") <>
                                         (to_binary.(third)  || "") <>
                                         (to_binary.(fourth) || "")
    [{to_binary.(key), decomposition} | acc]
  end

  composition_path = Path.join(__DIR__, "Composition.txt")

  compositions = Enum.reduce File.stream!(composition_path), [], fn(line, acc) ->
    [first, second, composition, _] = :binary.split(line, ";", [:global])
    key = to_binary.(first) <> to_binary.(second)
    [{key, to_binary.(composition)} | acc]
  end

  combining_class_path = Path.join(__DIR__, "CombiningClasses.txt")

  combining_classes = Enum.reduce File.stream!(combining_class_path), [], fn(line, acc) ->
    [codepoint, class, _] = :binary.split(line, ";", [:global])
    [{String.to_integer(codepoint, 16), class} | acc]
  end

  # Normalize

  def normalize(string, :nfd) when is_binary(string) do
    normalize_nfd(string, "")
  end

  def normalize(string, :nfc) when is_binary(string) do
    normalize_nfc(string, "")
  end

  defp normalize_nfd("", acc), do: acc

  defp normalize_nfd(<<cp::utf8, rest::binary>>, acc) when cp in 0xAC00..0xD7A3 do
    {syllable_index, t_count, n_count} = {cp - 0xAC00, 28, 588}
    lead  = 0x1100 + div(syllable_index, n_count)
    vowel = 0x1161 + div(rem(syllable_index, n_count), t_count)
    trail = 0x11A7 + rem(syllable_index, t_count)
    binary =
      if trail == 0x11A7 do
        <<lead::utf8, vowel::utf8>>
      else
        <<lead::utf8, vowel::utf8, trail::utf8>>
      end
    normalize_nfd(rest, acc <> binary)
  end

  for {binary, decomposition} <- decompositions do
    defp normalize_nfd(unquote(binary) <> rest, acc) do
      normalize_nfd(rest, acc <> unquote(decomposition))
    end
  end

  defp normalize_nfd(binary, acc) do
    {n, rest} = String.Graphemes.next_grapheme_size(binary)
    part = :binary.part(binary, 0, n)
    case n do
      1 -> normalize_nfd(rest, acc <> part)
      _ -> normalize_nfd(rest, acc <> canonical_order(part))
    end
  end

  defp normalize_nfc("", acc), do: acc

  defp normalize_nfc(<<cp::utf8, rest::binary>>, acc) when cp in 0xAC00..0xD7A3 do
    normalize_nfc(rest, acc <> <<cp::utf8>>)
  end

  defp normalize_nfc(binary, acc) do
    {n, rest} = String.Graphemes.next_grapheme_size(binary)
    part = :binary.part(binary, 0, n)
    case n do
      1 -> normalize_nfc(rest, acc <> part)
      _ -> normalize_nfc(rest, acc <> compose(normalize_nfd(part, "")))
    end
  end

  defp canonical_order(binary) do
    binary
    |> :unicode.characters_to_list()
    |> Enum.sort_by(&combining_class/1)
    |> :unicode.characters_to_binary()
  end

  for {codepoint, class} <- combining_classes do
    defp combining_class(unquote(codepoint)), do: unquote(class)
  end

  defp combining_class(_), do: 0

  defp compose(<<_::utf8>> = binary), do: binary

  defp compose(<<lead::utf8, vowel::utf8, rest::binary>>) when lead in 0x1100..0x1112 and vowel in 0x1161..0x1175 do
    codepoint = 0xAC00 + ((lead - 0x1100) * 588) + ((vowel - 0x1161) * 28)
    case rest do
      <<trail::utf8, accents::binary>> when trail in 0x11A7..0x11C2 ->
        <<codepoint + trail - 0x11A7::utf8, accents::binary>>
      _ ->
        <<codepoint::utf8, rest::binary>>
    end
  end

  for {binary, composition} <- compositions do
    defp compose(unquote(binary)), do: unquote(composition)
  end

  defp compose(<<cp::utf8, rest::binary>>) do
    compose(rest, <<cp::utf8>>, "", combining_class(cp) - 1)
  end

  defp compose("", base, accents, _), do: base <> accents

  defp compose(<<cp::utf8, rest::binary>>, base, accents, last_class) do
    part_class = combining_class(cp)
    combined = <<base::binary, cp::utf8>>
    if last_class < part_class and composable?(combined) do
      compose(rest, compose(combined), accents, last_class)
    else
      compose(rest, base, <<accents::binary, cp::utf8>>, part_class)
    end
  end

  for {binary, _} <- compositions do
    defp composable?(unquote(binary)), do: true
  end

  defp composable?(_), do: false
end
