defmodule String.Unicode do
  @moduledoc false
  def version, do: {7, 0, 0}

  to_binary = fn
    "" ->
      nil
    codepoints ->
      codepoints = :binary.split(codepoints, " ", [:global])
      Enum.reduce codepoints, "", fn(codepoint, acc) ->
        acc <> << String.to_integer(codepoint, 16) :: utf8 >>
      end
  end

  data_path = Path.join(__DIR__, "UnicodeData.txt")

  {codes, whitespace} = Enum.reduce File.stream!(data_path), {[], []}, fn(line, {cacc, wacc}) ->
    [codepoint, _name, _category,
     _class, bidi, _decomposition,
     _numeric_1, _numeric_2, _numeric_3,
     _bidi_mirror, _unicode_1, _iso,
     upper, lower, title] = :binary.split(line, ";", [:global])

    title = :binary.part(title, 0, byte_size(title) - 1)

    cond do
      upper != "" or lower != "" or title != "" ->
        {[{to_binary.(codepoint), to_binary.(upper), to_binary.(lower), to_binary.(title)} | cacc], wacc}
      bidi in ["B", "S", "WS"] ->
        {cacc, [to_binary.(codepoint) | wacc]}
      true ->
        {cacc, wacc}
    end
  end

  special_path = Path.join(__DIR__, "SpecialCasing.txt")

  codes = Enum.reduce File.stream!(special_path), codes, fn(line, acc) ->
    [codepoint, lower, title, upper, _comment] = :binary.split(line, "; ", [:global])
    key = to_binary.(codepoint)
    :lists.keystore(key, 1, acc, {key, to_binary.(upper), to_binary.(lower), to_binary.(title)})
  end

  # Downcase

  def downcase(string), do: downcase(string, "")

  for {codepoint, _upper, lower, _title} <- codes, lower && lower != codepoint do
    defp downcase(unquote(codepoint) <> rest, acc) do
      downcase(rest, acc <> unquote(lower))
    end
  end

  defp downcase(<<char, rest :: binary>>, acc) do
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

  defp upcase(<<char, rest :: binary>>, acc) do
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

  def titlecase_once(<< char, rest :: binary >>) do
    {<< char >>, rest}
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

  for codepoint <- whitespace do
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

  @compile {:inline, add_buffer_to_acc: 2}

  defp add_buffer_to_acc("", acc),     do: acc
  defp add_buffer_to_acc(buffer, acc), do: [buffer|acc]

  # Codepoints

  def next_codepoint(<< cp :: utf8, rest :: binary >>) do
    {<<cp :: utf8>>, rest}
  end

  def next_codepoint(<< cp, rest :: binary >>) do
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

  to_range = fn
    first, ""   ->
      [<< String.to_integer(first, 16) :: utf8 >>]
    first, last ->
      range = String.to_integer(first, 16)..String.to_integer(last, 16)
      Enum.map(range, fn(int) -> << int :: utf8 >> end)
  end

  cluster = Enum.reduce File.stream!(cluster_path), HashDict.new, fn(line, dict) ->
    [_full, first, last, class] = Regex.run(regex, line)

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
    raise "it seems this new unicode version has added Prepend items. " <>
          "Please remove this error and uncomment the code below"
  end

  # Don't break CRLF
  def next_grapheme_size(<<?\r, ?\n, rest :: binary>>) do
    {2, rest}
  end

  # Break on control
  for codepoint <- cluster["CR"] ++ cluster["LF"] ++ cluster["Control"] do
    def next_grapheme_size(<<unquote(codepoint), rest :: binary>>) do
      {unquote(byte_size(codepoint)), rest}
    end
  end

  # Break on Prepend*
  # for codepoint <- cluster["Prepend"] do
  #   def next_grapheme_size(<<unquote(codepoint), rest :: binary>>) do
  #     next_prepend_size(rest, unquote(byte_size(codepoint)))
  #   end
  # end

  # Handle Hangul L
  for codepoint <- cluster["L"] do
    def next_grapheme_size(<<unquote(codepoint), rest :: binary>>) do
      next_hangul_l_size(rest, unquote(byte_size(codepoint)))
    end
  end

  # Handle Hangul T
  for codepoint <- cluster["T"] do
    def next_grapheme_size(<<unquote(codepoint), rest :: binary>>) do
      next_hangul_t_size(rest, unquote(byte_size(codepoint)))
    end
  end

  # Handle Regional
  for codepoint <- cluster["Regional_Indicator"] do
    def next_grapheme_size(<<unquote(codepoint), rest :: binary>>) do
      next_regional_size(rest, unquote(byte_size(codepoint)))
    end
  end

  # Handle extended entries

  def next_grapheme_size(<<cp :: utf8, rest :: binary>>) do
    case cp do
      x when x <= 0x007F -> next_extend_size(rest, 1)
      x when x <= 0x07FF -> next_extend_size(rest, 2)
      x when x <= 0xFFFF -> next_extend_size(rest, 3)
      _                  -> next_extend_size(rest, 4)
    end
  end

  def next_grapheme_size(<<_, rest :: binary>>) do
    {1, rest}
  end

  def next_grapheme_size(<<>>) do
    nil
  end

  # Handle Hangul L
  for codepoint <- cluster["L"] do
    defp next_hangul_l_size(<<unquote(codepoint), rest :: binary>>, size) do
      next_hangul_l_size(rest, size + unquote(byte_size(codepoint)))
    end
  end

  for codepoint <- cluster["LV"] do
    defp next_hangul_l_size(<<unquote(codepoint), rest :: binary>>, size) do
      next_hangul_v_size(rest, size + unquote(byte_size(codepoint)))
    end
  end

  for codepoint <- cluster["LVT"] do
    defp next_hangul_l_size(<<unquote(codepoint), rest :: binary>>, size) do
      next_hangul_t_size(rest, size + unquote(byte_size(codepoint)))
    end
  end

  defp next_hangul_l_size(rest, size) do
    next_hangul_v_size(rest, size)
  end

  # Handle Hangul V
  for codepoint <- cluster["V"] do
    defp next_hangul_v_size(<<unquote(codepoint), rest :: binary>>, size) do
      next_hangul_v_size(rest, size + unquote(byte_size(codepoint)))
    end
  end

  defp next_hangul_v_size(rest, size) do
    next_hangul_t_size(rest, size)
  end

  # Handle Hangul T
  for codepoint <- cluster["T"] do
    defp next_hangul_t_size(<<unquote(codepoint), rest :: binary>>, size) do
      next_hangul_t_size(rest, size + unquote(byte_size(codepoint)))
    end
  end

  defp next_hangul_t_size(rest, size) do
    next_extend_size(rest, size)
  end

  # Handle regional
  for codepoint <- cluster["Regional_Indicator"] do
    defp next_regional_size(<<unquote(codepoint), rest :: binary>>, size) do
      next_regional_size(rest, size + unquote(byte_size(codepoint)))
    end
  end

  defp next_regional_size(rest, size) do
    next_extend_size(rest, size)
  end

  # Handle Extend+SpacingMark
  for codepoint <- cluster["Extend"] ++ cluster["SpacingMark"]  do
    defp next_extend_size(<<unquote(codepoint), rest :: binary>>, size) do
      next_extend_size(rest, size + unquote(byte_size(codepoint)))
    end
  end

  defp next_extend_size(rest, size) do
    {size, rest}
  end

  # Handle Prepend
  # for codepoint <- cluster["Prepend"] do
  #   defp next_prepend_size(<<unquote(codepoint), rest :: binary>>, size) do
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
