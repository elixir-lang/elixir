data_path = Path.join(__DIR__, "UnicodeData.txt")

to_binary = fn
  "" ->
    nil

  codepoints ->
    codepoints
    |> :binary.split(" ", [:global])
    |> Enum.map(&<<String.to_integer(&1, 16)::utf8>>)
    |> IO.iodata_to_binary()
end

rangify = fn [head | tail] ->
  {first, last, acc} =
    Enum.reduce(tail, {head, head, []}, fn
      number, {first, last, acc} when number == first - 1 ->
        {number, last, acc}

      number, {first, last, acc} ->
        {number, number, [{first, last} | acc]}
    end)

  [{first, last} | acc]
end

# A character is case ignorable if:
#
#    Word_Break(C) = MidLetter or MidNumLet or Single_Quote, or
#    General_Category(C) = Nonspacing_Mark (Mn), Enclosing_Mark (Me), Format (Cf),
#                          Modifier_Letter (Lm), or Modifier_Symbol (Sk).
#
# Word breaks are defined below based on TR29 (http://unicode.org/reports/tr29/).
# The categories are computed later.
case_ignorable = [
  0x0027,
  0x002E,
  0x2018,
  0x2019,
  0x2024,
  0xFE52,
  0xFF07,
  0xFF0E,
  0x00B7,
  0x0387,
  0x05F4,
  0x2027,
  0x003A,
  0xFE13,
  0xFE55,
  0xFF1A
]

acc = {[], [], case_ignorable, [], %{}, %{}}
cased_letter_categories = :binary.compile_pattern(["Ll", "Lt", "Lu"])
case_ignorable_categories = :binary.compile_pattern(["Mn", "Me", "Cf", "Lm", "Sk"])

{codes, cased_letters, case_ignorable, non_breakable, decompositions, combining_classes} =
  Enum.reduce(File.stream!(data_path), acc, fn line, {cacc, lacc, iacc, wacc, dacc, kacc} ->
    [
      codepoint,
      _name,
      category,
      class,
      _bidi,
      decomposition,
      _numeric_1,
      _numeric_2,
      _numeric_3,
      _bidi_mirror,
      _unicode_1,
      _iso,
      upper,
      lower,
      title
    ] = :binary.split(line, ";", [:global])

    title = binary_part(title, 0, byte_size(title) - 1)

    cacc =
      if upper != "" or lower != "" or title != "" do
        [{to_binary.(codepoint), to_binary.(upper), to_binary.(lower), to_binary.(title)} | cacc]
      else
        cacc
      end

    cased_letter_categories = :binary.compile_pattern(["Ll", "Lt", "Lu"])
    case_ignorable_categories = :binary.compile_pattern(["Mn", "Me", "Cf", "Lm", "Sk"])

    {lacc, iacc} =
      cond do
        match?({0, _}, :binary.match(category, cased_letter_categories)) ->
          {[String.to_integer(codepoint, 16) | lacc], iacc}

        match?({0, _}, :binary.match(category, case_ignorable_categories)) ->
          {lacc, [String.to_integer(codepoint, 16) | iacc]}

        true ->
          {lacc, iacc}
      end

    wacc =
      case decomposition do
        "<noBreak>" <> _ -> [to_binary.(codepoint) | wacc]
        _ -> wacc
      end

    dacc =
      case decomposition do
        # Decomposition
        <<h, _::binary>> when h != ?< ->
          decomposition =
            decomposition
            |> :binary.split(" ", [:global])
            |> Enum.map(&String.to_integer(&1, 16))

          Map.put(dacc, String.to_integer(codepoint, 16), decomposition)

        _ ->
          dacc
      end

    kacc =
      case Integer.parse(class) do
        {0, ""} -> kacc
        {n, ""} -> Map.put(kacc, String.to_integer(codepoint, 16), n)
      end

    {cacc, lacc, iacc, wacc, dacc, kacc}
  end)

defmodule String.Casing do
  @moduledoc false

  special_path = Path.join(__DIR__, "SpecialCasing.txt")

  codes =
    Enum.reduce(File.stream!(special_path), codes, fn line, acc ->
      if String.starts_with?(line, ["#", "\n"]) do
        acc
      else
        [codepoint, lower, title, upper, _] = :binary.split(line, "; ", [:global])
        key = to_binary.(codepoint)

        :lists.keystore(
          key,
          1,
          acc,
          {key, to_binary.(upper), to_binary.(lower), to_binary.(title)}
        )
      end
    end)

  # Guards

  [lowers, uppers] =
    codes
    |> Enum.reduce([[], []], fn
      {_codepoint, upper, lower, _title}, [lowers, uppers] ->
        [[lower | lowers], [upper | uppers]]
    end)
    |> Enum.map(&MapSet.new/1)
    |> Enum.map(&MapSet.delete(&1, nil))
    |> Enum.map(&MapSet.to_list/1)
    |> Enum.map(&Enum.chunk_every(&1, 700))
    |> Enum.map(fn chunks ->
      for chunk <- chunks do
        {:in, [context: Elixir, import: Kernel], [{:value, [], nil}, chunk]}
      end
    end)

  defmodule Guards do
    defguard is_lowercase(value) when unquote({:or, [context: Elixir, import: Kernel], lowers})
    defguard is_uppercase(value) when unquote({:or, [context: Elixir, import: Kernel], uppers})
  end

  # Downcase

  @conditional_downcase [
    sigma = <<0x03A3::utf8>>
  ]

  def downcase(<<unquote(sigma), rest::bits>>, acc, mode) do
    downcased =
      if mode == :greek and cased_letter_list?(acc) and not cased_letter_binary?(rest) do
        <<0x03C2::utf8>>
      else
        <<0x03C3::utf8>>
      end

    downcase(rest, [downcased | acc], mode)
  end

  for {codepoint, _upper, lower, _title} <- codes,
      lower && lower != codepoint,
      codepoint not in @conditional_downcase do
    def downcase(<<unquote(codepoint), rest::bits>>, acc, mode) do
      downcase(rest, [unquote(lower) | acc], mode)
    end
  end

  def downcase(<<char, rest::bits>>, acc, mode) do
    downcase(rest, [<<char>> | acc], mode)
  end

  def downcase("", acc, _mode), do: IO.iodata_to_binary(:lists.reverse(acc))

  # Sigma handling

  defp cased_letter_binary?(<<codepoint::utf8, rest::bits>>) do
    if case_ignorable?(codepoint) do
      cased_letter_binary?(rest)
    else
      cased_letter?(codepoint)
    end
  end

  defp cased_letter_binary?(_), do: false

  defp cased_letter_list?([<<codepoint::utf8>> | rest]) do
    if case_ignorable?(codepoint) do
      cased_letter_list?(rest)
    else
      cased_letter?(codepoint)
    end
  end

  defp cased_letter_list?(_), do: false

  for {first, last} <- rangify.(cased_letters) do
    if first == last do
      defp cased_letter?(unquote(first)), do: true
    else
      defp cased_letter?(codepoint)
           when codepoint >= unquote(first) and codepoint <= unquote(last),
           do: true
    end
  end

  defp cased_letter?(_), do: false

  for {first, last} <- rangify.(case_ignorable) do
    if first == last do
      defp case_ignorable?(unquote(first)), do: true
    else
      defp case_ignorable?(codepoint)
           when codepoint >= unquote(first) and codepoint <= unquote(last),
           do: true
    end
  end

  defp case_ignorable?(_), do: false

  # Upcase

  for {codepoint, upper, _lower, _title} <- codes, upper && upper != codepoint do
    def upcase(<<unquote(codepoint), rest::bits>>, acc, mode) do
      upcase(rest, [unquote(upper) | acc], mode)
    end
  end

  def upcase(<<char, rest::bits>>, acc, mode) do
    upcase(rest, [char | acc], mode)
  end

  def upcase("", acc, _mode), do: IO.iodata_to_binary(:lists.reverse(acc))

  # Titlecase once

  def titlecase_once("", _mode), do: {"", ""}

  for {codepoint, _upper, _lower, title} <- codes, title && title != codepoint do
    def titlecase_once(unquote(codepoint) <> rest, _mode) do
      {unquote(title), rest}
    end
  end

  def titlecase_once(<<char::utf8, rest::binary>>, _mode) do
    {<<char::utf8>>, rest}
  end

  def titlecase_once(<<char, rest::binary>>, _mode) do
    {<<char>>, rest}
  end
end

defmodule String.Break do
  @moduledoc false
  @whitespace_max_size 3

  prop_path = Path.join(__DIR__, "PropList.txt")

  whitespace =
    Enum.reduce(File.stream!(prop_path), [], fn line, acc ->
      case :binary.split(line, ";") do
        [<<first::4-bytes, "..", last::4-bytes, _::binary>>, <<" White_Space", _::binary>>] ->
          first = String.to_integer(first, 16)
          last = String.to_integer(last, 16)
          Enum.map(first..last, fn int -> <<int::utf8>> end) ++ acc

        [<<single::4-bytes, _::binary>>, <<" White_Space", _::binary>>] ->
          [<<String.to_integer(single, 16)::utf8>> | acc]

        _ ->
          acc
      end
    end)

  # trim_leading

  def trim_leading(string) when is_binary(string) do
    do_trim_leading(string)
  end

  for codepoint <- whitespace do
    def do_trim_leading(<<unquote(codepoint), rest::bits>>), do: do_trim_leading(rest)
  end

  def do_trim_leading(<<rest::bits>>), do: rest

  # trim_trailing

  for cp <- whitespace do
    # We need to increment @whitespace_max_size as well
    # as the small table (_s) if we add a new entry here.
    case byte_size(cp) do
      3 ->
        defp do_trim_trailing_l(unquote(cp)), do: -3

      2 ->
        defp do_trim_trailing_l(<<_, unquote(cp)>>), do: -2
        defp do_trim_trailing_s(unquote(cp)), do: <<>>

      1 ->
        defp do_trim_trailing_l(<<unquote(cp), unquote(cp), unquote(cp)>>), do: -3
        defp do_trim_trailing_l(<<_, unquote(cp), unquote(cp)>>), do: -2
        defp do_trim_trailing_l(<<_, _, unquote(cp)>>), do: -1

        defp do_trim_trailing_s(<<x, unquote(cp)>>), do: do_trim_trailing_s(<<x>>)
        defp do_trim_trailing_s(unquote(cp)), do: <<>>
    end
  end

  defp do_trim_trailing_l(_), do: 0
  defp do_trim_trailing_s(o), do: o

  def trim_trailing(string) when is_binary(string) do
    trim_trailing(string, byte_size(string))
  end

  defp trim_trailing(string, size) when size < @whitespace_max_size do
    do_trim_trailing_s(string)
  end

  defp trim_trailing(string, size) do
    trail = binary_part(string, size, -@whitespace_max_size)

    case do_trim_trailing_l(trail) do
      0 -> string
      x -> trim_trailing(binary_part(string, 0, size + x), size + x)
    end
  end

  # Split

  def split(string) do
    :binary.split(string, unquote(whitespace -- non_breakable), [:global, :trim_all])
  end

  # Decompose

  def decompose(entries, map) do
    for entry <- entries do
      case map do
        %{^entry => match} -> decompose(match, map)
        %{} -> <<entry::utf8>>
      end
    end
  end
end
