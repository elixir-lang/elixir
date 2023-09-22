# How to update the Unicode files
#
# Unicode files can be found in https://www.unicode.org/Public/VERSION_NUMBER/ where
# VERSION_NUMBER is the current Unicode version.
#
# 1. Replace UnicodeData.txt by copying original
# 2. Replace PropertyValueAliases.txt by copying original
# 3. Replace PropList.txt by copying original
# 4. Replace ScriptExtensions.txt by copying original
# 5. Replace Scripts.txt by copying original
# 6. Replace SpecialCasing.txt by copying original
# 7. Replace confusables.txt by copying original
#    (from https://www.unicode.org/Public/security/VERSION_NUMBER/)
# 8. Replace IdentifierType.txt by copying original
#    (from https://www.unicode.org/Public/security/VERSION_NUMBER/)
# 9. Update String.Unicode.version/0 and on String module docs (version and link)
# 10. make unicode

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
# Word breaks are defined below based on TR29 (https://unicode.org/reports/tr29/).
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
  data_path
  |> File.read!()
  |> String.split(["\r\n", "\n"], trim: true)
  |> Enum.reduce(acc, fn line, {cacc, lacc, iacc, wacc, dacc, kacc} ->
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
      _title
    ] = :binary.split(line, ";", [:global])

    cacc =
      if upper != "" or lower != "" do
        [{to_binary.(codepoint), to_binary.(upper), to_binary.(lower)} | cacc]
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

          :maps.put(String.to_integer(codepoint, 16), decomposition, dacc)

        _ ->
          dacc
      end

    kacc =
      case String.to_integer(class) do
        0 -> kacc
        n -> :maps.put(String.to_integer(codepoint, 16), n, kacc)
      end

    {cacc, lacc, iacc, wacc, dacc, kacc}
  end)

defmodule String.Unicode do
  @moduledoc false
  def version, do: {15, 1, 0}

  [unconditional_mappings, _conditional_mappings] =
    Path.join(__DIR__, "SpecialCasing.txt")
    |> File.read!()
    |> :binary.split("# Conditional Mappings")

  codes =
    unconditional_mappings
    |> String.split(["\r\n", "\n"], trim: true)
    |> Enum.reduce(codes, fn
      "", acc ->
        acc

      "#" <> _, acc ->
        acc

      line, acc ->
        [codepoint, lower, _title, upper, _] = :binary.split(line, "; ", [:global])
        key = to_binary.(codepoint)

        :lists.keystore(
          key,
          1,
          acc,
          {key, to_binary.(upper), to_binary.(lower)}
        )
    end)

  # The function computes byte lookups based on the prefix. For example,
  # Á, É, etc all have the same prefix <<195>>, so they are lumped
  # together for lookup and then we just do a byte lookup later. We
  # tried doing the byte lookup on 64-element tuple (since the byte
  # is always within 0b10000000 and 0b10111111) but that's slower,
  # especially because we need to check the byte range for invalid
  # Unicode, instead the last byte lookup is a case. Grouping the
  # top-level lookup makes the cost of a miss 3x cheaper albeit a
  # hit is 10% more expensive) and reduces bytecode size.
  compute_lookup = fn key_values ->
    prefixes =
      Enum.reduce(key_values, %{}, fn {codepoint, result}, acc ->
        prefix_size = bit_size(codepoint) - 8
        <<prefix::size(^prefix_size)-bits, byte>> = codepoint
        Map.update(acc, prefix, [{byte, result}], &[{byte, result} | &1])
      end)

    {singles, tables} =
      Enum.reduce(Map.delete(prefixes, ""), {[], []}, fn {prefix, pairs}, {singles, tables} ->
        case pairs do
          [{byte, result}] ->
            {[{prefix <> <<byte>>, result} | singles], tables}

          _ ->
            clauses =
              Enum.flat_map(pairs, fn {byte, result} ->
                quote do
                  unquote(byte) -> unquote(result)
                end
              end)

            clauses = clauses ++ quote do: (byte -> <<unquote(prefix), byte>>)
            {singles, [{prefix, clauses} | tables]}
        end
      end)

    {Enum.sort(singles), Enum.sort_by(tables, &(-byte_size(elem(&1, 0))))}
  end

  # Sigma variants for Greek
  @letter_sigma <<0x03A3::utf8>>
  @letter_small_sigma_final <<0x03C2::utf8>>
  @letter_small_sigma <<0x03C3::utf8>>

  # Letter I variants for Turkic languages
  @letter_I <<0x0049::utf8>>
  @dotless_letter_i <<0x0131::utf8>>
  @letter_i <<0x0069::utf8>>
  @letter_I_dot_above <<0x0130::utf8>>
  @combining_dot_above <<0x0307::utf8>>

  # Downcase

  # Turkic İ -> i
  def downcase(<<unquote(@letter_I_dot_above), rest::bits>>, acc, mode) do
    char = if mode == :turkic, do: @letter_i, else: <<@letter_i, @combining_dot_above>>
    downcase(rest, [char | acc], mode)
  end

  def downcase(<<@letter_I, @combining_dot_above, rest::bits>>, acc, mode) do
    char = if mode == :turkic, do: @letter_i, else: <<@letter_i, @combining_dot_above>>
    downcase(rest, [char | acc], mode)
  end

  # Turkic I -> ı
  def downcase(<<@letter_I, rest::bits>>, acc, mode) do
    char = if mode == :turkic, do: @dotless_letter_i, else: @letter_i
    downcase(rest, [char | acc], mode)
  end

  # Greek sigma
  def downcase(<<@letter_sigma, rest::bits>>, acc, mode) do
    downcased =
      if mode == :greek and cased_letter_list?(acc) and not cased_letter_binary?(rest) do
        @letter_small_sigma_final
      else
        @letter_small_sigma
      end

    downcase(rest, [downcased | acc], mode)
  end

  conditional_downcase = [@letter_I, @letter_I_dot_above, @letter_sigma]

  {singles, tables} =
    compute_lookup.(
      for {codepoint, _upper, lower} <- codes,
          lower && lower != codepoint,
          codepoint not in conditional_downcase,
          do: {codepoint, lower}
    )

  for {codepoint, lower} <- singles do
    def downcase(<<unquote(codepoint), rest::bits>>, acc, mode) do
      downcase(rest, [unquote(lower) | acc], mode)
    end
  end

  for {prefix, clauses} <- tables do
    def downcase(<<unquote(prefix), byte, rest::bits>>, acc, mode) do
      value = case byte, do: unquote(clauses)
      downcase(rest, [value | acc], mode)
    end
  end

  def downcase(<<byte, rest::bits>>, acc, mode) do
    if byte >= ?A and byte <= ?Z do
      downcase(rest, [byte + 32 | acc], mode)
    else
      downcase(rest, [byte | acc], mode)
    end
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

  # Turkic i -> İ
  def upcase(<<@letter_i, rest::bits>>, acc, mode) do
    char = if mode == :turkic, do: @letter_I_dot_above, else: @letter_I
    upcase(rest, [char | acc], mode)
  end

  conditional_upcase = [@letter_i]

  {singles, tables} =
    compute_lookup.(
      for {codepoint, upper, _lower} <- codes,
          upper && upper != codepoint,
          codepoint not in conditional_upcase,
          do: {codepoint, upper}
    )

  for {codepoint, upper} <- singles do
    def upcase(<<unquote(codepoint), rest::bits>>, acc, mode) do
      upcase(rest, [unquote(upper) | acc], mode)
    end
  end

  for {prefix, clauses} <- tables do
    def upcase(<<unquote(prefix), byte, rest::bits>>, acc, mode) do
      value = case byte, do: unquote(clauses)
      upcase(rest, [value | acc], mode)
    end
  end

  def upcase(<<byte, rest::bits>>, acc, mode) do
    if byte >= ?a and byte <= ?z do
      upcase(rest, [byte - 32 | acc], mode)
    else
      upcase(rest, [byte | acc], mode)
    end
  end

  def upcase("", acc, _mode), do: IO.iodata_to_binary(:lists.reverse(acc))
end

defmodule String.Break do
  @moduledoc false
  @whitespace_max_size 3

  prop_path = Path.join(__DIR__, "PropList.txt")

  whitespace =
    prop_path
    |> File.read!()
    |> String.split(["\r\n", "\n"])
    |> Enum.reduce([], fn line, acc ->
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

  IO.puts(:stderr, "[Unicode] Break on #{length(whitespace)} whitespace codepoints")

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
