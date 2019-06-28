defmodule String.Tokenizer do
  @moduledoc false

  data_path = Path.join(__DIR__, "UnicodeData.txt")

  {letter_uptitlecase, start, continue, _} =
    data_path
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.reduce({[], [], [], nil}, fn line, acc ->
      {letter_uptitlecase, start, continue, first} = acc
      [codepoint, line] = :binary.split(line, ";")
      [name, line] = :binary.split(line, ";")
      [category, _] = :binary.split(line, ";")

      {codepoints, first} =
        case name do
          "<" <> _ when is_integer(first) ->
            last = String.to_integer(codepoint, 16)
            {Enum.to_list(last..first), nil}

          "<" <> _ ->
            first = String.to_integer(codepoint, 16)
            {[first], first + 1}

          _ ->
            {[String.to_integer(codepoint, 16)], nil}
        end

      cond do
        category in ~w(Lu Lt) ->
          {codepoints ++ letter_uptitlecase, start, continue, first}

        category in ~w(Ll Lm Lo Nl) ->
          {letter_uptitlecase, codepoints ++ start, continue, first}

        category in ~w(Mn Mc Nd Pc) ->
          {letter_uptitlecase, start, codepoints ++ continue, first}

        true ->
          {letter_uptitlecase, start, continue, first}
      end
    end)

  prop_path = Path.join(__DIR__, "PropList.txt")

  {start, continue, patterns} =
    prop_path
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.reduce({start, continue, []}, fn line, acc ->
      [codepoints | category] = :binary.split(line, ";")

      pos =
        case category do
          [" Other_ID_Start" <> _] -> 0
          [" Other_ID_Continue" <> _] -> 1
          [" Pattern_White_Space" <> _] -> 2
          [" Pattern_Syntax" <> _] -> 2
          _ -> -1
        end

      if pos >= 0 do
        entries =
          case :binary.split(codepoints, "..") do
            [<<codepoint::4-binary, _::binary>>] ->
              [String.to_integer(codepoint, 16)]

            [first, <<last::4-binary, _::binary>>] ->
              Enum.to_list(String.to_integer(last, 16)..String.to_integer(first, 16))
          end

        put_elem(acc, pos, entries ++ elem(acc, pos))
      else
        acc
      end
    end)

  id_upper = letter_uptitlecase -- patterns
  id_start = start -- patterns
  id_continue = continue -- patterns

  unicode_upper = Enum.filter(id_upper, &(&1 > 127))
  unicode_start = Enum.filter(id_start, &(&1 > 127))
  unicode_continue = Enum.filter(id_continue, &(&1 > 127))

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

  @compile {:inline, ascii_upper?: 1, ascii_start?: 1, ascii_continue?: 1}
  defp ascii_upper?(entry), do: entry in ?A..?Z

  defp ascii_start?(?_), do: true
  defp ascii_start?(entry), do: entry in ?a..?z

  defp ascii_continue?(entry), do: entry in ?0..?9

  range = rangify.(unicode_upper)

  for {first, last} <- range do
    if first == last do
      defp unicode_upper?(unquote(first)), do: true
    else
      defp unicode_upper?(entry) when entry in unquote(first)..unquote(last), do: true
    end
  end

  defp unicode_upper?(_), do: false

  range = rangify.(unicode_start)

  for {first, last} <- range do
    if first == last do
      defp unicode_start?(unquote(first)), do: true
    else
      defp unicode_start?(entry) when entry in unquote(first)..unquote(last), do: true
    end
  end

  defp unicode_start?(_), do: false

  unless {13312, 19893} in range do
    raise "CHECK: CJK Ideograph not in range"
  end

  for {first, last} <- rangify.(unicode_continue) do
    if first == last do
      defp unicode_continue?(unquote(first)), do: true
    else
      defp unicode_continue?(entry) when entry in unquote(first)..unquote(last), do: true
    end
  end

  defp unicode_continue?(_), do: false

  # Pattern is used as a performance check since most
  # atoms and variables end with an atom character.
  for {first, last} <- rangify.(patterns), last <= 127 do
    if first == last do
      defp ascii_pattern?(unquote(first)), do: true
    else
      defp ascii_pattern?(entry) when entry in unquote(first)..unquote(last), do: true
    end
  end

  defp ascii_pattern?(_), do: false

  def tokenize([head | tail]) do
    cond do
      ascii_upper?(head) ->
        validate(continue(tail, [head], 1, true, []), :alias)

      ascii_start?(head) ->
        validate(continue(tail, [head], 1, true, []), :identifier)

      unicode_upper?(head) ->
        validate(continue(tail, [head], 1, false, []), :atom)

      unicode_start?(head) ->
        validate(continue(tail, [head], 1, false, []), :identifier)

      true ->
        {:error, :empty}
    end
  end

  def tokenize([]) do
    {:error, :empty}
  end

  defp continue([?! | tail], acc, length, ascii_letters?, special) do
    {[?! | acc], tail, length + 1, ascii_letters?, [?! | special]}
  end

  defp continue([?? | tail], acc, length, ascii_letters?, special) do
    {[?? | acc], tail, length + 1, ascii_letters?, [?? | special]}
  end

  defp continue([?@ | tail], acc, length, ascii_letters?, special) do
    continue(tail, [?@ | acc], length + 1, ascii_letters?, [?@ | List.delete(special, ?@)])
  end

  defp continue([head | tail] = list, acc, length, ascii_letters?, special) do
    cond do
      ascii_start?(head) or ascii_upper?(head) or ascii_continue?(head) ->
        continue(tail, [head | acc], length + 1, ascii_letters?, special)

      not ascii_pattern?(head) and
          (unicode_start?(head) or unicode_upper?(head) or unicode_continue?(head)) ->
        continue(tail, [head | acc], length + 1, false, special)

      true ->
        {acc, list, length, ascii_letters?, special}
    end
  end

  defp continue([], acc, length, ascii_letters?, special) do
    {acc, [], length, ascii_letters?, special}
  end

  defp validate({acc, rest, length, ascii_letters?, special}, kind) do
    acc = :lists.reverse(acc)

    if ascii_letters? or :unicode.characters_to_nfc_list(acc) == acc do
      {kind, acc, rest, length, ascii_letters?, special}
    else
      {:error, {:not_nfc, acc}}
    end
  end
end
