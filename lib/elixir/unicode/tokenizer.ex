defmodule String.Tokenizer do
  @moduledoc false
  @on_load :check_otp_release

  data_path = Path.join(__DIR__, "UnicodeData.txt")

  {letter_uptitlecase, start, continue} =
    Enum.reduce File.stream!(data_path), {[], [], []}, fn
      line, {letter_uptitlecase, start, continue} ->
        [codepoint, line] = :binary.split(line, ";")
        [_name, line] = :binary.split(line, ";")
        [category, _] = :binary.split(line, ";")

        cond do
          category in ~w(Lu Lt) ->
            {[String.to_integer(codepoint, 16) | letter_uptitlecase], start, continue}
          category in ~w(Ll Lm Lo Nl) ->
            {letter_uptitlecase, [String.to_integer(codepoint, 16) | start], continue}
          category in ~w(Mn Mc Nd Pc) ->
            {letter_uptitlecase, start, [String.to_integer(codepoint, 16) | continue]}
          true ->
            {letter_uptitlecase, start, continue}
        end
    end

  prop_path = Path.join(__DIR__, "PropList.txt")

  {start, continue, patterns} =
    Enum.reduce File.stream!(prop_path), {start, continue, []}, fn line, acc ->
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
    end

  id_upper = letter_uptitlecase -- patterns
  id_start = start -- patterns
  id_continue = continue -- patterns

  {ascii_upper, unicode_upper} = Enum.split_with(id_upper, & &1 <= 127)
  {ascii_start, unicode_start} = Enum.split_with(id_start, & &1 <= 127)

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

  for {first, last} <- rangify.(ascii_upper) do
    if first == last do
      defp ascii_upper?(unquote(first)), do: true
    else
      defp ascii_upper?(entry) when entry in unquote(first)..unquote(last), do: true
    end
  end

  defp ascii_upper?(_), do: false

  for {first, last} <- rangify.(unicode_upper) do
    if first == last do
      defp unicode_upper?(unquote(first)), do: true
    else
      defp unicode_upper?(entry) when entry in unquote(first)..unquote(last), do: true
    end
  end

  defp unicode_upper?(_), do: false

  for {first, last} <- [{?_, ?_} | rangify.(ascii_start)] do
    if first == last do
      defp ascii_start?(unquote(first)), do: true
    else
      defp ascii_start?(entry) when entry in unquote(first)..unquote(last), do: true
    end
  end

  defp ascii_start?(_), do: false

  for {first, last} <- rangify.(unicode_start) do
    if first == last do
      defp unicode_start?(unquote(first)), do: true
    else
      defp unicode_start?(entry) when entry in unquote(first)..unquote(last), do: true
    end
  end

  defp unicode_start?(_), do: false

  for {first, last} <- rangify.(id_continue) do
    if first == last do
      defp continue?(unquote(first)), do: true
    else
      defp continue?(entry) when entry in unquote(first)..unquote(last), do: true
    end
  end

  defp continue?(_), do: false

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

  def tokenize_atom([head | tail] = list) do
    case ascii_start?(head) or ascii_upper?(head) or unicode_start?(head) or unicode_upper?(head) do
      true -> validate_token(continue_atom(tail, [head]))
      false -> {[], list}
    end
  end

  defp continue_atom([?! | tail], acc) do
    {[?! | acc], tail}
  end
  defp continue_atom([?? | tail], acc) do
    {[?? | acc], tail}
  end
  defp continue_atom([?@ | tail], acc) do
    continue_atom(tail, [?@ | acc])
  end
  defp continue_atom([head | tail] = list, acc) do
    if ascii_start?(head) or ascii_upper?(head) or
       (not ascii_pattern?(head) and (unicode_start?(head) or unicode_upper?(head) or continue?(head))) do
      continue_atom(tail, [head | acc])
    else
      {acc, list}
    end
  end
  defp continue_atom([], acc) do
    {acc, []}
  end

  def tokenize_var([head | tail] = list) do
    case ascii_start?(head) or (not ascii_pattern?(head) and unicode_start?(head)) do
      true -> validate_token(continue_var(tail, [head]))
      false -> {[], list}
    end
  end

  defp continue_var([?! | tail], acc) do
    {[?! | acc], tail}
  end
  defp continue_var([?? | tail], acc) do
    {[?? | acc], tail}
  end
  defp continue_var([head | tail] = list, acc) do
    if ascii_start?(head) or ascii_upper?(head) or
       (not ascii_pattern?(head) and (unicode_start?(head) or unicode_upper?(head) or continue?(head))) do
      continue_var(tail, [head | acc])
    else
      {acc, list}
    end
  end
  defp continue_var([], acc) do
    {acc, []}
  end

  defp validate_token({acc, list}) do
    acc = :lists.reverse(acc)
    case :unicode.characters_to_nfc_list(acc) do
      ^acc -> {:ok, acc, list}
      _ -> {:error, "oops"}
    end
  end

  defp check_otp_release do
    case List.to_integer(:erlang.system_info(:otp_release)) >= 20 do
      true -> :ok
      false -> :error
    end
  end
end
