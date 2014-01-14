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
    nil
  end

  def codepoints(binary) when is_binary(binary) do
    do_codepoints(next_codepoint(binary))
  end

  defp do_codepoints({ c, rest }) do
    [c|do_codepoints(next_codepoint(rest))]
  end

  defp do_codepoints(nil) do
    []
  end
end
