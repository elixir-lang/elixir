defmodule GraphemesTest do
  def run do
    IO.puts "Running GraphemeBreakTest.txt"
    count = run_grapheme_break()
    IO.puts "Got #{count} failures"
  end

  defp run_grapheme_break do
    Path.join(__DIR__, "GraphemeBreakTest.txt")
    |> File.stream!()
    |> Stream.filter(&match?("÷" <> _, &1))
    |> Stream.reject(& &1 =~ "D800")
    |> Enum.reduce(0, fn line, acc ->
      [string | _] = String.split(line, "#", parts: 2)
      {string, graphemes} = parse_grapheme_break(string)
      if String.graphemes(string) == graphemes do
        acc
      else
        acc = acc + 1
        IO.puts """
        ============== Failure ##{acc} ==============

            String.graphemes(#{inspect string})

        must be:

            #{inspect graphemes}

        got:

            #{inspect String.graphemes(string)}

        On line:

            #{line}
        """
        acc
      end
    end)
  end

  defp parse_grapheme_break(string) do
    string
    |> String.trim()
    |> String.trim_leading("÷ ")
    |> String.trim_trailing(" ÷")
    |> parse_grapheme_break("", [])
  end

  defp parse_grapheme_break(string, acc_string, acc_list) do
    case String.split(string, " ÷ ", parts: 2) do
      [left, right] ->
        grapheme = breaks_to_grapheme(left)
        parse_grapheme_break(right, acc_string <> grapheme, [grapheme | acc_list])
      [left] ->
        grapheme = breaks_to_grapheme(left)
        {acc_string <> grapheme, :lists.reverse(acc_list, [grapheme])}
    end
  end

  defp breaks_to_grapheme(string) do
    for codepoint <- String.split(string, " × "),
        do: <<String.to_integer(codepoint, 16)::utf8>>,
        into: ""
  end
end

GraphemesTest.run
