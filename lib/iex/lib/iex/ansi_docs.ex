defmodule IEx.ANSIDocs do
  @moduledoc false

  @doc """
  Prints the head of the documentation (i.e. the function signature)
  """
  def print_heading(string, use_ansi // IO.ANSI.terminal?) do
    if use_ansi do
      write_doc_heading(string)
    else
      IO.puts "* #{string}\n"
    end
    IEx.dont_display_result
  end

  defp write_doc_heading(heading) do
    IO.puts IO.ANSI.reset
    width = column_width("")
    padding = div(width + String.length(heading), 2)
    heading = heading |> String.rjust(padding) |> String.ljust(width)
    write(:doc_title, heading)
  end

  @doc """
  Prints the documentation body.
  """
  def print(doc, use_ansi // IO.ANSI.terminal?) do
    if use_ansi do
      doc
      |> String.split(["\r\n","\n"], trim: false)
      |> Enum.map(&String.rstrip/1)
      |> process("")
    else
      IO.puts doc
    end
    IEx.dont_display_result
  end

  defp process([], _indent), do: nil

  defp process(["# " <> heading | rest], _indent) do
    write_h1(String.strip(heading))
    process(rest, "")
  end

  defp process(["## " <> heading | rest], _indent) do
    write_h2(String.strip(heading))
    process(rest, "")
  end

  defp process(["### " <> heading | rest], indent) do
    write_h3(String.strip(heading), indent)
    process(rest, indent)
  end

  defp process(["" | rest], indent) do
    process(rest, indent)
  end

  defp process(["    " <> line | rest], indent) do
    process_code(rest, [line], indent)
  end

  defp process([line | rest], indent) do
    { stripped, count } = strip_spaces(line, 0)
    case stripped do
      "* " <> item ->
        process_list(item, rest, count, indent)
      _ ->
        process_text(rest, [line], indent, false)
    end
  end

  defp strip_spaces(" " <> line, acc) do
    strip_spaces(line, acc + 1)
  end

  defp strip_spaces(rest, acc) do
    { rest, acc }
  end

  ## Headings

  defp write_h1(heading) do
    write_h2(String.upcase(heading))
  end

  defp write_h2(heading) do
    write(:doc_headings, heading)
  end

  defp write_h3(heading, indent) do
    IO.write(indent)
    write(:doc_headings, heading)
  end

  ## Lists

  defp process_list(line, rest, count, indent) do
    IO.write indent <> "• "
    { contents, rest, done } = process_list_next(rest, count, false, [])
    process_text(contents, [line], indent <> "  ", true)
    if done, do: IO.puts IO.ANSI.reset
    process(rest, indent)
  end

  # Process the thing after a list item entry. It can be either:
  #
  # * Continuation of the list
  # * A nested list
  # * The end of the list
  #
  defp process_list_next([" " <> _ = line | rest], count, _done, acc) do
    case list_next(line, count) do
      :done    -> { Enum.reverse(acc), [line|rest], false }
      chopped  -> process_list_next(rest, count, false, [chopped|acc])
    end
  end

  defp process_list_next(["* " <> _ | _] = rest, _count, _done, acc) do
    { Enum.reverse(acc), rest, false }
  end

  defp process_list_next(["" | rest], count, _done, acc) do
    process_list_next(rest, count, true, [""|acc])
  end

  defp process_list_next(rest, _count, done, acc) do
    { Enum.reverse(acc), rest, done }
  end

  defp list_next("* " <> _, 0),     do: :done
  defp list_next(line, 0),          do: chop(line, 2)
  defp list_next(" " <> line, acc), do: list_next(line, acc - 1)
  defp list_next(line, _acc),       do: line

  defp chop(" " <> line, acc) when acc > 0, do: chop(line, acc - 1)
  defp chop(line, _acc), do: line

  ## Text (paragraphs / lists)

  defp process_text(doc=["" | _], para, indent, from_list) do
    write_text(Enum.reverse(para), indent, from_list)
    process(doc, indent)
  end

  defp process_text([], para, indent, from_list) do
    write_text(Enum.reverse(para), indent, from_list)
  end

  defp process_text([line | rest], para, indent, true) do
    { stripped, count } = strip_spaces(line, 0)
    case stripped do
      "* " <> item ->
        write_text(Enum.reverse(para), indent, true)
        process_list(item, rest, count, indent)
      _ ->
        process_text(rest, [line | para], indent, true)
    end
  end

  defp process_text([line | rest], para, indent, from_list) do
    process_text(rest, [line | para], indent, from_list)
  end

  defp write_text(para, indent, from_list) do
    para  = para |> Enum.join(" ") |> handle_links
    words = para |> String.split(%r{\s})
    width = column_width(indent)
    unless from_list, do: IO.write(indent)
    write_with_wrap(words, width, width, indent)
    unless from_list, do: IO.puts IO.ANSI.reset
  end

  ## Code blocks

  defp process_code([], code, indent) do
    write_code(code, indent)
  end

  # Blank line between code blocks
  defp process_code([ "", "    " <> line | rest ], code, indent) do
    process_code(rest, [line, "" | code], indent)
  end

  defp process_code([ "    " <> line | rest ], code, indent) do
    process_code(rest, [line|code], indent)
  end

  defp process_code(rest, code, indent) do
    write_code(code, indent)
    process(rest, indent)
  end

  defp write_code(code, indent) do
    write(:doc_code, "#{indent}┃ #{Enum.join(Enum.reverse(code), "\n#{indent}┃ ")}")
  end

  ## Helpers

  defp write(style, string) do
    IO.puts IEx.color(style, string)
    IO.puts IO.ANSI.reset
  end

  defp write_with_wrap([], left, max, _indent) do
    unless left == max, do: IO.write("\n")
  end

  defp write_with_wrap([word|words], left_on_line, max_columns, indent) do
    { word, leader, trailer, punc } = look_for_markup(word)

    word_length = String.length(word)
    if punc, do: word_length = word_length + 1

    if word_length >= left_on_line do
      IO.write "\n"
      IO.write indent
      left_on_line = max_columns
    end

    if leader,  do: IO.write(leader)
    IO.write(word)
    if trailer, do: IO.write(trailer)
    if punc,    do: IO.write(punc)

    unless length(words) == 0, do: IO.write(" ")
    write_with_wrap(words, left_on_line - word_length - 1, max_columns, indent)
  end

  defp look_for_markup(word) when size(word) <= 2 do
    { word, nil, nil, nil }
  end

  defp look_for_markup(word) do
    if String.starts_with?(word, ["`", "_", "*"]) do
      <<first::utf8, word::binary>> = word
      leader = color(color_name_for(first))
    end

    if String.ends_with?(word, %w/ . , : ; ' " ! - ] } )/) do
      punc = String.last(word)
      word = String.slice(word, 0, String.length(word)-1)
    end

    if String.ends_with?(word, ["`", "_", "*"]) do
      chop = if String.ends_with?(word, "**"), do: 2, else: 1
      word = String.slice(word, 0, String.length(word)-chop)
      trailer = IO.ANSI.reset
    end

    { word, leader, trailer, punc }
  end

  defp handle_links(text) do
    Regex.replace(%r{\[(.*?)\]\((.*?)\)}, text, "\\1 (\\2)")
  end

  defp color_name_for(?`), do: :doc_inline_code
  defp color_name_for(?_), do: :doc_underline
  defp color_name_for(?*), do: :doc_bold

  defp color(color_name) do
    colors = IEx.Options.get(:colors)

    if colors[:enabled] do
      IO.ANSI.escape_fragment("%{#{colors[color_name]}}", true)
    else
      ""
    end
  end

  defp column_width(_indent) do
    case :io.columns do
      { :ok, width } -> width
      _              -> 80
    end
  end
end