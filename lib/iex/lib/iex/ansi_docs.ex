defmodule IEx.ANSIDocs do
  @moduledoc """
  Take a Markdown formatted docstring and attempt to make it a
  little  prettier on the terminal.

  We support only a minimal subset of Markdown. It is preferrable
  to keep it simple (and not properly escape in some cases) than have a
  full-featured markdown parser.

  We use the following attributes (whose defaults are set in mix.exs):

  * :doc_code        — the attributes for code blocks (cyan, bright)
  * :doc_inline_code - inline code (cyan)
  * :doc_headings    - h1 and h2 (yellow, bright)
  * :doc_title       — the overall heading for the output (reverse,yellow,bright)
  * :doc_bold        - (bright)
  * :doc_underline   - (underline)
  """

  def doc_heading(string, use_ansi // IO.ANSI.terminal?) do
    if use_ansi do
      write_doc_heading(string)
    else
      IO.puts "* #{string}\n"
    end
    IEx.dont_display_result
  end

  def format(doc, use_ansi // IO.ANSI.terminal?) do
    if use_ansi do
      doc
      |> String.split(["\r\n","\n"], trim: false)
      |> Enum.map(&String.rstrip/1)
      |> process(_indent = "")
    else
      IO.puts doc
    end
    IEx.dont_display_result
  end

  # Bring lines back to a common left margin
  defp bring_to_margin([]), do: []
  defp bring_to_margin(lines) do
    case (lines |> Enum.map(&get_leading_spaces/1) |> Enum.min) do
      0 -> lines
      n -> Enum.map(lines, &strip_leading_spaces(&1, n))
    end
  end

  defp get_leading_spaces(""), do: 999
  defp get_leading_spaces(line) do
     [{ _, leading_spaces }] = Regex.run(%r{^\s*}, line, return: :index)
     leading_spaces
  end

  defp strip_leading_spaces("", _n),  do: ""
  defp strip_leading_spaces(line, n), do: String.slice(line, n, 999)


  defp process([], _indent), do: nil

  defp process([<< "# " :: utf8, heading :: binary>> | rest], _indent) do
    write_h1(String.strip(heading))
    process(rest, "")
  end

  defp process([<< "## " :: utf8, heading :: binary>> | rest], _indent) do
    write_h2(String.strip(heading))
    process(rest, "")
  end

  defp process([<< "### " :: utf8, heading :: binary>> | rest], indent) do
    write_h3(String.strip(heading), indent)
    process(rest, indent)
  end

  defp process([<< "* " :: utf8, line :: binary >> | rest], indent) do
    { list_content, rest } = split_out_list(rest)
    list_content 
    |> process_ul([line], indent)
    rest 
    |> bring_to_margin 
    |> process(indent)
  end

  defp process([ "" | rest ], indent) do
    process(rest, indent)
  end

  defp process( [ << "    " :: utf8, line :: binary>> | rest], indent) do
    process_code(rest, [line], indent)
  end

  defp process([ line | rest ], indent) do
    process_para(rest, [line], indent)
  end

  defp process_para(doc=[ "" | _rest], para, indent) do
    write_para(para, indent)
    process(doc, indent)
  end

  defp process_para([], para, indent) do
    write_para(para, indent)
  end

  defp process_para([line | rest], para, indent) do
    process_para(rest, [ line | para ], indent)
  end

  defp process_code([], code, indent) do
    write_code(code, indent)
  end

  # blank line between code blocks
  defp process_code([ "", << "    " :: utf8, line :: binary>> | rest ], code, indent) do
    process_code(rest, [line, "" | code], indent)
  end

  defp process_code([ << "    " :: utf8, line :: binary>> | rest ], code, indent) do
    process_code(rest, [line|code], indent)
  end

  defp process_code(rest, code, indent) do
    write_code(code, indent)
    process(rest, indent)
  end


  defp process_ul([], result, indent) do
    write_ul(result, indent)
  end

  defp process_ul(["" | rest], result, indent) do
    write_ul(result, indent)
    IO.puts ""
    process(rest, indent)
  end

  defp process_ul(sublist = [line|rest], result, indent) do
    if (String.lstrip(line) |> String.starts_with?("* ")) do
      write_ul(result, indent)
      sublist |> bring_to_margin |> process(indent <> "  ")
    else
      process_ul(rest, [line|result], indent)
    end
  end

  defp write_doc_heading(heading) do
    IO.puts IO.ANSI.reset
    width = column_width("")
    padding = div(width + String.length(heading), 2)
    heading = heading |> String.rjust(padding) |> String.ljust(width)
    write(:doc_title, heading)
  end

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

  defp write_para(para, indent) do
    para = para |> Enum.reverse |> Enum.join(" ")
    para = Regex.replace(%r{\[(.*?)\]\((.*?)\)}, para, "\\1 (\\2)")
    words = para |> String.split(%r{\s})
    width = column_width(indent)
    IO.write(indent)
    write_with_wrap(words, width, width, indent)
    IO.puts ""
  end

  defp write_code(code, indent) do
    write(:doc_code, "#{indent}┃ #{Enum.join(Enum.reverse(code), "\n#{indent}┃ ")}")
  end

  defp write_ul(list, indent) do
    list = list |> Enum.reverse |> Enum.join(" ") |> String.split(%r{\s})
    IO.write(indent)
    IO.write("• ")
    width = column_width(indent)
    write_with_wrap(list, width, width, indent <> "  ")
  end

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

  # divide the lines into the leading portion that can be part of
  # the list and the rest. The first group is lines with at least 2
  # leading spaces (which we remove). 

  defp split_out_list(lines) do
    { list, rest } = Enum.split_while lines, &list_leader?/1
    { Enum.map(list, &chop(&1, 2)), rest }
  end

  defp list_leader?(<<"  "::utf8, rest::binary>>), do: true
  defp list_leader?(""), do: true
  defp list_leader?(_),  do: false

  defp chop(line, n) when size(line) >= n do
    String.slice(line, n, 999)
  end

  defp chop(_, _), do: ""

  defp color_name_for(?`), do: :doc_inline_code
  defp color_name_for(?_), do: :doc_underline
  defp color_name_for(?*), do: :doc_bold


  defp color(color_name) do
    colors = IEx.Options.get(:colors)
    if colors[:enabled] do
      IO.ANSI.escape_fragment("%{#{colors[color_name]}}")
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