defmodule FormatDocs do
  @moduledoc """
  Take a Markdown formatted docstring and attempt to make it a little 
  prettier on the terminal. We support only a minimal subset of Markdown,
  and we cheat like crazy to avoid having to do a proper parse, so there'll 
  be boundary conditions galore.

  We use the following attributes (whose defaults are set in mix.exs):

  * :code—the attributes for code blocks (cyan, bright)
  * :inline_code:inline code (cyan)
  * :headings: h1 and h2 (yellow, bright)
  * :doc_heading—the overall heading for the output (reverse,yellow,bright)
  * :bold (bright)
  * :underline "underline",
  """

  def doc_heading(string) do
    if IO.ANSI.terminal? do
      write_doc_heading(string)
    else
      IO.puts "* #{string}\n"
    end
  end

  def write(doc) do
    if IO.ANSI.terminal? do
      doc
      |> String.split(["\r\n","\n"], trim: false)
      |> Enum.map(&String.rstrip/1)
      |> bring_to_margin
      |> process(_indent = "")
    else
      IO.puts doc
    end
  end

  @doc "Bring lines back to a common left margin"
  def bring_to_margin(lines) do
    case (lines |> Enum.map(&get_leading_spaces/1) |> Enum.min) do
      0 -> lines
      n -> Enum.map(lines, &strip_leading_spaces(&1, n))
    end
  end

  def get_leading_spaces(""), do: 999
  def get_leading_spaces(line) do
     [{ _, leading_spaces }] = Regex.run(%r{^\s*}, line, return: :index)
     leading_spaces
  end

  def strip_leading_spaces(line, n), do: String.slice(line, n, 999)



  def process([], _indent), do: nil

  def process([<< "# " :: utf8, heading :: binary>> | rest], _indent) do
    write_h1(String.strip(heading))
    process(rest, "")
  end

  def process([<< "## " :: utf8, heading :: binary>> | rest], _indent) do
    write_h2(String.strip(heading))
    process(rest, "")
  end

  def process([<< "* " :: utf8, line :: binary >> | rest], indent) do
    { list_content, rest } = split_out_list(rest)
    process_ul(list_content, [line], indent <> "  ")
    process(rest, indent)
  end

  def process([ "" | rest ], indent) do
    process(rest, indent)
  end

  def process( [ << "    " :: utf8, line :: binary>> | rest], indent) do
    process_code(rest, [line], indent)
  end

  def process([ line | rest ], indent) do
    process_para(rest, [line], indent)
  end

  def process_para(doc=[ "" | _rest], para, indent) do
    write_para(para, indent)
    process(doc, indent)
  end

  def process_para([], para, indent) do
    write_para(para, indent)
  end

  def process_para([line | rest], para, indent) do
    process_para(rest, [ line | para ], indent)
  end



  def process_code([], code, indent) do
    write_code(code, indent)
  end

  def process_code([ << "    " :: utf8, line :: binary>> | rest ], code, indent) do
    process_code(rest, [line|code], indent)
  end

  def process_code(rest, code, indent) do
    write_code(code, indent)
    process(rest, indent)
  end

  def process_ul([ << "  " :: utf8, line :: binary>> | rest], list, indent) do
    process_ul(rest, [ line | list ], indent)
  end

  def process_ul(rest, list, indent) do
    write_ul(list, indent)
    process(rest, indent)
  end


  def write_doc_heading(heading) do
    IO.puts IO.ANSI.reset
    width = column_width("")
    padding = div(width + String.length(heading), 2)
    heading = heading |> String.rjust(padding) |> String.ljust(width)
    write(:doc_heading, heading)
  end

  def write_h1(heading) do
    write_h2(String.upcase(heading))
  end

  def write_h2(heading) do
    write(:headings, heading)
  end

  def write_para(para, indent) do
    para = para |> Enum.reverse |> Enum.join(" ")
    para = Regex.replace(%r{\[(.*?)\]\((.*?)\)}, para, "\\1 (\\2)")
    words = para |> String.split(%r{\s})
    width = column_width(indent)
    IO.write(indent)
    write_with_wrap(words, width, width, indent)
    IO.puts ""
  end

  def write_code(code, indent) do
    write(:code, "#{indent}┃ #{Enum.join(Enum.reverse(code), "\n#{indent}┃ ")}")
  end

  def write_ul(list, indent) do
    list = list |> Enum.reverse |> Enum.join(" ") |> String.split(%r{\s})
    IO.write("• ")
    width = column_width(indent)
    write_with_wrap(list, width, width, indent)
  end

  def write(style, string) do
    IO.puts IEx.color(style, string)
    IO.puts IO.ANSI.reset
  end


  def write_with_wrap([], left, max, _indent) do
    unless left == max, do: IO.write("\n")
  end

  def write_with_wrap([word|words], left_on_line, max_columns, indent) do
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
    if punc do
      IO.write(punc)
    end
    IO.write(" ")
    write_with_wrap(words, left_on_line - word_length - 1, max_columns, indent)
  end

  def look_for_markup(word) do
    leader = nil
    trailer = nil
    punc    = nil

    if String.starts_with?(word, ["`", "_", "*"]) do
      << first::utf8, word::binary>> = word
      leader = color(color_name_for(first))
    end

    if String.ends_with?(word, %w/ . , : ; ' " ! - ] } )/) do
      punc = String.last(word)
      word = String.slice(word, 0, String.length(word)-1)
    end

    if String.ends_with?(word, ["`", "_", "*"]) do
      word = String.slice(word, 0, String.length(word)-1)
      trailer = IO.ANSI.reset
    end
  
    { word, leader, trailer, punc }
  end

  # divide the lines into the leading portion that can be part of
  # the list and the rest. The first group is lines with at least 2
  # leading spaces (which we remove)
  def split_out_list(lines) do
    { list, rest } = Enum.split_while lines, fn line ->
      String.starts_with?(line, "  ") || line == ""
    end
    { Enum.map(list, &chop(&1, 2)), rest }
  end

  def chop(line, n) when size(line) >= n do
    String.slice(line, n, 999)
  end

  def chop(_, _), do: ""


  def color_name_for(?`), do: :inline_code
  def color_name_for(?_), do: :underline
  def color_name_for(?*), do: :bold

  def color(color_name) do
    colors = IEx.Options.get(:colors)
    if colors[:enabled] do
      IO.ANSI.escape_fragment("%{#{colors[color_name]}}")
    else
      ""
    end
  end

  def column_width(indent) do
    { :ok, width } = :io.columns
    width - String.length(indent) - 1
  end
end