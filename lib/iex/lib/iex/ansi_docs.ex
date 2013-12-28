defmodule IEx.ANSIDocs do
  @moduledoc false

  @bullets [?*, ?-, ?+]

  @doc """
  Prints the head of the documentation (i.e. the function signature)
  """
  def print_heading(string, use_ansi // IO.ANSI.terminal?, colors // IEx.Options.get(:colors)) do
    if use_ansi do
      write_doc_heading(string, colors)
    else
      IO.puts "* #{string}\n"
    end
    IEx.dont_display_result
  end

  defp write_doc_heading(heading, colors) do
    IO.puts IO.ANSI.reset
    width   = column_width()
    padding = div(width + String.length(heading), 2)
    heading = heading |> String.rjust(padding) |> String.ljust(width)
    write(:doc_title, heading, colors)
  end

  @doc """
  Prints the documentation body.
  """
  def print(doc, use_ansi // IO.ANSI.terminal?, colors // IEx.Options.get(:colors)) do
    if use_ansi do
      doc
      |> String.split(["\r\n","\n"], trim: false)
      |> Enum.map(&String.rstrip/1)
      |> process("", colors)
    else
      IO.puts doc
    end
    IEx.dont_display_result
  end

  defp process([], _indent, _colors), do: nil

  defp process(["# " <> heading | rest], _indent, colors) do
    write_h1(String.strip(heading), colors)
    process(rest, "", colors)
  end

  defp process(["## " <> heading | rest], _indent, colors) do
    write_h2(String.strip(heading), colors)
    process(rest, "", colors)
  end

  defp process(["### " <> heading | rest], indent, colors) do
    write_h3(String.strip(heading), indent, colors)
    process(rest, indent, colors)
  end

  defp process(["" | rest], indent, colors) do
    process(rest, indent, colors)
  end

  defp process(["    " <> line | rest], indent, colors) do
    process_code(rest, [line], indent, colors)
  end

  defp process([line | rest], indent, colors) do
    { stripped, count } = strip_spaces(line, 0)
    case stripped do
      <<bullet, ?\s, item :: binary >> when bullet in @bullets ->
        process_list(item, rest, count, indent, colors)
      _ ->
        process_text(rest, [line], indent, false, colors)
    end
  end

  defp strip_spaces(" " <> line, acc) do
    strip_spaces(line, acc + 1)
  end

  defp strip_spaces(rest, acc) do
    { rest, acc }
  end

  ## Headings

  defp write_h1(heading, colors) do
    write_h2(String.upcase(heading), colors)
  end

  defp write_h2(heading, colors) do
    write(:doc_headings, heading, colors)
  end

  defp write_h3(heading, indent, colors) do
    IO.write(indent)
    write(:doc_headings, heading, colors)
  end

  ## Lists

  defp process_list(line, rest, count, indent, colors) do
    IO.write indent <> "• "
    { contents, rest, done } = process_list_next(rest, count, false, [])
    process_text(contents, [line], indent <> "  ", true, colors)
    if done, do: IO.puts(IO.ANSI.reset)
    process(rest, indent, colors)
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

  defp process_list_next([<<bullet, ?\s, _ :: binary>> | _] = rest, _count, _done, acc) when bullet in @bullets do
    { Enum.reverse(acc), rest, false }
  end

  defp process_list_next(["" | rest], count, _done, acc) do
    process_list_next(rest, count, true, [""|acc])
  end

  defp process_list_next(rest, _count, done, acc) do
    { Enum.reverse(acc), rest, done }
  end

  defp list_next(<<bullet, ?\s, _ :: binary>>, 0) when bullet in @bullets, do: :done
  defp list_next(line, 0),          do: chop(line, 2)
  defp list_next(" " <> line, acc), do: list_next(line, acc - 1)
  defp list_next(line, _acc),       do: line

  defp chop(" " <> line, acc) when acc > 0, do: chop(line, acc - 1)
  defp chop(line, _acc), do: line

  ## Text (paragraphs / lists)

  defp process_text(doc=["" | _], para, indent, from_list, colors) do
    write_text(Enum.reverse(para), indent, from_list)
    process(doc, indent, colors)
  end

  defp process_text([], para, indent, from_list, _colors) do
    write_text(Enum.reverse(para), indent, from_list)
  end

  defp process_text([line | rest], para, indent, true, colors) do
    { stripped, count } = strip_spaces(line, 0)
    case stripped do
      <<bullet, ?\s, item :: binary>> when bullet in @bullets ->
        write_text(Enum.reverse(para), indent, true)
        process_list(item, rest, count, indent, colors)
      _ ->
        process_text(rest, [line | para], indent, true, colors)
    end
  end

  defp process_text([line | rest], para, indent, from_list, colors) do
    process_text(rest, [line | para], indent, from_list, colors)
  end

  defp write_text(lines, indent, from_list) do
    lines
    |> Enum.join(" ")
    |> handle_links
    |> handle_inline(nil, [], [])
    |> String.split(%r{\s})
    |> write_with_wrap(column_width() - size(indent), indent, from_list)

    unless from_list, do: IO.puts(IO.ANSI.reset)
  end

  ## Code blocks

  defp process_code([], code, indent, _colors) do
    write_code(code, indent)
  end

  # Blank line between code blocks
  defp process_code([ "", "    " <> line | rest ], code, indent, colors) do
    process_code(rest, [line, "" | code], indent, colors)
  end

  defp process_code([ "    " <> line | rest ], code, indent, colors) do
    process_code(rest, [line|code], indent, colors)
  end

  defp process_code(rest, code, indent, colors) do
    write_code(code, indent)
    process(rest, indent, colors)
  end

  defp write_code(code, indent) do
    write(:doc_code, "#{indent}┃ #{Enum.join(Enum.reverse(code), "\n#{indent}┃ ")}")
  end

  ## Helpers

  defp write(style, string, colors // IEx.Options.get(:colors)) do
    color =  colors[style]
    enabled = colors[:enabled]
    seq_color = IO.ANSI.escape_fragment("%{#{color}}", enabled)
    seq_reset = IO.ANSI.escape_fragment("%{reset}", enabled)
    IO.puts seq_color <> string <> seq_reset
    IO.puts seq_reset
  end

  defp write_with_wrap([], _available, _indent, _first) do
    :ok
  end

  defp write_with_wrap(words, available, indent, first) do
    { words, rest } = take_words(words, available, [])
    IO.puts (if first, do: "", else: indent) <> Enum.join(words, " ")
    write_with_wrap(rest, available, indent, false)
  end

  defp take_words([word|words], available, acc) do
    available = available - length_without_escape(word, 0)

    cond do
      # It fits, take one for space and continue decreasing
      available > 0 ->
        take_words(words, available - 1, [word|acc])

      # No space but we got no words
      acc == [] ->
        { [word], words }

      # Otherwise
      true ->
        { Enum.reverse(acc), [word|words] }
    end
  end

  defp take_words([], _available, acc) do
    { Enum.reverse(acc), [] }
  end

  defp length_without_escape(<< ?\e, ?[, _, _, ?m, rest :: binary >>, count) do
    length_without_escape(rest, count)
  end

  defp length_without_escape(<< ?\e, ?[, _, ?m, rest :: binary >>, count) do
    length_without_escape(rest, count)
  end

  defp length_without_escape(rest, count) do
    case String.next_grapheme(rest) do
      :no_grapheme -> count
      { _, rest }  -> length_without_escape(rest, count + 1)
    end
  end

  defp handle_links(text) do
    text
    |> remove_square_brackets_in_link
    |> escape_underlines_in_link
  end

  defp escape_underlines_in_link(text) do
    case Regex.match?(%r{.*(https?\S*)}, text) do
      true ->
        Regex.replace(%r{_}, text, "\\\\_")
      _ ->
        text
    end
  end

  defp remove_square_brackets_in_link(text) do
    Regex.replace(%r{\[(.*?)\]\((.*?)\)}, text, "\\1 (\\2)")
  end

  # Single inline quotes.
  @single [?`, ?_, ?*]

  # ` does not require space in between
  @spaced [?_, ?*]

  # Clauses for handling spaces
  defp handle_inline(<<?*, ?*, ?\s, rest :: binary>>, nil, buffer, acc) do
    handle_inline(rest, nil, [?\s, ?*, ?*|buffer], acc)
  end

  defp handle_inline(<<mark, ?\s, rest :: binary>>, nil, buffer, acc) when mark in @spaced do
    handle_inline(rest, nil, [?\s, mark|buffer], acc)
  end

  defp handle_inline(<<?\s, ?*, ?*, rest :: binary>>, limit, buffer, acc) do
    handle_inline(rest, limit, [?*, ?*, ?\s|buffer], acc)
  end

  defp handle_inline(<<?\s, mark, rest :: binary>>, limit, buffer, acc) when mark in @spaced do
    handle_inline(rest, limit, [mark, ?\s|buffer], acc)
  end

  # Clauses for handling escape
  defp handle_inline(<<?\\, ?\\, rest :: binary>>, limit, buffer, acc) do
    handle_inline(rest, limit, [?\\|buffer], acc)
  end

  defp handle_inline(<<?\\, ?*, ?*, rest :: binary>>, limit, buffer, acc) do
    handle_inline(rest, limit, [?*, ?*|buffer], acc)
  end

  # A escape is not valid inside `
  defp handle_inline(<<?\\, mark, rest :: binary>>, limit, buffer, acc)
      when mark in [?_, ?*, ?`] and not(mark == limit and mark == ?`) do
    handle_inline(rest, limit, [mark|buffer], acc)
  end

  # Inline start
  defp handle_inline(<<?*, ?*, rest :: binary>>, nil, buffer, acc) when rest != "" do
    handle_inline(rest, ?d, ["**"], [Enum.reverse(buffer)|acc])
  end

  defp handle_inline(<<mark, rest :: binary>>, nil, buffer, acc) when rest != "" and mark in @single do
    handle_inline(rest, mark, [<<mark>>], [Enum.reverse(buffer)|acc])
  end

  # Inline end
  defp handle_inline(<<?*, ?*, rest :: binary>>, ?d, buffer, acc) do
    handle_inline(rest, nil, [], [inline_buffer(buffer)|acc])
  end

  defp handle_inline(<<mark, rest :: binary>>, mark, buffer, acc) when mark in @single do
    handle_inline(rest, nil, [], [inline_buffer(buffer)|acc])
  end

  defp handle_inline(<<char, rest :: binary>>, mark, buffer, acc) do
    handle_inline(rest, mark, [char|buffer], acc)
  end

  defp handle_inline(<<>>, _mark, buffer, acc) do
    iolist_to_binary Enum.reverse([Enum.reverse(buffer)|acc])
  end

  defp inline_buffer(buffer) do
    [h|t] = Enum.reverse([IO.ANSI.reset|buffer])
    [color_for(h)|t]
  end

  defp color_for("`"),  do: color(:doc_inline_code)
  defp color_for("_"),  do: color(:doc_underline)
  defp color_for("*"),  do: color(:doc_bold)
  defp color_for("**"), do: color(:doc_bold)

  defp color(color_name) do
    colors = IEx.Options.get(:colors)

    if colors[:enabled] do
      IO.ANSI.escape_fragment("%{#{colors[color_name]}}", true)
    else
      ""
    end
  end

  defp column_width() do
    case :io.columns do
      { :ok, width } -> min(width, 80)
      _              -> 80
    end
  end
end