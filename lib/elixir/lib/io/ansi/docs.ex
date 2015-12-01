defmodule IO.ANSI.Docs do
  @moduledoc false

  @bullets [?*, ?-, ?+]

  @doc """
  The default options used by this module.

  The supported values are:

    * `:enabled`           - toggles coloring on and off (true)
    * `:doc_bold`          - bold text (bright)
    * `:doc_code`          - code blocks (cyan, bright)
    * `:doc_headings`      - h1 and h2 headings (yellow, bright)
    * `:doc_inline_code`   - inline code (cyan)
    * `:doc_table_heading` - style for table headings
    * `:doc_title`         - top level heading (reverse, yellow, bright)
    * `:doc_underline`     - underlined text (underline)
    * `:width`             - the width to format the text (80)

  Values for the color settings are strings with
  comma-separated ANSI values.
  """
  def default_options do
    [enabled:           true,
     doc_bold:          [:bright],
     doc_code:          [:cyan, :bright],
     doc_headings:      [:yellow],
     doc_inline_code:   [:cyan],
     doc_table_heading: [:reverse],
     doc_title:         [:reverse, :yellow],
     doc_underline:     [:underline],
     width:             80]
  end

  @doc """
  Prints the head of the documentation (i.e. the function signature).

  See `default_options/0` for docs on the supported options.
  """
  def print_heading(heading, options \\ []) do
    IO.puts IO.ANSI.reset
    options = Keyword.merge(default_options, options)
    width   = options[:width]
    padding = div(width + String.length(heading), 2)
    heading = heading |> String.rjust(padding) |> String.ljust(width)
    write(:doc_title, heading, options)
    newline_after_block
  end

  @doc """
  Prints the documentation body.

  In addition to the printing string, takes a set of options
  defined in `default_options/1`.
  """
  def print(doc, options \\ []) do
    options = Keyword.merge(default_options, options)
    doc
    |> String.split(["\r\n", "\n"], trim: false)
    |> Enum.map(&String.rstrip/1)
    |> process([], "", options)
  end

  defp process([], text, indent, options) do
    write_text(text, indent, options)
  end

  defp process(["# " <> heading | rest], text, indent, options) do
    write_text(text, indent, options)
    write_h1(String.strip(heading), options)
    process(rest, [], "", options)
  end

  defp process(["## " <> heading | rest], text, indent, options) do
    write_text(text, indent, options)
    write_h2(String.strip(heading), options)
    process(rest, [], "", options)
  end

  defp process(["### " <> heading | rest], text, indent, options) do
    write_text(text, indent, options)
    write_h3(String.strip(heading), indent, options)
    process(rest, [], "", options)
  end

  defp process(["" | rest], text, indent, options) do
    write_text(text, indent, options)
    process(rest, [], indent, options)
  end

  defp process(["    " <> line | rest], text, indent, options) do
    write_text(text, indent, options)
    process_code(rest, [line], indent, options)
  end

  defp process(["```" <> _line | rest], text, indent, options) do
    process_fenced_code_block(rest, text, indent, options, _delimiter = "```")
  end

  defp process(["~~~" <> _line | rest], text, indent, options) do
    process_fenced_code_block(rest, text, indent, options, _delimiter = "~~~")
  end

  defp process(all=[line | rest], text, indent, options) do
    {stripped, count} = strip_spaces(line, 0, :infinity)
    cond do
      link_label?(stripped, count) ->
        write_text([line], indent, options, true)
        process(rest, text, indent, options)
      table_line?(stripped) and rest != [] and table_line?(hd(rest)) ->
        write_text(text, indent, options)
        process_table(all, indent, options)
      true ->
        process_rest(stripped, rest, count, text, indent, options)
    end
  end

  ## Headings

  defp write_h1(heading, options) do
    write_h2(String.upcase(heading), options)
  end

  defp write_h2(heading, options) do
    write(:doc_headings, heading, options)
    newline_after_block
  end

  defp write_h3(heading, indent, options) do
    IO.write(indent)
    write(:doc_headings, heading, options)
    newline_after_block
  end

  ## Lists

  defp process_rest(stripped, rest, count, text, indent, options) do
    case stripped do
      <<bullet, ?\s, item::binary>> when bullet in @bullets ->
        write_text(text, indent, options)
        process_list("• ", item, rest, count, indent, options)
      <<d1, ?., ?\s, item::binary>> when d1 in ?0..?9 ->
        write_text(text, indent, options)
        process_list(<<d1, ?., ?\s>>, item, rest, count, indent, options)
      <<d1, d2, ?., ?\s, item::binary>> when d1 in ?0..?9 and d2 in ?0..?9 ->
        write_text(text, indent, options)
        process_list(<<d1, d2, ?., ?\s>>, item, rest, count, indent, options)
      _ ->
        process(rest, [stripped | text], indent, options)
    end
  end

  defp process_list(entry, line, rest, count, indent, options) do
    # The first list always win some extra padding
    if indent == "", do: entry = "  " <> entry
    new_indent = indent <> String.duplicate(" ", String.length(entry))

    {contents, rest, done} = process_list_next(rest, count, byte_size(new_indent), [])
    process(contents, [indent <> entry <> line, :no_wrap], new_indent, options)

    if done, do: newline_after_block()
    process(rest, [], indent, options)
  end

  defp process_list_next([line | rest], count, max, acc) do
    {stripped, next_count} = strip_spaces(line, 0, max)
    case process_list_next_kind(stripped, rest, count, next_count) do
      :next -> process_list_next(rest, count, max, [stripped | acc])
      :done -> {Enum.reverse(acc), [line | rest], true}
      :list -> {Enum.reverse(acc), [line | rest], false}
    end
  end

  defp process_list_next([], _count, _max, acc) do
    {Enum.reverse(acc), [], true}
  end

  defp process_list_next_kind(stripped, rest, count, next_count) do
    case {stripped, rest} do
      {<<bullet, ?\s, _::binary>>, _} when bullet in @bullets and next_count <= count ->
        :list
      {<<d1, ?., ?\s, _::binary>>, _} when d1 in ?0..?9 and next_count <= count ->
        :list
      {<<d1, d2, ?., ?\s, _::binary>>, _} when d1 in ?0..?9 and d2 in ?0..?9 and next_count <= count ->
        :list
      {"", [" " <> _ | _]} ->
        :next
      {"", _} ->
        :done
      _ ->
        :next
    end
  end

  ## Text

  defp write_text(text, indent, options) do
    case Enum.reverse(text) do
      [:no_wrap|rest] -> write_text(rest, indent, options, true)
      rest -> write_text(rest, indent, options, false)
    end
  end

  defp write_text([], _indent, _options, _no_wrap) do
    :ok
  end

  defp write_text(lines, indent, options, no_wrap) do
    lines
    |> Enum.join(" ")
    |> handle_links
    |> handle_inline(options)
    |> String.split(~r{\s})
    |> write_with_wrap(options[:width] - byte_size(indent), indent, no_wrap)

    unless no_wrap, do: newline_after_block()
  end

  ## Code blocks

  defp process_code([], code, indent, options) do
    write_code(code, indent, options)
  end

  # Blank line between code blocks
  defp process_code(["", "    " <> line | rest], code, indent, options) do
    process_code(rest, [line, "" | code], indent, options)
  end

  defp process_code(["    " <> line | rest], code, indent, options) do
    process_code(rest, [line|code], indent, options)
  end

  defp process_code(rest, code, indent, options) do
    write_code(code, indent, options)
    process(rest, [], indent, options)
  end

  defp process_fenced_code_block(rest, text, indent, options, delimiter) do
    write_text(text, indent, options)
    process_fenced_code(rest, [], indent, options, delimiter)
  end

  defp process_fenced_code([], code, indent, options, _delimiter) do
    write_code(code, indent, options)
  end

  defp process_fenced_code([line | rest], code, indent, options, delimiter) do
    if line === delimiter do
      process_code(rest, code, indent, options)
    else
      process_fenced_code(rest, [line|code], indent, options, delimiter)
    end
  end

  defp write_code(code, indent, options) do
    write(:doc_code, "#{indent}┃ #{Enum.join(Enum.reverse(code), "\n#{indent}┃ ")}", options)
    newline_after_block
  end

  ## Tables

  defp process_table(lines, indent, options) do
    {table, rest} = Enum.split_while(lines, &table_line?/1)
    table_lines(table, options)
    newline_after_block
    process(rest, [], indent, options)
  end

  defp table_lines(lines, options) do
    lines = Enum.map(lines, &split_into_columns(&1, options))
    count = Enum.map(lines, &length/1) |> Enum.max
    lines = Enum.map(lines, &pad_to_number_of_columns(&1, count))

    widths = for line <- lines, do:
              (for {_col, length} <- line, do: length)

    col_widths = Enum.reduce(widths,
                             List.duplicate(0, count),
                             &max_column_widths/2)

    render_table(lines, col_widths, options)
  end

  defp split_into_columns(line, options) do
    line
    |> String.strip(?|)
    |> String.strip()
    |> String.split(~r/\s\|\s/)
    |> Enum.map(&render_column(&1, options))
  end

  defp render_column(col, options) do
    col = col
          |> String.replace(~r/\\ \|/x, "|")
          |> handle_links
          |> handle_inline(options)
    {col, length_without_escape(col, 0)}
  end

  defp pad_to_number_of_columns(cols, col_count),
    do: cols ++ List.duplicate({"", 0}, col_count - length(cols))

  defp max_column_widths(cols, widths),
    do: Enum.zip(cols, widths) |> Enum.map(fn {a, b} -> max(a, b) end)

  # If second line is heading separator, use the heading style on the first
  defp render_table([first, second | rest], widths, options) do
    combined = Enum.zip(first, widths)
    if table_header?(second) do
      draw_table_row(combined, options, :heading)
      render_table(rest, widths, options)
    else
      draw_table_row(combined, options)
      render_table([second | rest], widths, options)
    end
  end

  defp render_table([first | rest], widths, options) do
    combined = Enum.zip(first, widths)
    draw_table_row(combined, options)
    render_table(rest, widths, options)
  end

  defp render_table([], _, _),
    do: nil

  defp table_header?(row), do:
    Enum.all?(row, fn {col, _} -> col =~ ~r/^:?-+:?$/ end)

  defp draw_table_row(cols_and_widths, options, heading \\ false) do
    columns =
      Enum.map_join(cols_and_widths, " | ", fn {{col, length}, width} ->
        col <> String.duplicate(" ", width - length)
      end)

    if heading do
      write(:doc_table_heading, columns, options)
    else
      IO.puts columns
    end
  end

  defp table_line?(line) do
    Regex.match?(~r'''
      ( ^ \s{0,3} \| (?: [^|]+ \|)+ \s* $ )
    |
      (\s \| \s)
    '''x, line)
  end

  ## Helpers

  defp link_label?("[" <> rest, count) when count <= 3, do: link_label?(rest)
  defp link_label?(_, _), do: false

  defp link_label?("]: " <> _), do: true
  defp link_label?("]" <> _), do: false
  defp link_label?(""), do: false
  defp link_label?(<<_>> <> rest), do: link_label?(rest)

  defp strip_spaces(" " <> line, acc, max) when acc < max,
    do: strip_spaces(line, acc + 1, max)
  defp strip_spaces(rest, acc, _max),
    do: {rest, acc}

  defp write(style, string, options) do
    IO.puts [color(style, options), string, IO.ANSI.reset]
  end

  defp write_with_wrap([], _available, _indent, _first) do
    :ok
  end

  defp write_with_wrap(words, available, indent, first) do
    {words, rest} = take_words(words, available, [])
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
        {[word], words}

      # Otherwise
      true ->
        {Enum.reverse(acc), [word|words]}
    end
  end

  defp take_words([], _available, acc) do
    {Enum.reverse(acc), []}
  end

  defp length_without_escape(<<?\e, ?[, _, _, ?m>> <> rest, count) do
    length_without_escape(rest, count)
  end

  defp length_without_escape(<<?\e, ?[, _, ?m>> <> rest, count) do
    length_without_escape(rest, count)
  end

  defp length_without_escape(rest, count) do
    case String.next_grapheme(rest) do
      {_, rest} -> length_without_escape(rest, count + 1)
      nil -> count
    end
  end

  defp handle_links(text) do
    text
    |> remove_square_brackets_in_link
    |> escape_underlines_in_link
  end

  defp escape_underlines_in_link(text) do
    Regex.replace(~r{https?\S*}, text, &String.replace(&1, "_", "\\_"))
  end

  defp remove_square_brackets_in_link(text) do
    Regex.replace(~r{\[(.*?)\]\((.*?)\)}, text, "\\1 (\\2)")
  end


  # We have four entries: **, *, _ and `.
  #
  # The first three behave the same while the last one is simpler
  # when it comes to delimiters. But, since the first has two
  # characters, we need to handle 3 cases:
  #
  # 1. **
  # 2. _ and *
  # 3. `
  #
  # Where the first two should have the same code but match differently.
  @single [?_, ?*]

  # Characters that can mark the beginning or the end of a word.
  # Only support the most common ones at this moment.
  @delimiters [?\s, ?', ?", ?!, ?@, ?#, ?$, ?%, ?^, ?&, ?-, ?+, ?(, ?), ?[, ?], ?{, ?}, ?<, ?>, ?.]

  # Inline start

  defp handle_inline(<<?*, ?*, rest::binary>>, options) do
    handle_inline(rest, ?d, ["**"], [], options)
  end

  defp handle_inline(<<mark, rest::binary>>, options) when mark in @single do
    handle_inline(rest, mark, [<<mark>>], [], options)
  end

  defp handle_inline(rest, options) do
    handle_inline(rest, nil, [], [], options)
  end

  # Inline delimiters

  defp handle_inline(<<delimiter, ?*, ?*, rest::binary>>, nil, buffer, acc, options)
      when rest != "" and delimiter in @delimiters do
    handle_inline(rest, ?d, ["**"], [delimiter, Enum.reverse(buffer)|acc], options)
  end

  defp handle_inline(<<delimiter, mark, rest::binary>>, nil, buffer, acc, options)
      when rest != "" and delimiter in @delimiters and mark in @single do
    handle_inline(rest, mark, [<<mark>>], [delimiter, Enum.reverse(buffer)|acc], options)
  end

  defp handle_inline(<<?`, rest::binary>>, nil, buffer, acc, options)
      when rest != "" do
    handle_inline(rest, ?`, ["`"], [Enum.reverse(buffer)|acc], options)
  end

  # Clauses for handling escape

  defp handle_inline(<<?\\, ?\\, ?*, ?*, rest::binary>>, nil, buffer, acc, options)
      when rest != "" do
    handle_inline(rest, ?d, ["**"], [?\\, Enum.reverse(buffer)|acc], options)
  end

  defp handle_inline(<<?\\, ?\\, mark, rest::binary>>, nil, buffer, acc, options)
      when rest != "" and mark in @single do
    handle_inline(rest, mark, [<<mark>>], [?\\, Enum.reverse(buffer)|acc], options)
  end

  defp handle_inline(<<?\\, ?\\, rest::binary>>, limit, buffer, acc, options) do
    handle_inline(rest, limit, [?\\|buffer], acc, options)
  end

  # An escape is not valid inside `
  defp handle_inline(<<?\\, mark, rest::binary>>, limit, buffer, acc, options)
      when not(mark == limit and mark == ?`) do
    handle_inline(rest, limit, [mark|buffer], acc, options)
  end

  # Inline end

  defp handle_inline(<<?*, ?*, delimiter, rest::binary>>, ?d, buffer, acc, options)
      when delimiter in @delimiters do
    handle_inline(<<delimiter, rest::binary>>, nil, [], [inline_buffer(buffer, options)|acc], options)
  end

  defp handle_inline(<<mark, delimiter, rest::binary>>, mark, buffer, acc, options)
      when delimiter in @delimiters and mark in @single do
    handle_inline(<<delimiter, rest::binary>>, nil, [], [inline_buffer(buffer, options)|acc], options)
  end

  defp handle_inline(<<?*, ?*, rest::binary>>, ?d, buffer, acc, options)
      when rest == "" do
    handle_inline(<<>>, nil, [], [inline_buffer(buffer, options)|acc], options)
  end

  defp handle_inline(<<mark, rest::binary>>, mark, buffer, acc, options)
      when rest == "" and mark in @single do
    handle_inline(<<>>, nil, [], [inline_buffer(buffer, options)|acc], options)
  end

  defp handle_inline(<<?`, rest::binary>>, ?`, buffer, acc, options) do
    handle_inline(rest, nil, [], [inline_buffer(buffer, options)|acc], options)
  end

  # Catch all

  defp handle_inline(<<char, rest::binary>>, mark, buffer, acc, options) do
    handle_inline(rest, mark, [char|buffer], acc, options)
  end

  defp handle_inline(<<>>, _mark, buffer, acc, _options) do
    IO.iodata_to_binary Enum.reverse([Enum.reverse(buffer)|acc])
  end

  defp inline_buffer(buffer, options) do
    [h|t] = Enum.reverse([IO.ANSI.reset|buffer])
    [color_for(h, options)|t]
  end

  defp color_for(mark, colors) do
    case mark do
      "`"  -> color(:doc_inline_code, colors)
      "_"  -> color(:doc_underline, colors)
      "*"  -> color(:doc_bold, colors)
      "**" -> color(:doc_bold, colors)
    end
  end

  defp color(style, colors) do
    color = colors[style]
    IO.ANSI.format_fragment(color, colors[:enabled])
  end

  defp newline_after_block, do: IO.puts(IO.ANSI.reset)
end
