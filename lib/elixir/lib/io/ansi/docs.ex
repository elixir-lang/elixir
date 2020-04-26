defmodule IO.ANSI.Docs do
  @moduledoc false

  @bullet_text "• "
  @bullets [?*, ?-, ?+]
  @spaces [" ", "\n", "\t"]

  @doc """
  The default options used by this module.

  The supported keys are:

    * `:enabled`           - toggles coloring on and off (true)
    * `:doc_bold`          - bold text (bright)
    * `:doc_code`          - code blocks (cyan)
    * `:doc_headings`      - h1, h2, h3, h4, h5, h6 headings (yellow)
    * `:doc_metadata`      - documentation metadata keys (yellow)
    * `:doc_quote`         - leading quote character `> ` (light black)
    * `:doc_inline_code`   - inline code (cyan)
    * `:doc_table_heading` - the style for table headings
    * `:doc_title`         - top level heading (reverse, yellow)
    * `:doc_underline`     - underlined text (underline)
    * `:width`             - the width to format the text (80)

  Values for the color settings are strings with
  comma-separated ANSI values.
  """
  @spec default_options() :: keyword
  def default_options do
    [
      enabled: true,
      doc_bold: [:bright],
      doc_code: [:cyan],
      doc_headings: [:yellow],
      doc_metadata: [:yellow],
      doc_quote: [:light_black],
      doc_inline_code: [:cyan],
      doc_table_heading: [:reverse],
      doc_title: [:reverse, :yellow],
      doc_underline: [:underline],
      width: 80
    ]
  end

  @doc """
  Prints the head of the documentation (i.e. the function signature).

  See `default_options/0` for docs on the supported options.
  """
  @spec print_headings([String.t()], keyword) :: :ok
  def print_headings(headings, options \\ []) do
    options = Keyword.merge(default_options(), options)
    newline_after_block(options)
    width = options[:width]

    for heading <- headings do
      padding = div(width + String.length(heading), 2)
      heading = String.pad_leading(heading, padding)
      heading = if options[:enabled], do: String.pad_trailing(heading, width), else: heading
      write(:doc_title, heading, options)
    end

    newline_after_block(options)
  end

  @doc """
  Prints documentation metadata (only `delegate_to`, `deprecated`, `guard`, and `since` for now).

  See `default_options/0` for docs on the supported options.
  """
  @spec print_metadata(map, keyword) :: :ok
  def print_metadata(metadata, options \\ []) when is_map(metadata) do
    options = Keyword.merge(default_options(), options)
    print_each_metadata(metadata, options) && IO.write("\n")
  end

  @metadata_filter [:deprecated, :guard, :since]

  defp print_each_metadata(metadata, options) do
    Enum.reduce(metadata, false, fn
      {key, value}, _printed when is_binary(value) and key in @metadata_filter ->
        label = metadata_label(key, options)
        indent = String.duplicate(" ", length_without_escape(label, 0) + 1)
        write_with_wrap([label | String.split(value, @spaces)], options[:width], indent, true, "")

      {key, value}, _printed when is_boolean(value) and key in @metadata_filter ->
        IO.puts([metadata_label(key, options), ?\s, to_string(value)])

      {:delegate_to, {m, f, a}}, _printed ->
        label = metadata_label(:delegate_to, options)
        IO.puts([label, ?\s, Exception.format_mfa(m, f, a)])

      _metadata, printed ->
        printed
    end)
  end

  defp metadata_label(key, options) do
    "#{color(:doc_metadata, options)}#{key}:#{maybe_reset(options)}"
  end

  @doc """
  Prints the documentation body `doc` according to `format`.

  It takes a set of `options` defined in `default_options/0`.
  """
  @spec print(term(), String.t(), keyword) :: :ok
  def print(doc, format, options \\ [])

  def print(doc, "text/markdown", options) when is_binary(doc) and is_list(options) do
    print_markdown(doc, options)
  end

  def print(doc, "application/erlang+html", options) when is_list(options) do
    print_erlang_html(doc, options)
  end

  def print(_doc, format, options) when is_binary(format) and is_list(options) do
    IO.puts("\nUnknown documentation format #{inspect(format)}\n")
  end

  ## Erlang+html

  def print_erlang_html(doc, options) do
    options = Keyword.merge(default_options(), options)
    IO.write(traverse_erlang_html(doc, "", options))
  end

  defp traverse_erlang_html(text, _indent, _options) when is_binary(text) do
    text
  end

  defp traverse_erlang_html(nodes, indent, options) when is_list(nodes) do
    for node <- nodes do
      traverse_erlang_html(node, indent, options)
    end
  end

  defp traverse_erlang_html({:div, [class: class] ++ _, entries}, indent, options) do
    prefix = indent <> quote_prefix(options)

    content =
      entries
      |> traverse_erlang_html(indent, options)
      |> IO.iodata_to_binary()
      |> String.trim_trailing()

    [
      prefix,
      class |> to_string() |> String.upcase(),
      "\n#{prefix}\n#{prefix}" | String.replace(content, "\n", "\n#{prefix}")
    ]
    |> newline_cons()
  end

  defp traverse_erlang_html({:p, _, entries}, indent, options) do
    [indent | handle_erlang_html_text(entries, indent, options)]
  end

  defp traverse_erlang_html({:h1, _, entries}, indent, options) do
    entries |> traverse_erlang_html(indent, options) |> heading(1, options) |> newline_cons()
  end

  defp traverse_erlang_html({:h2, _, entries}, indent, options) do
    entries |> traverse_erlang_html(indent, options) |> heading(2, options) |> newline_cons()
  end

  defp traverse_erlang_html({:h3, _, entries}, indent, options) do
    entries |> traverse_erlang_html(indent, options) |> heading(3, options) |> newline_cons()
  end

  defp traverse_erlang_html({:h4, _, entries}, indent, options) do
    entries |> traverse_erlang_html(indent, options) |> heading(4, options) |> newline_cons()
  end

  defp traverse_erlang_html({:h5, _, entries}, indent, options) do
    entries |> traverse_erlang_html(indent, options) |> heading(5, options) |> newline_cons()
  end

  defp traverse_erlang_html({:h6, _, entries}, indent, options) do
    entries |> traverse_erlang_html(indent, options) |> heading(6, options) |> newline_cons()
  end

  defp traverse_erlang_html({:br, _, []}, _indent, _options) do
    []
  end

  defp traverse_erlang_html({:i, _, entries}, indent, options) do
    inline_text("_", traverse_erlang_html(entries, indent, options), options)
  end

  defp traverse_erlang_html({:em, _, entries}, indent, options) do
    inline_text("*", traverse_erlang_html(entries, indent, options), options)
  end

  defp traverse_erlang_html({:code, _, entries}, indent, options) do
    inline_text("`", traverse_erlang_html(entries, indent, options), options)
  end

  defp traverse_erlang_html({:pre, _, [{:code, _, entries}]}, indent, options) do
    string =
      entries
      |> traverse_erlang_html(indent, options)
      |> IO.iodata_to_binary()

    ["#{indent}    ", String.replace(string, "\n", "\n#{indent}    ")] |> newline_cons()
  end

  defp traverse_erlang_html({:a, attributes, entries}, indent, options) do
    if href = attributes[:href] do
      [traverse_erlang_html(entries, indent, options), ?\s, ?(, href, ?)]
    else
      traverse_erlang_html(entries, indent, options)
    end
  end

  defp traverse_erlang_html({:dl, _, entries}, indent, options) do
    traverse_erlang_html(entries, indent, options)
  end

  defp traverse_erlang_html({:dt, _, entries}, indent, options) do
    ["#{indent}  ", @bullet_text | handle_erlang_html_text(entries, indent <> "    ", options)]
  end

  defp traverse_erlang_html({:dd, _, entries}, indent, options) do
    ["#{indent}    " | handle_erlang_html_text(entries, indent <> "    ", options)]
  end

  defp traverse_erlang_html({:ul, attributes, entries}, indent, options) do
    if attributes[:class] == "types" do
      types =
        for {:li, _, lines} <- entries,
            line <- lines,
            do: ["#{indent}    ", line, ?\n]

      ["#{indent}Typespecs:\n\n", types, ?\n]
    else
      for {:li, _, lines} <- entries do
        ["#{indent}  ", @bullet_text | handle_erlang_html_text(lines, indent <> "    ", options)]
      end
    end
  end

  defp traverse_erlang_html({:ol, _, entries}, indent, options) do
    for {{:li, _, lines}, i} <- Enum.with_index(entries, 1) do
      [
        "#{indent}  ",
        Integer.to_string(i),
        ". " | handle_erlang_html_text(lines, indent <> "     ", options)
      ]
    end
  end

  defp traverse_erlang_html({tag, _, entries}, indent, options) do
    [
      indent <> "<#{tag}>\n",
      traverse_erlang_html(entries, indent <> "    ", options)
      |> IO.iodata_to_binary()
      |> String.trim_trailing(),
      "\n" <> indent <> "</#{tag}>"
    ]
    |> newline_cons()
  end

  defp newline_cons(text) do
    [text | "\n\n"]
  end

  defp handle_erlang_html_text(entries, indent, options) do
    if Enum.all?(entries, &inline_html?/1) do
      entries
      |> traverse_erlang_html(indent, options)
      |> IO.iodata_to_binary()
      |> String.split(@spaces)
      |> wrap_text(options[:width], indent, true, "", [])
      |> tl()
      |> newline_cons()
    else
      entries
      |> traverse_erlang_html(indent, options)
      |> IO.iodata_to_binary()
      |> String.trim_leading()
    end
  end

  defp inline_html?(binary) when is_binary(binary), do: true
  defp inline_html?({tag, _, _}) when tag in [:a, :code, :em, :i, :br], do: true
  defp inline_html?(_), do: false

  ## Markdown

  def print_markdown(doc, options) do
    options = Keyword.merge(default_options(), options)

    doc
    |> String.split(["\r\n", "\n"], trim: false)
    |> Enum.map(&String.trim_trailing/1)
    |> process([], "", options)
  end

  defp process([], text, indent, options) do
    write_text(text, indent, options)
  end

  defp process(["# " <> _ = heading | rest], text, indent, options) do
    write_heading(heading, rest, text, indent, options)
  end

  defp process(["## " <> _ = heading | rest], text, indent, options) do
    write_heading(heading, rest, text, indent, options)
  end

  defp process(["### " <> _ = heading | rest], text, indent, options) do
    write_heading(heading, rest, text, indent, options)
  end

  defp process(["#### " <> _ = heading | rest], text, indent, options) do
    write_heading(heading, rest, text, indent, options)
  end

  defp process(["##### " <> _ = heading | rest], text, indent, options) do
    write_heading(heading, rest, text, indent, options)
  end

  defp process(["###### " <> _ = heading | rest], text, indent, options) do
    write_heading(heading, rest, text, indent, options)
  end

  defp process([">" <> line | rest], text, indent, options) do
    write_text(text, indent, options)
    process_quote(rest, [line], indent, options)
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

  defp process(all = [line | rest], text, indent, options) do
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

  ### Headings

  defp write_heading(heading, rest, text, indent, options) do
    write_text(text, indent, options)
    write(:doc_headings, heading, options)
    newline_after_block(options)
    process(rest, [], "", options)
  end

  ### Quotes

  defp process_quote([], lines, indent, options) do
    write_quote(lines, indent, options, false)
  end

  defp process_quote([">", ">" <> line | rest], lines, indent, options) do
    write_quote(lines, indent, options, true)
    write_empty_quote_line(options)
    process_quote(rest, [line], indent, options)
  end

  defp process_quote([">" <> line | rest], lines, indent, options) do
    process_quote(rest, [line | lines], indent, options)
  end

  defp process_quote(rest, lines, indent, options) do
    write_quote(lines, indent, options, false)
    process(rest, [], indent, options)
  end

  defp write_quote(lines, indent, options, no_wrap) do
    lines
    |> Enum.map(&String.trim/1)
    |> Enum.reverse()
    |> write_lines(
      indent,
      options,
      no_wrap,
      quote_prefix(options)
    )
  end

  defp write_empty_quote_line(options) do
    options
    |> quote_prefix()
    |> IO.puts()
  end

  ### Lists

  defp process_rest(stripped, rest, count, text, indent, options) do
    case stripped do
      <<bullet, ?\s, item::binary>> when bullet in @bullets ->
        write_text(text, indent, options)
        process_list(@bullet_text, item, rest, count, indent, options)

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
    entry = if indent == "", do: "  " <> entry, else: entry
    new_indent = indent <> String.duplicate(" ", String.length(entry))

    {contents, rest, done} =
      process_list_next(rest, count, byte_size(new_indent) - byte_size(indent), [])

    process(contents, [indent <> entry <> line, :no_wrap], new_indent, options)

    if done, do: newline_after_block(options)
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

      {<<d1, d2, ?., ?\s, _::binary>>, _}
      when d1 in ?0..?9 and d2 in ?0..?9 and next_count <= count ->
        :list

      {"", [" " <> _ | _]} ->
        :next

      {"", _} ->
        :done

      _ ->
        :next
    end
  end

  ### Text

  defp write_text(text, indent, options) do
    case Enum.reverse(text) do
      [:no_wrap | rest] -> write_text(rest, indent, options, true)
      rest -> write_text(rest, indent, options, false)
    end
  end

  defp write_text([], _indent, _options, _no_wrap) do
    :ok
  end

  defp write_text(lines, indent, options, no_wrap) do
    write_lines(lines, indent, options, no_wrap, "")
  end

  defp write_lines(lines, indent, options, no_wrap, prefix) do
    lines
    |> Enum.join(" ")
    |> format_text(options)
    |> String.split(@spaces)
    |> write_with_wrap(options[:width] - byte_size(indent), indent, no_wrap, prefix)

    unless no_wrap, do: newline_after_block(options)
  end

  defp format_text(text, options) do
    text
    |> handle_links()
    |> handle_inline(options)
  end

  ### Code blocks

  defp process_code([], code, indent, options) do
    write_code(code, indent, options)
  end

  # Blank line between code blocks
  defp process_code(["", "    " <> line | rest], code, indent, options) do
    process_code(rest, [line, "" | code], indent, options)
  end

  defp process_code(["    " <> line | rest], code, indent, options) do
    process_code(rest, [line | code], indent, options)
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
      process_fenced_code(rest, [line | code], indent, options, delimiter)
    end
  end

  defp write_code(code, indent, options) do
    write(:doc_code, "#{indent}    #{Enum.join(Enum.reverse(code), "\n#{indent}    ")}", options)
    newline_after_block(options)
  end

  ### Tables

  defp process_table(lines, indent, options) do
    {table, rest} = Enum.split_while(lines, &table_line?/1)
    table_lines(table, options)
    newline_after_block(options)
    process(rest, [], indent, options)
  end

  defp table_lines(lines, options) do
    lines = Enum.map(lines, &split_into_columns(&1, options))
    count = Enum.map(lines, &length/1) |> Enum.max()
    lines = Enum.map(lines, &pad_to_number_of_columns(&1, count))

    widths =
      for line <- lines do
        if table_header?(line) do
          for _ <- line, do: 0
        else
          for {_col, length} <- line, do: length
        end
      end

    col_widths = Enum.reduce(widths, List.duplicate(0, count), &max_column_widths/2)
    render_table(lines, col_widths, options)
  end

  defp split_into_columns(line, options) do
    line
    |> String.trim(" ")
    |> String.trim("|")
    |> String.split("|")
    |> Enum.map(&render_column(&1, options))
  end

  defp render_column(col, options) do
    col =
      col
      |> String.trim()
      |> String.replace("\\\|", "|")
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
      alignments = Enum.map(second, &column_alignment/1)
      options = Keyword.put_new(options, :alignments, alignments)
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

  defp render_table([], _, _), do: nil

  defp column_alignment({line, _}) do
    cond do
      String.starts_with?(line, ":") and String.ends_with?(line, ":") -> :center
      String.ends_with?(line, ":") -> :right
      true -> :left
    end
  end

  defp table_header?(line) do
    Enum.all?(line, fn {col, _} -> table_header_column?(col) end)
  end

  defp table_header_column?(":" <> rest), do: table_header_contents?(rest)
  defp table_header_column?(col), do: table_header_contents?(col)

  defp table_header_contents?("-" <> rest), do: table_header_contents?(rest)
  defp table_header_contents?(":"), do: true
  defp table_header_contents?(""), do: true
  defp table_header_contents?(_), do: false

  defp draw_table_row(cols_and_widths, options, heading \\ false) do
    default_alignments = List.duplicate(:left, length(cols_and_widths))
    alignments = Keyword.get(options, :alignments, default_alignments)

    columns =
      cols_and_widths
      |> Enum.zip(alignments)
      |> Enum.map_join(" | ", &generate_table_cell/1)

    if heading do
      write(:doc_table_heading, columns, options)
    else
      IO.puts(columns)
    end
  end

  defp generate_table_cell({{{col, length}, width}, :center}) do
    ansi_diff = byte_size(col) - length
    width = width + ansi_diff

    col
    |> String.pad_leading(div(width, 2) - div(length, 2) + length)
    |> String.pad_trailing(width + 1 - rem(width, 2))
  end

  defp generate_table_cell({{{col, length}, width}, :right}) do
    ansi_diff = byte_size(col) - length
    String.pad_leading(col, width + ansi_diff)
  end

  defp generate_table_cell({{{col, length}, width}, :left}) do
    ansi_diff = byte_size(col) - length
    String.pad_trailing(col, width + ansi_diff)
  end

  defp table_line?(line) do
    line =~ ~r/[:\ -]\|[:\ -]/
  end

  ## Helpers

  defp link_label?("[" <> rest, count) when count <= 3, do: link_label?(rest)
  defp link_label?(_, _), do: false

  defp link_label?("]: " <> _), do: true
  defp link_label?("]" <> _), do: false
  defp link_label?(""), do: false
  defp link_label?(<<_>> <> rest), do: link_label?(rest)

  defp strip_spaces(" " <> line, acc, max) when acc < max, do: strip_spaces(line, acc + 1, max)
  defp strip_spaces(rest, acc, _max), do: {rest, acc}

  defp write(style, string, options) do
    IO.puts([color(style, options), string, maybe_reset(options)])
  end

  defp write_with_wrap([], _available, _indent, _first, _prefix) do
    :ok
  end

  defp write_with_wrap(words, available, indent, first, prefix) do
    words
    |> wrap_text(available, indent, first, prefix, [])
    |> tl()
    |> IO.puts()
  end

  defp wrap_text([], _available, _indent, _first, _prefix, wrapped_lines) do
    Enum.reverse(wrapped_lines)
  end

  defp wrap_text(words, available, indent, first, prefix, wrapped_lines) do
    prefix_length = length_without_escape(prefix, 0)
    {words, rest} = take_words(words, available - prefix_length, [])
    line = [if(first, do: "", else: indent), prefix, Enum.join(words, " ")]

    wrap_text(rest, available, indent, false, prefix, [line, ?\n | wrapped_lines])
  end

  defp take_words([word | words], available, acc) do
    available = available - length_without_escape(word, 0)

    cond do
      # It fits, take one for space and continue decreasing
      available > 0 ->
        take_words(words, available - 1, [word | acc])

      # No space but we got no words
      acc == [] ->
        {[word], words}

      # Otherwise
      true ->
        {Enum.reverse(acc), [word | words]}
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
    # Regular expression adapted from https://tools.ietf.org/html/rfc3986#appendix-B
    Regex.replace(~r{[a-z][a-z0-9\+\-\.]*://\S*}i, text, &String.replace(&1, "_", "\\_"))
  end

  defp remove_square_brackets_in_link(text) do
    Regex.replace(~r{\[([^\]]*?)\]\((.*?)\)}, text, "\\1 (\\2)")
  end

  # We have four entries: **, *, _ and `.
  #
  # The first three behave the same while the last one is simpler
  # when it comes to delimiters as it ignores spaces and escape
  # characters. But, since the first has two characters, we need to
  # handle 3 cases:
  #
  # 1. **
  # 2. _ and *
  # 3. `
  #
  # Where the first two should have the same code but match differently.
  @single [?_, ?*]

  # Characters that can mark the beginning or the end of a word.
  # Only support the most common ones at this moment.
  @delimiters [?\s, ?', ?", ?!, ?@, ?#, ?$, ?%, ?^, ?&] ++
                [?-, ?+, ?(, ?), ?[, ?], ?{, ?}, ?<, ?>, ?.]

  ### Inline start

  defp handle_inline(<<?*, ?*, rest::binary>>, options) do
    handle_inline(rest, ?d, ["**"], [], options)
  end

  defp handle_inline(<<mark, rest::binary>>, options) when mark in @single do
    handle_inline(rest, mark, [<<mark>>], [], options)
  end

  defp handle_inline(rest, options) do
    handle_inline(rest, nil, [], [], options)
  end

  ### Inline delimiters

  defp handle_inline(<<delimiter, ?*, ?*, rest::binary>>, nil, buffer, acc, options)
       when rest != "" and delimiter in @delimiters do
    handle_inline(rest, ?d, ["**"], [delimiter, Enum.reverse(buffer) | acc], options)
  end

  defp handle_inline(<<delimiter, mark, rest::binary>>, nil, buffer, acc, options)
       when rest != "" and delimiter in @delimiters and mark in @single do
    handle_inline(rest, mark, [<<mark>>], [delimiter, Enum.reverse(buffer) | acc], options)
  end

  defp handle_inline(<<?`, rest::binary>>, nil, buffer, acc, options)
       when rest != "" do
    handle_inline(rest, ?`, ["`"], [Enum.reverse(buffer) | acc], options)
  end

  ### Clauses for handling escape

  defp handle_inline(<<?\\, ?\\, ?*, ?*, rest::binary>>, nil, buffer, acc, options)
       when rest != "" do
    handle_inline(rest, ?d, ["**"], [?\\, Enum.reverse(buffer) | acc], options)
  end

  defp handle_inline(<<?\\, ?\\, mark, rest::binary>>, nil, buffer, acc, options)
       when rest != "" and mark in @single do
    handle_inline(rest, mark, [<<mark>>], [?\\, Enum.reverse(buffer) | acc], options)
  end

  defp handle_inline(<<?\\, ?\\, rest::binary>>, limit, buffer, acc, options) do
    handle_inline(rest, limit, [?\\ | buffer], acc, options)
  end

  # An escape is not valid inside `
  defp handle_inline(<<?\\, mark, rest::binary>>, limit, buffer, acc, options) when limit != ?` do
    handle_inline(rest, limit, [mark | buffer], acc, options)
  end

  ### Inline end

  defp handle_inline(<<?*, ?*, delimiter, rest::binary>>, ?d, buffer, acc, options)
       when delimiter in @delimiters do
    inline_buffer = inline_buffer(buffer, options)
    handle_inline(<<delimiter, rest::binary>>, nil, [], [inline_buffer | acc], options)
  end

  defp handle_inline(<<mark, delimiter, rest::binary>>, mark, buffer, acc, options)
       when delimiter in @delimiters and mark in @single do
    inline_buffer = inline_buffer(buffer, options)
    handle_inline(<<delimiter, rest::binary>>, nil, [], [inline_buffer | acc], options)
  end

  defp handle_inline(<<?*, ?*, rest::binary>>, ?d, buffer, acc, options)
       when rest == "" do
    handle_inline(<<>>, nil, [], [inline_buffer(buffer, options) | acc], options)
  end

  defp handle_inline(<<mark, rest::binary>>, mark, buffer, acc, options)
       when rest == "" and mark in @single do
    handle_inline(<<>>, nil, [], [inline_buffer(buffer, options) | acc], options)
  end

  defp handle_inline(<<?`, rest::binary>>, ?`, buffer, acc, options) do
    handle_inline(rest, nil, [], [inline_buffer(buffer, options) | acc], options)
  end

  ### Catch all

  defp handle_inline(<<char, rest::binary>>, mark, buffer, acc, options) do
    handle_inline(rest, mark, [char | buffer], acc, options)
  end

  defp handle_inline(<<>>, _mark, buffer, acc, _options) do
    IO.iodata_to_binary(Enum.reverse([Enum.reverse(buffer) | acc]))
  end

  defp inline_buffer(buffer, options) do
    [mark | t] = Enum.reverse(buffer)
    inline_text(mark, t, options)
  end

  ## Helpers

  defp quote_prefix(options), do: "#{color(:doc_quote, options)}> #{maybe_reset(options)}"

  defp heading(text, n, options) do
    [color(:doc_headings, options), String.duplicate("#", n), " ", text, maybe_reset(options)]
  end

  defp inline_text(mark, text, options) do
    if options[:enabled] do
      [[color_for(mark, options) | text] | IO.ANSI.reset()]
    else
      [[mark | text] | mark]
    end
  end

  defp color_for(mark, colors) do
    case mark do
      "`" -> color(:doc_inline_code, colors)
      "_" -> color(:doc_underline, colors)
      "*" -> color(:doc_bold, colors)
      "**" -> color(:doc_bold, colors)
    end
  end

  defp color(style, colors) do
    IO.ANSI.format_fragment(colors[style], colors[:enabled])
  end

  defp newline_after_block(options) do
    IO.puts(maybe_reset(options))
  end

  defp maybe_reset(options) do
    if options[:enabled], do: IO.ANSI.reset(), else: ""
  end
end
