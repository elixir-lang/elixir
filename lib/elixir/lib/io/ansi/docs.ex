defmodule IO.ANSI.Docs do
  @moduledoc false

  @bullets [?*, ?-, ?+]

  @doc """
  The default options used by this module.

  The supported values are:

  * `:enabled`         - toggles coloring on and off (true)
  * `:doc_code`        - code blocks (cyan, bright)
  * `:doc_inline_code` - inline code (cyan)
  * `:doc_headings`    - h1 and h2 headings (yellow, bright)
  * `:doc_title`       - top level heading (reverse, yellow, bright)
  * `:doc_bold`        - bold text (bright)
  * `:doc_underline`   - underlined text (underline)
  * `:width`           - the width to format the text (80)

  Values for the color settings are strings with
  comma-separated ANSI values.
  """
  def default_options do
    [enabled: true,
     doc_code: "cyan,bright",
     doc_inline_code: "cyan",
     doc_headings: "yellow,bright",
     doc_title: "reverse,yellow,bright",
     doc_bold: "bright",
     doc_underline: "underline",
     width: 80]
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
  end

  @doc """
  Prints the documentation body.

  In addition to the priting string, takes a set of options
  defined in `default_options/1`.
  """
  def print(doc, options \\ []) do
    options = Keyword.merge(default_options, options)
    doc
    |> String.split(["\r\n","\n"], trim: false)
    |> Enum.map(&String.rstrip/1)
    |> process("", options)
  end

  defp process([], _indent, _options), do: nil

  defp process(["# " <> heading | rest], _indent, options) do
    write_h1(String.strip(heading), options)
    process(rest, "", options)
  end

  defp process(["## " <> heading | rest], _indent, options) do
    write_h2(String.strip(heading), options)
    process(rest, "", options)
  end

  defp process(["### " <> heading | rest], indent, options) do
    write_h3(String.strip(heading), indent, options)
    process(rest, indent, options)
  end

  defp process(["" | rest], indent, options) do
    process(rest, indent, options)
  end

  defp process(["    " <> line | rest], indent, options) do
    process_code(rest, [line], indent, options)
  end

  defp process([line | rest], indent, options) do
    {stripped, count} = strip_spaces(line, 0)
    case stripped do
      <<bullet, ?\s, item :: binary >> when bullet in @bullets ->
        process_list(item, rest, count, indent, options)
      _ ->
        process_text(rest, [line], indent, false, options)
    end
  end

  defp strip_spaces(" " <> line, acc) do
    strip_spaces(line, acc + 1)
  end

  defp strip_spaces(rest, acc) do
    {rest, acc}
  end

  ## Headings

  defp write_h1(heading, options) do
    write_h2(String.upcase(heading), options)
  end

  defp write_h2(heading, options) do
    write(:doc_headings, heading, options)
  end

  defp write_h3(heading, indent, options) do
    IO.write(indent)
    write(:doc_headings, heading, options)
  end

  ## Lists

  defp process_list(line, rest, count, indent, options) do
    IO.write indent <> "• "
    {contents, rest, done} = process_list_next(rest, count, false, [])
    process_text(contents, [line], indent <> "  ", true, options)
    if done, do: IO.puts(IO.ANSI.reset)
    process(rest, indent, options)
  end

  # Process the thing after a list item entry. It can be either:
  #
  # * Continuation of the list
  # * A nested list
  # * The end of the list
  #
  defp process_list_next([" " <> _ = line | rest], count, _done, acc) do
    case list_next(line, count) do
      :done    -> {Enum.reverse(acc), [line|rest], false}
      chopped  -> process_list_next(rest, count, false, [chopped|acc])
    end
  end

  defp process_list_next([<<bullet, ?\s, _ :: binary>> | _] = rest, _count, _done, acc) when bullet in @bullets do
    {Enum.reverse(acc), rest, false}
  end

  defp process_list_next(["" | rest], count, _done, acc) do
    process_list_next(rest, count, true, [""|acc])
  end

  defp process_list_next(rest, _count, done, acc) do
    {Enum.reverse(acc), rest, done}
  end

  defp list_next(<<bullet, ?\s, _ :: binary>>, 0) when bullet in @bullets, do: :done
  defp list_next(line, 0),          do: chop(line, 2)
  defp list_next(" " <> line, acc), do: list_next(line, acc - 1)
  defp list_next(line, _acc),       do: line

  defp chop(" " <> line, acc) when acc > 0, do: chop(line, acc - 1)
  defp chop(line, _acc), do: line

  ## Text (paragraphs / lists)

  defp process_text(doc=["" | _], para, indent, from_list, options) do
    write_text(Enum.reverse(para), indent, from_list, options)
    process(doc, indent, options)
  end

  defp process_text([], para, indent, from_list, options) do
    write_text(Enum.reverse(para), indent, from_list, options)
  end

  defp process_text([line | rest], para, indent, true, options) do
    {stripped, count} = strip_spaces(line, 0)
    case stripped do
      <<bullet, ?\s, item :: binary>> when bullet in @bullets ->
        write_text(Enum.reverse(para), indent, true, options)
        process_list(item, rest, count, indent, options)
      _ ->
        process_text(rest, [line | para], indent, true, options)
    end
  end

  defp process_text([line | rest], para, indent, from_list, options) do
    process_text(rest, [line | para], indent, from_list, options)
  end

  defp write_text(lines, indent, from_list, options) do
    lines
    |> Enum.join(" ")
    |> handle_links
    |> handle_inline(nil, [], [], options)
    |> String.split(~r{\s})
    |> write_with_wrap(options[:width] - byte_size(indent), indent, from_list)

    unless from_list, do: IO.puts(IO.ANSI.reset)
  end

  ## Code blocks

  defp process_code([], code, indent, options) do
    write_code(code, indent, options)
  end

  # Blank line between code blocks
  defp process_code([ "", "    " <> line | rest ], code, indent, options) do
    process_code(rest, [line, "" | code], indent, options)
  end

  defp process_code([ "    " <> line | rest ], code, indent, options) do
    process_code(rest, [line|code], indent, options)
  end

  defp process_code(rest, code, indent, options) do
    write_code(code, indent, options)
    process(rest, indent, options)
  end

  defp write_code(code, indent, options) do
    write(:doc_code, "#{indent}┃ #{Enum.join(Enum.reverse(code), "\n#{indent}┃ ")}", options)
  end

  ## Helpers

  defp write(style, string, options) do
    IO.puts color(style, options) <> string <> IO.ANSI.reset
    IO.puts IO.ANSI.reset
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

  defp length_without_escape(<< ?\e, ?[, _, _, ?m, rest :: binary >>, count) do
    length_without_escape(rest, count)
  end

  defp length_without_escape(<< ?\e, ?[, _, ?m, rest :: binary >>, count) do
    length_without_escape(rest, count)
  end

  defp length_without_escape(rest, count) do
    case String.next_grapheme(rest) do
      {_, rest}  -> length_without_escape(rest, count + 1)
      nil -> count
    end
  end

  defp handle_links(text) do
    text
    |> remove_square_brackets_in_link
    |> escape_underlines_in_link
  end

  defp escape_underlines_in_link(text) do
    case Regex.match?(~r{.*(https?\S*)}, text) do
      true ->
        Regex.replace(~r{_}, text, "\\\\_")
      _ ->
        text
    end
  end

  defp remove_square_brackets_in_link(text) do
    Regex.replace(~r{\[(.*?)\]\((.*?)\)}, text, "\\1 (\\2)")
  end

  # Single inline quotes.
  @single [?`, ?_, ?*]

  # ` does not require space in between
  @spaced [?_, ?*]

  # Clauses for handling spaces
  defp handle_inline(<<?*, ?*, ?\s, rest :: binary>>, nil, buffer, acc, options) do
    handle_inline(rest, nil, [?\s, ?*, ?*|buffer], acc, options)
  end

  defp handle_inline(<<mark, ?\s, rest :: binary>>, nil, buffer, acc, options) when mark in @spaced do
    handle_inline(rest, nil, [?\s, mark|buffer], acc, options)
  end

  defp handle_inline(<<?\s, ?*, ?*, rest :: binary>>, limit, buffer, acc, options) do
    handle_inline(rest, limit, [?*, ?*, ?\s|buffer], acc, options)
  end

  defp handle_inline(<<?\s, mark, rest :: binary>>, limit, buffer, acc, options) when mark in @spaced do
    handle_inline(rest, limit, [mark, ?\s|buffer], acc, options)
  end

  # Clauses for handling escape
  defp handle_inline(<<?\\, ?\\, rest :: binary>>, limit, buffer, acc, options) do
    handle_inline(rest, limit, [?\\|buffer], acc, options)
  end

  defp handle_inline(<<?\\, ?*, ?*, rest :: binary>>, limit, buffer, acc, options) do
    handle_inline(rest, limit, [?*, ?*|buffer], acc, options)
  end

  # A escape is not valid inside `
  defp handle_inline(<<?\\, mark, rest :: binary>>, limit, buffer, acc, options)
      when mark in [?_, ?*, ?`] and not(mark == limit and mark == ?`) do
    handle_inline(rest, limit, [mark|buffer], acc, options)
  end

  # Inline start
  defp handle_inline(<<?*, ?*, rest :: binary>>, nil, buffer, acc, options) when rest != "" do
    handle_inline(rest, ?d, ["**"], [Enum.reverse(buffer)|acc], options)
  end

  defp handle_inline(<<mark, rest :: binary>>, nil, buffer, acc, options) when rest != "" and mark in @single do
    handle_inline(rest, mark, [<<mark>>], [Enum.reverse(buffer)|acc], options)
  end

  # Inline end
  defp handle_inline(<<?*, ?*, rest :: binary>>, ?d, buffer, acc, options) do
    handle_inline(rest, nil, [], [inline_buffer(buffer, options)|acc], options)
  end

  defp handle_inline(<<mark, rest :: binary>>, mark, buffer, acc, options) when mark in @single do
    handle_inline(rest, nil, [], [inline_buffer(buffer, options)|acc], options)
  end

  defp handle_inline(<<char, rest :: binary>>, mark, buffer, acc, options) do
    handle_inline(rest, mark, [char|buffer], acc, options)
  end

  defp handle_inline(<<>>, _mark, buffer, acc, _options) do
    IO.iodata_to_binary Enum.reverse([Enum.reverse(buffer)|acc])
  end

  defp inline_buffer(buffer, options) do
    [h|t] = Enum.reverse([IO.ANSI.reset|buffer])
    [color_for(h, options)|t]
  end

  defp color_for("`", colors),  do: color(:doc_inline_code, colors)
  defp color_for("_", colors),  do: color(:doc_underline, colors)
  defp color_for("*", colors),  do: color(:doc_bold, colors)
  defp color_for("**", colors), do: color(:doc_bold, colors)

  defp color(style, colors) do
    color = colors[style]
    enabled = colors[:enabled]
    IO.ANSI.escape_fragment("%{#{color}}", enabled)
  end
end
