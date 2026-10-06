# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule IO.ANSI do
  @moduledoc """
  Functionality to render ANSI escape sequences.

  [ANSI escape sequences](https://en.wikipedia.org/wiki/ANSI_escape_code)
  are characters embedded in text used to control formatting, color, and
  other output options on video text terminals.

  ANSI escapes are typically enabled on all Unix terminals. They are also
  available on Windows consoles from Windows 10, although it must be
  explicitly enabled for the current user in the registry by running the
  following command:

      reg add HKCU\\Console /v VirtualTerminalLevel /t REG_DWORD /d 1

  After running the command above, you must restart your current console.

  ## Examples

  Because the ANSI escape sequences are embedded in text, the normal usage of
  these functions is to concatenate their output with text.

      formatted_text = IO.ANSI.blue_background() <> "Example" <> IO.ANSI.reset()
      IO.puts(formatted_text)

  A higher level and more convenient API is also available via `IO.ANSI.format/1`,
  where you use atoms to represent each ANSI escape sequence and by default
  checks if ANSI is enabled:

      IO.puts(IO.ANSI.format([:blue_background, "Example"]))

  In case ANSI is disabled, the ANSI escape sequences are simply discarded.
  """

  @type ansicode :: atom
  @type ansilist ::
          maybe_improper_list(char | ansicode | binary | ansilist, binary | ansicode | [])
  @type ansidata :: ansilist | ansicode | binary

  @doc """
  Checks if ANSI coloring is supported and enabled on this machine.

  This function simply reads the configuration value for
  `:ansi_enabled` in the `:elixir` application. The value is by
  default `false` unless Elixir can detect during startup that
  both `stdout` and `stderr` are terminals.
  """
  @spec enabled? :: boolean
  def enabled? do
    Application.get_env(:elixir, :ansi_enabled, false)
  end

  @doc """
  Syntax colors to be used by `Inspect`.

  Those colors are used throughout Elixir's standard library,
  such as `dbg/2` and `IEx`.

  The colors can be changed by setting the `:ansi_syntax_colors`
  in the `:elixir` application configuration. Configuration for
  most built-in data types are supported: `:atom`, `:binary`,
  `:boolean`, `:charlist`, `:list`, `:map`, `:nil`, `:number`,
  `:string`, and `:tuple`. The default is:

      [
        atom: :cyan
        boolean: :magenta,
        charlist: :yellow,
        nil: :magenta,
        number: :yellow,
        string: :green
      ]

  """
  @doc since: "1.14.0"
  @spec syntax_colors :: Keyword.t(ansidata)
  def syntax_colors do
    Application.fetch_env!(:elixir, :ansi_syntax_colors)
  end

  @doc "Sets foreground color."
  @spec color(0..255) :: String.t()
  def color(code) when code in 0..255, do: "\e[38;5;#{code}m"

  @doc ~S"""
  Sets the foreground color from individual RGB values.

  Valid values for each color are in the range 0 to 5.
  """
  @spec color(0..5, 0..5, 0..5) :: String.t()
  def color(r, g, b) when r in 0..5 and g in 0..5 and b in 0..5 do
    color(16 + 36 * r + 6 * g + b)
  end

  @doc "Sets background color."
  @spec color_background(0..255) :: String.t()
  def color_background(code) when code in 0..255, do: "\e[48;5;#{code}m"

  @doc ~S"""
  Sets the background color from individual RGB values.

  Valid values for each color are in the range 0 to 5.
  """
  @spec color_background(0..5, 0..5, 0..5) :: String.t()
  def color_background(r, g, b) when r in 0..5 and g in 0..5 and b in 0..5 do
    color_background(16 + 36 * r + 6 * g + b)
  end

  defsequence = fn name, code, terminator ->
    sequence = if terminator, do: "\e[#{code}#{terminator}", else: code

    @spec unquote(name)() :: String.t()
    def unquote(name)() do
      unquote(sequence)
    end

    defp format_sequence(unquote(name)) do
      unquote(sequence)
    end
  end

  @doc "Resets all attributes."
  defsequence.(:reset, 0, "m")

  @doc "Bold or increased intensity."
  @doc since: "1.21.0"
  defsequence.(:bold, 1, "m")

  @doc "Bold or increased intensity (alias for `bold/0`)."
  defsequence.(:bright, 1, "m")

  @doc "Dim or decreased intensity."
  @doc since: "1.21.0"
  defsequence.(:dim, 2, "m")

  @doc "Faint (decreased intensity) (alias for `dim/0`)."
  defsequence.(:faint, 2, "m")

  @doc "Italic: on. Not widely supported. Sometimes treated as inverse."
  defsequence.(:italic, 3, "m")

  @doc "Underline: single."
  defsequence.(:underline, 4, "m")

  @doc "Blink: slow. Less than 150 per minute."
  @doc since: "1.21.0"
  defsequence.(:blink, 5, "m")

  @doc "Blink: slow. Less than 150 per minute."
  @doc deprecated: "Use blink/0 instead."
  defsequence.(:blink_slow, 5, "m")

  @doc "Blink: rapid. MS-DOS ANSI.SYS; 150 per minute or more; not widely supported."
  defsequence.(:blink_rapid, 6, "m")

  @doc "Image: negative. Swap foreground and background."
  defsequence.(:inverse, 7, "m")

  @doc "Image: negative. Swap foreground and background."
  @doc deprecated: "Use inverse/0 instead."
  defsequence.(:reverse, 7, "m")

  @doc "Invisible. Not widely supported."
  @doc since: "1.21.0"
  defsequence.(:invisible, 8, "m")

  @doc "Conceal. Not widely supported."
  @doc deprecated: "Use invisible/0 instead."
  defsequence.(:conceal, 8, "m")

  @doc "Strikethrough. Not widely supported."
  @doc since: "1.21.0"
  defsequence.(:strikethrough, 9, "m")

  @doc "Crossed-out. Characters legible, but marked for deletion. Not widely supported."
  @doc deprecated: "Use strikethrough/0 instead."
  defsequence.(:crossed_out, 9, "m")

  @doc "Sets primary (default) font."
  defsequence.(:primary_font, 10, "m")

  for font_n <- [1, 2, 3, 4, 5, 6, 7, 8, 9] do
    @doc "Sets alternative font #{font_n}."
    defsequence.(String.to_unsafe_atom("font_#{font_n}"), font_n + 10, "m")
  end

  @doc "Normal color or intensity."
  @doc since: "1.21.0"
  defsequence.(:bold_off, 22, "m")

  @doc "Normal color or intensity (alias for `normal/0`)."
  @doc since: "1.21.0"
  defsequence.(:dim_off, 22, "m")

  @doc "Normal color or intensity (alias for `normal/0`)."
  defsequence.(:normal, 22, "m")

  @doc "Italic: off."
  @doc since: "1.21.0"
  defsequence.(:italic_off, 23, "m")

  @doc "Not italic."
  @doc deprecated: "Use italic_off/0 instead."
  defsequence.(:not_italic, 23, "m")

  @doc "Underline: off."
  @doc since: "1.21.0"
  defsequence.(:underline_off, 24, "m")

  @doc "Underline: none."
  @doc deprecated: "Use underline_off/0 instead."
  defsequence.(:no_underline, 24, "m")

  @doc "Blink: off."
  defsequence.(:blink_off, 25, "m")

  @doc "Image: positive. Normal foreground and background."
  defsequence.(:inverse_off, 27, "m")

  @doc "Image: positive. Normal foreground and background."
  @doc deprecated: "Use inverse_off/0 instead."
  defsequence.(:reverse_off, 27, "m")

  @doc "Invisible: off."
  @doc since: "1.21.0"
  defsequence.(:invisible_off, 28, "m")

  @doc "Reveal: Not concealed."
  @doc deprecated: "Use invisible_off/0 instead."
  defsequence.(:reveal, 28, "m")

  @doc "Strikethrough: off."
  @doc since: "1.21.0"
  defsequence.(:strikethrough_off, 29, "m")

  @doc "Not crossed-out."
  @doc deprecated: "Use strikethrough_off/0 instead."
  defsequence.(:not_crossed_out, 29, "m")

  colors = [:black, :red, :green, :yellow, :blue, :magenta, :cyan, :white]

  for {color, code} <- Enum.with_index(colors) do
    @doc "Sets foreground color to #{color}."
    defsequence.(color, code + 30, "m")

    @doc "Sets foreground color to light #{color}."
    defsequence.(String.to_unsafe_atom("light_#{color}"), code + 90, "m")

    @doc "Sets background color to #{color}."
    defsequence.(String.to_unsafe_atom("#{color}_background"), code + 40, "m")

    @doc "Sets background color to light #{color}."
    defsequence.(String.to_unsafe_atom("light_#{color}_background"), code + 100, "m")
  end

  @doc "Default text color."
  defsequence.(:default_color, 39, "m")

  @doc "Default background color."
  defsequence.(:default_background, 49, "m")

  @doc "Framed."
  defsequence.(:framed, 51, "m")

  @doc "Encircled."
  defsequence.(:encircled, 52, "m")

  @doc "Overlined."
  @doc since: "1.21.0"
  defsequence.(:overline, 53, "m")

  @doc "Overlined."
  @doc deprecated: "Use overline/0 instead."
  defsequence.(:overlined, 53, "m")

  @doc "Not framed or encircled."
  defsequence.(:not_framed_encircled, 54, "m")

  @doc "Not overlined."
  @doc since: "1.21.0"
  defsequence.(:overline_off, 55, "m")

  @doc "Not overlined."
  @doc deprecated: "Use overline_off/0 instead."
  defsequence.(:not_overlined, 55, "m")

  @doc "Clears screen."
  defsequence.(:clear, "2", "J")

  @doc "Clears line."
  defsequence.(:clear_line, "2", "K")

  @doc "Sends cursor home."
  @doc since: "1.21.0"
  defsequence.(:cursor_home, "", "H")

  @doc "Sends cursor home."
  @doc deprecated: "Use cursor_home/0 instead."
  defsequence.(:home, "", "H")

  @doc """
  Sends cursor to the absolute position specified by `line` and `column`.

  Line `0` and column `0` would mean the top left corner.
  """
  @spec cursor(non_neg_integer, non_neg_integer) :: String.t()
  def cursor(line, column)
      when is_integer(line) and line >= 0 and is_integer(column) and column >= 0 do
    "\e[#{line};#{column}H"
  end

  @doc "Sends cursor `lines` up."
  @spec cursor_up(pos_integer) :: String.t()
  def cursor_up(lines \\ 1) when is_integer(lines) and lines >= 1, do: "\e[#{lines}A"

  @doc "Sends cursor `lines` down."
  @spec cursor_down(pos_integer) :: String.t()
  def cursor_down(lines \\ 1) when is_integer(lines) and lines >= 1, do: "\e[#{lines}B"

  @doc "Sends cursor one column forward."
  @doc since: "1.21.0"
  defsequence.(:cursor_forward, "", "C")

  @doc "Sends cursor one column to the right."
  @doc deprecated: "Use cursor_forward/0 instead."
  @spec cursor_right() :: String.t()
  def cursor_right(), do: "\e[1C"

  @doc "Sends cursor `columns` to the right."
  @spec cursor_right(pos_integer) :: String.t()
  def cursor_right(columns) when is_integer(columns) and columns >= 1, do: "\e[#{columns}C"

  @doc "Sends cursor one column backward."
  @doc since: "1.21.0"
  defsequence.(:cursor_backward, "\b", nil)

  @doc "Sends cursor one column to the left."
  @doc deprecated: "Use cursor_backward/0 instead."
  @spec cursor_left() :: String.t()
  def cursor_left(), do: "\e[1D"

  @doc "Sends cursor `columns` to the left."
  @spec cursor_left(pos_integer) :: String.t()
  def cursor_left(columns) when is_integer(columns) and columns >= 1, do: "\e[#{columns}D"

  defp format_sequence(other) do
    raise ArgumentError, "invalid ANSI sequence specification: #{inspect(other)}"
  end

  @doc ~S"""
  Formats a chardata-like argument by converting named ANSI sequences into actual
  ANSI codes.

  The named sequences are represented by atoms.

  It will also append an `IO.ANSI.reset/0` to the chardata when a conversion is
  performed. If you don't want this behavior, use `format_fragment/2`.

  An optional boolean parameter can be passed to enable or disable
  emitting actual ANSI codes. When `false`, no ANSI codes will be emitted.
  By default checks if ANSI is enabled using the `enabled?/0` function.

  An `ArgumentError` will be raised if an invalid ANSI code is provided.

  ## Examples

      iex> IO.ANSI.format(["Hello, ", :red, :bright, "world!"], true)
      [[[[[[], "Hello, "] | "\e[31m"] | "\e[1m"], "world!"] | "\e[0m"]

  """
  @spec format(ansidata, boolean) :: IO.chardata()
  def format(ansidata, emit? \\ enabled?()) when is_boolean(emit?) do
    do_format(ansidata, [], [], emit?, :maybe)
  end

  @doc ~S"""
  Formats a chardata-like argument by converting named ANSI sequences into actual
  ANSI codes.

  The named sequences are represented by atoms.

  An optional boolean parameter can be passed to enable or disable
  emitting actual ANSI codes. When `false`, no ANSI codes will be emitted.
  By default checks if ANSI is enabled using the `enabled?/0` function.

  ## Examples

      iex> IO.ANSI.format_fragment([:bright, ~c"Word"], true)
      [[[[[[] | "\e[1m"], 87], 111], 114], 100]

  """
  @spec format_fragment(ansidata, boolean) :: IO.chardata()
  def format_fragment(ansidata, emit? \\ enabled?()) when is_boolean(emit?) do
    do_format(ansidata, [], [], emit?, false)
  end

  defp do_format([term | rest], rem, acc, emit?, append_reset) do
    do_format(term, [rest | rem], acc, emit?, append_reset)
  end

  defp do_format(term, rem, acc, true, append_reset) when is_atom(term) do
    do_format([], rem, [acc | format_sequence(term)], true, !!append_reset)
  end

  defp do_format(term, rem, acc, false, append_reset) when is_atom(term) do
    format_sequence(term)
    do_format([], rem, acc, false, append_reset)
  end

  defp do_format(term, rem, acc, emit?, append_reset) when not is_list(term) do
    do_format([], rem, [acc, term], emit?, append_reset)
  end

  defp do_format([], [next | rest], acc, emit?, append_reset) do
    do_format(next, rest, acc, emit?, append_reset)
  end

  defp do_format([], [], acc, true, true) do
    [acc | reset()]
  end

  defp do_format([], [], acc, _emit?, _append_reset) do
    acc
  end
end
