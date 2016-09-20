defmodule IO.ANSI.Sequence do
  @moduledoc false

  defmacro defsequence(name, code, terminator \\ "m") do
    quote bind_quoted: [name: name, code: code, terminator: terminator] do
      def unquote(name)() do
        "\e[#{unquote(code)}#{unquote(terminator)}"
      end

      defp format_sequence(unquote(name)) do
        unquote(name)()
      end
    end
  end
end

defmodule IO.ANSI do
  @moduledoc """
  Functionality to render ANSI escape sequences.

  [ANSI escape sequences](https://en.wikipedia.org/wiki/ANSI_escape_code)
  are characters embedded in text used to control formatting, color, and
  other output options on video text terminals.
  """

  import IO.ANSI.Sequence

  @typep ansicode :: atom
  @typep ansilist :: maybe_improper_list(char | ansicode | binary | ansilist, binary | ansicode | [])
  @type  ansidata :: ansilist | ansicode | binary

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

  @doc "Sets foreground color."
  @spec color(0..255) :: String.t
  def color(code) when code in 0..255, do: "\e[38;5;#{code}m"

  @doc ~S"""
  Sets the foreground color from individual RGB values.

  Valid values for each color are in the range 0 to 5.
  """
  @spec color(0..5, 0..5, 0..5) :: String.t
  def color(r, g, b) when r in 0..5 and g in 0..5 and b in 0..5 do
    color(16 + (36 * r) + (6 * g) + b)
  end

  @doc "Sets background color."
  @spec color_background(0..255) :: String.t
  def color_background(code) when code in 0..255, do: "\e[48;5;#{code}m"

  @doc ~S"""
  Sets the background color from individual RGB values.

  Valid values for each color are in the range 0 to 5.
  """
  @spec color_background(0..5, 0..5, 0..5) :: String.t
  def color_background(r, g, b) when r in 0..5 and g in 0..5 and b in 0..5 do
    color_background(16 + (36 * r) + (6 * g) + b)
  end

  @doc "Resets all attributes."
  defsequence :reset, 0

  @doc "Bright (increased intensity) or bold."
  defsequence :bright, 1

  @doc "Faint (decreased intensity). Not widely supported."
  defsequence :faint, 2

  @doc "Italic: on. Not widely supported. Sometimes treated as inverse."
  defsequence :italic, 3

  @doc "Underline: single."
  defsequence :underline, 4

  @doc "Blink: slow. Less than 150 per minute."
  defsequence :blink_slow, 5

  @doc "Blink: rapid. MS-DOS ANSI.SYS; 150 per minute or more; not widely supported."
  defsequence :blink_rapid, 6

  @doc "Image: negative. Swap foreground and background."
  defsequence :inverse, 7

  @doc "Image: negative. Swap foreground and background."
  defsequence :reverse, 7

  @doc "Conceal. Not widely supported."
  defsequence :conceal, 8

  @doc "Crossed-out. Characters legible, but marked for deletion. Not widely supported."
  defsequence :crossed_out, 9

  @doc "Sets primary (default) font."
  defsequence :primary_font, 10

  for font_n <- [1, 2, 3, 4, 5, 6, 7, 8, 9] do
    @doc "Sets alternative font #{font_n}."
    defsequence :"font_#{font_n}", font_n + 10
  end

  @doc "Normal color or intensity."
  defsequence :normal, 22

  @doc "Not italic."
  defsequence :not_italic, 23

  @doc "Underline: none."
  defsequence :no_underline, 24

  @doc "Blink: off."
  defsequence :blink_off, 25

  @doc "Image: positive. Normal foreground and background."
  defsequence :inverse_off, 27

  @doc "Image: positive. Normal foreground and background."
  defsequence :reverse_off, 27

  colors = [:black, :red, :green, :yellow, :blue, :magenta, :cyan, :white]

  for {color, code} <- Enum.with_index(colors) do
    @doc "Sets foreground color to #{color}."
    defsequence color, code + 30

    @doc "Sets foreground color to light #{color}."
    defsequence :"light_#{color}", code + 90

    @doc "Sets background color to #{color}."
    defsequence :"#{color}_background", code + 40

    @doc "Sets background color to light #{color}."
    defsequence :"light_#{color}_background", code + 100
  end

  @doc "Default text color."
  defsequence :default_color, 39

  @doc "Default background color."
  defsequence :default_background, 49

  @doc "Framed."
  defsequence :framed, 51

  @doc "Encircled."
  defsequence :encircled, 52

  @doc "Overlined."
  defsequence :overlined, 53

  @doc "Not framed or encircled."
  defsequence :not_framed_encircled, 54

  @doc "Not overlined."
  defsequence :not_overlined, 55

  @doc "Sends cursor home."
  defsequence :home, "", "H"

  @doc "Clears screen."
  defsequence :clear, "2", "J"

  @doc "Clears line."
  defsequence :clear_line, "2", "K"

  defp format_sequence(other) do
    raise ArgumentError, "invalid ANSI sequence specification: #{inspect other}"
  end

  @doc ~S"""
  Formats a chardata-like argument by converting named ANSI sequences into actual
  ANSI codes.

  The named sequences are represented by atoms.

  It will also append an `IO.ANSI.reset/0` to the chardata when a conversion is
  performed. If you don't want this behaviour, use `format_fragment/2`.

  An optional boolean parameter can be passed to enable or disable
  emitting actual ANSI codes. When `false`, no ANSI codes will emitted.
  By default checks if ANSI is enabled using the `enabled?/0` function.

  ## Examples

      iex> IO.ANSI.format(["Hello, ", :red, :bright, "world!"], true)
      [[[[[[], "Hello, "] | "\e[31m"] | "\e[1m"], "world!"] | "\e[0m"]

  """
  def format(chardata, emit? \\ enabled?()) when is_boolean(emit?) do
    do_format(chardata, [], [], emit?, :maybe)
  end

  @doc ~S"""
  Formats a chardata-like argument by converting named ANSI sequences into actual
  ANSI codes.

  The named sequences are represented by atoms.

  An optional boolean parameter can be passed to enable or disable
  emitting actual ANSI codes. When `false`, no ANSI codes will emitted.
  By default checks if ANSI is enabled using the `enabled?/0` function.

  ## Examples

      iex> IO.ANSI.format_fragment([:bright, 'Word'], true)
      [[[[[[] | "\e[1m"], 87], 111], 114], 100]

  """
  def format_fragment(chardata, emit? \\ enabled?()) when is_boolean(emit?) do
    do_format(chardata, [], [], emit?, false)
  end

  defp do_format([term | rest], rem, acc, emit?, append_reset) do
    do_format(term, [rest | rem], acc, emit?, append_reset)
  end

  defp do_format(term, rem, acc, true, append_reset) when is_atom(term) do
    do_format([], rem, [acc | format_sequence(term)], true, !!append_reset)
  end

  defp do_format(term, rem, acc, false, append_reset) when is_atom(term) do
    do_format([], rem, acc, false, append_reset)
  end

  defp do_format(term, rem, acc, emit?, append_reset) when not is_list(term) do
    do_format([], rem, [acc, term], emit?, append_reset)
  end

  defp do_format([], [next | rest], acc, emit?, append_reset) do
    do_format(next, rest, acc, emit?, append_reset)
  end

  defp do_format([], [], acc, true, true) do
    [acc | IO.ANSI.reset]
  end

  defp do_format([], [], acc, _emit?, _append_reset) do
    acc
  end
end
