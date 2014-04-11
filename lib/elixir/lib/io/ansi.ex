defmodule IO.ANSI.Sequence do
  @moduledoc false

  defmacro defsequence(name, code \\ "", terminator \\ "m") do
    quote bind_quoted: [name: name, code: code, terminator: terminator] do
      def unquote(name)() do
        "\e[#{unquote(code)}#{unquote(terminator)}"
      end

      defp escape_sequence(<< unquote(atom_to_binary(name)), rest :: binary >>) do
        { "\e[#{unquote(code)}#{unquote(terminator)}", rest }
      end
    end
  end
end

defmodule IO.ANSI do
  @moduledoc """
  Functionality to render ANSI escape sequences
  (http://en.wikipedia.org/wiki/ANSI_escape_code) —  characters embedded
  in text used to control formatting, color, and other output options
  on video text terminals.
  """

  import IO.ANSI.Sequence

  @doc """
  Checks whether the default I/O device is a terminal or a file.

  Used to identify whether printing ANSI escape sequences will likely
  be displayed as intended.
  """
  @spec terminal? :: boolean
  @spec terminal?(:io.device) :: boolean
  def terminal?(device \\ :erlang.group_leader) do
    match?({:ok, _}, :io.columns(device))
  end

  @doc "Resets all attributes"
  defsequence :reset, 0

  @doc "Bright (increased intensity) or Bold"
  defsequence :bright, 1

  @doc "Faint (decreased intensity), not widely supported"
  defsequence :faint, 2

  @doc "Italic: on. Not widely supported. Sometimes treated as inverse."
  defsequence :italic, 3

  @doc "Underline: Single"
  defsequence :underline, 4

  @doc "Blink: Slow. Less than 150 per minute"
  defsequence :blink_slow, 5

  @doc "Blink: Rapid. MS-DOS ANSI.SYS; 150 per minute or more; not widely supported"
  defsequence :blink_rapid, 6

  @doc "Image: Negative. Swap foreground and background"
  defsequence :inverse, 7

  @doc "Image: Negative. Swap foreground and background"
  defsequence :reverse, 7

  @doc "Conceal. Not widely supported"
  defsequence :conceal, 8

  @doc "Crossed-out. Characters legible, but marked for deletion. Not widely supported."
  defsequence :crossed_out, 9

  @doc "Sets primary (default) font"
  defsequence :primary_font, 10

  for font_n <- [1, 2, 3, 4, 5, 6, 7, 8, 9] do
    @doc "Sets alternative font #{font_n}"
    defsequence :"font_#{font_n}", font_n + 10
  end

  @doc "Normal color or intensity"
  defsequence :normal, 22

  @doc "Not italic"
  defsequence :not_italic, 23

  @doc "Underline: None"
  defsequence :no_underline, 24

  @doc "Blink: off"
  defsequence :blink_off, 25

  colors = [:black, :red, :green, :yellow, :blue, :magenta, :cyan, :white]
  colors = Enum.zip(0..(length(colors)-1), colors)

  for { code, color } <- colors do
    @doc "Sets foreground color to #{color}"
    defsequence color, code + 30

    @doc "Sets background color to #{color}"
    defsequence :"#{color}_background", code + 40
  end

  @doc "Default text color"
  defsequence :default_color, 39

  @doc "Default background color"
  defsequence :default_background, 49

  @doc "Framed"
  defsequence :framed, 51

  @doc "Encircled"
  defsequence :encircled, 52

  @doc "Overlined"
  defsequence :overlined, 53

  @doc "Not framed or encircled"
  defsequence :not_framed_encircled, 54

  @doc "Not overlined"
  defsequence :not_overlined, 55

  @doc "Send cursor home"
  defsequence :home, "", "H"

  @doc "Clear screen"
  defsequence :clear, "2", "J"


  # Catch spaces between codes
  defp escape_sequence(<< ?\s, rest :: binary >>) do
    escape_sequence(rest)
  end

  defp escape_sequence(other) do
    [spec|_] = String.split(other, ~r/(,|\})/)
    raise ArgumentError, message: "invalid ANSI sequence specification: #{spec}"
  end

  @doc ~S"""
  Escapes a string by converting named ANSI sequences into actual ANSI codes.

  The format for referring to sequences is `%{red}` and `%{red,bright}` (for
  multiple sequences).

  It will also append a `%{reset}` to the string. If you don't want this
  behaviour, use `escape_fragment/2`.

  An optional boolean parameter can be passed to enable or disable
  emitting actual ANSI codes. When `false`, no ANSI codes will emitted.
  By default, standard output will be checked if it is a terminal capable
  of handling these sequences (using `terminal?/1` function)

  ## Examples

      iex> IO.ANSI.escape("Hello %{red,bright,green}yes", true)
      "Hello \e[31m\e[1m\e[32myes\e[0m"

  """
  @spec escape(String.t, emit :: boolean) :: String.t
  def escape(string, emit \\ terminal?) do
    {rendered, emitted} = do_escape(string, false, emit, false, [])
    if emitted and emit do
      rendered <> reset
    else
      rendered
    end
  end

  @doc ~S"""
  Escapes a string by converting named ANSI sequences into actual ANSI codes.

  The format for referring to sequences is `%{red}` and `%{red,bright}` (for
  multiple sequences).

  An optional boolean parameter can be passed to enable or disable
  emitting actual ANSI codes. When `false`, no ANSI codes will emitted.
  By default, standard output will be checked if it is a terminal capable
  of handling these sequences (using `terminal?/1` function)

  ## Examples

      iex> IO.ANSI.escape_fragment("Hello %{red,bright,green}yes", true)
      "Hello \e[31m\e[1m\e[32myes"

      iex> IO.ANSI.escape_fragment("%{reset}bye", true)
      "\e[0mbye"

  """
  @spec escape_fragment(String.t, emit :: boolean) :: String.t
  def escape_fragment(string, emit \\ terminal?) do
    {rendered, _emitted} = do_escape(string, false, emit, false, [])
    rendered
  end

  defp do_escape(<< ?%, ?{, rest :: binary >>, false, emit, _emitted, acc) do
    do_escape_sequence(rest, emit, acc)
  end
  defp do_escape(<< ?,, rest :: binary >>, true, emit, _emitted, acc) do
    do_escape_sequence(rest, emit, acc)
  end
  defp do_escape(<< ?\s, rest :: binary >>, true, emit, emitted, acc) do
    do_escape(rest, true, emit, emitted, acc)
  end
  defp do_escape(<< ?}, rest :: binary >>, true, emit, emitted, acc) do
    do_escape(rest, false, emit, emitted, acc)
  end
  defp do_escape(<< x :: [binary, size(1)], rest :: binary>>, false, emit, emitted, acc) do
    do_escape(rest, false, emit, emitted, [x|acc])
  end
  defp do_escape("", false, _emit, emitted, acc) do
    {iolist_to_binary(Enum.reverse(acc)), emitted}
  end

  defp do_escape_sequence(rest, emit, acc) do
    {code, rest} = escape_sequence(rest)
    if emit do
      acc = [code|acc]
    end
    do_escape(rest, true, emit, true, acc)
  end
end
