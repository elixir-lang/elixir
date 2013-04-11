defmodule IO.ANSI.Sequence do
  @moduledoc false

  defmacro defsequence(name, code) do
    binary_name = atom_to_binary(name)

    quote do
      def unquote(name)() do
        "\e[#{unquote(code)}m"
      end

      defp escape_sequence(<< unquote(binary_name), rest :: binary >>) do
        { "\e[#{unquote(code)}m", rest }
      end
    end
  end
end

defmodule IO.ANSI do
  @moduledoc """
  This module provides functionality to render ANSI escape sequences
  (http://en.wikipedia.org/wiki/ANSI_escape_code) —  characters embedded
  in the text used to control formatting, color, and other output options
  on video text terminals.

  Please be aware that in Erlang/OTP versions prior to R16, you will not
  be able to render ANSI escape sequences in iex or erlang shell
  """

  import IO.ANSI.Sequence

  @doc """
  Checks whether the default I/O device is a terminal or a file.

  Used to identify whether printing ANSI escape sequences will likely
  be printed as intended.

  Please note that invoked while in shell (iex) in Erlang/OTP
  prior to R16, terminal?/0 will always return false because
  Erlang shell did not support ANSI escape sequences up until
  R16.
  """
  @spec terminal? :: boolean
  @spec terminal?(:io.device) :: boolean
  def terminal?(device // :erlang.group_leader) do
    if :erlang.system_info(:otp_release) < 'R16' and
       Process.whereis(:user) != device do
      # Shell prior to R16 doesn't support ANSI escape
      # sequences
      false
    else
      match?({:ok, _}, :io.columns(device))
    end
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

  lc font_n inlist [1,2,3,4,5,6,7,8,9] do
    Module.eval_quoted __ENV__, (quote do
      @doc "Sets alternative font #{unquote(font_n)}"
      defsequence(unquote(:"font_#{font_n}"), unquote(font_n + 10))
    end)
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

  lc { code, color } inlist colors do
    Module.eval_quoted __ENV__, (quote do
      @doc "Sets foreground color to #{unquote(color)}"
      defsequence unquote(color), unquote(code + 30)

      @doc "Sets background color to #{unquote(color)}"
      defsequence unquote(:"#{color}_background"), unquote(code + 40)
    end)
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

  # Catch spaces between codes
  defp escape_sequence(<< ?\s, rest :: binary >>) do
    escape_sequence(rest)
  end

  defp escape_sequence(other) do
    [spec|_] = String.split(other, %r/(,|\})/)
    raise ArgumentError, message: "invalid ANSI sequence specification: #{spec}"
  end

  @doc %B"""
  Escapes a string coverting named ANSI sequences into actual ANSI codes.

  The format for referring sequences is `%{red}` and `%{red,bright}` (for
  multiple sequences)

  It will also force a %{reset} to get appended to every string. If you don't
  want this behaviour, use `escape_fragment/1` and `escape_fragment/2`.

  An optional boolean parameter can be passed to enable or disable
  emitting actual ANSI codes. When false, no ANSI codes will emitted.
  By default, standard output will be checked if it is a terminal capable
  of handling these sequences (using `terminal?/0` function)

  ## Example

    iex> IO.ANSI.escape "Hello %{red,bright,green}yes"
    "Hello \e[31m\e[1m\e[32myes\e[0m"
  """
  @spec escape(String.t, emit :: boolean) :: String.t
  def escape(string, emit // terminal?) do
    {rendered, emitted} = do_escape(string, false, emit, false, [])
    if emitted and emit do
      rendered <> reset
    else
      rendered
    end
  end

  @doc %B"""
  Escapes a string coverting named ANSI sequences into actual ANSI codes.

  The format for referring sequences is `%{red}` and `%{red,bright}` (for
  multiple sequences)

  An optional boolean parameter can be passed to enable or disable
  emitting actual ANSI codes. When false, no ANSI codes will emitted.
  By default, standard output will be checked if it is a terminal capable
  of handling these sequences (using `terminal?/0` function)

  ## Example

    iex> IO.ANSI.escape "Hello %{red,bright,green}yes"
    "Hello \e[31m\e[1m\e[32myes\e[0m"

  """
  @spec escape_fragment(String.t, emit :: boolean) :: String.t
  def escape_fragment(string, emit // terminal?) do
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
    {list_to_binary(Enum.reverse(acc)), emitted}
  end

  defp do_escape_sequence(rest, emit, acc) do
    {code, rest} = escape_sequence(rest)
    if emit do
      acc = [code|acc]
    end
    do_escape(rest, true, emit, true, acc)
  end
end
