defmodule IO.ANSI.Sequence do
  @moduledoc false
  defmacro __using__(_) do
    quote do
      import IO.ANSI.Sequence
    end
  end
  defmacro defsequence({name, _, [code]}) do
    defsequence(name, code)
  end
  def defsequence(name, code) do
    binary_name = atom_to_binary(name)
    quote do
      #@sequences {unquote(name), unquote(code), @doc}
      def unquote(name)() do
        "\e[#{unquote(code)}m"
      end
      defp escape_sequence(<< unquote(binary_name), rest :: binary >>) do
        {"\e[#{unquote(code)}m", rest}
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
  use IO.ANSI.Sequence

  terminal_doc = """
  Checks whether the default I/O device is a terminal or a file.

  Used to identify whether printing ANSI escape sequences will likely
  be printed as intended
  """
  @doc terminal_doc
  @spec terminal? :: boolean
  def terminal?, do: match?({:ok, _}, :io.columns)

  @doc terminal_doc
  @spec terminal?(:io.device) :: boolean
  def terminal?(device), do: match?({:ok, _}, :io.columns(device))


  @doc "Resets all attributes"
  defsequence reset(0)

  @doc "Bright (increased intensity) or Bold"
  defsequence bright(1)

  @doc "Faint (decreased intensity), not widely supported"
  defsequence faint(2)

  @doc "Italic: on. Not widely supported. Sometimes treated as inverse."
  defsequence italic(3)

  @doc "Underline: Single"
  defsequence underline(4)

  @doc "Blink: Slow. Less than 150 per minute"
  defsequence blink_slow(5)

  @doc "Blink: Rapid. MS-DOS ANSI.SYS; 150 per minute or more; not widely supported"
  defsequence blink_rapid(6)

  @doc "Image: Negative. Swap foreground and background"
  defsequence inverse(7)
  @doc "Image: Negative. Swap foreground and background"
  defsequence reverse(7)

  @doc "Conceal. Not widely supported"
  defsequence conceal(8)

  @doc "Crossed-out. Characters legible, but marked for deletion. Not widely supported."
  defsequence crossed_out(9)

  @doc "Sets primary (default) font"
  defsequence primary_font(10)

  lc font_n inlist [1,2,3,4,5,6,7,8,9] do
    Module.eval_quoted __MODULE__, quote do: @doc "Sets alternative font #{unquote(font_n)}"
    Module.eval_quoted __MODULE__, defsequence(binary_to_atom("font_#{font_n}"), font_n + 10)    
  end

  @doc "Normal color or intensity"
  defsequence normal(22)

  @doc "Not italic"
  defsequence not_italic(23)

  @doc "Underline: None"
  defsequence no_underline(24)

  @doc "Blink: off"
  defsequence blink_off(25)


  colors = [:black, :red, :green, :yellow, :blue, :magenta, :cyan, :white]
  colors = Enum.zip(0..length(colors)-1, colors)
  lc {code, color} inlist colors do
    bgcolor = binary_to_atom("#{color}_background")
    Module.eval_quoted __MODULE__, quote do: @doc "Sets foreground color to #{unquote(color)}"
    Module.eval_quoted __MODULE__, defsequence(color, code + 30)
    Module.eval_quoted __MODULE__, quote do: @doc "Sets background color to #{unquote(color)}"
    Module.eval_quoted __MODULE__, defsequence(bgcolor, code + 40)
  end

  @doc "Default text color"
  defsequence default_color(39)

  @doc "Default background color"
  defsequence default_background(49)

  @doc "Framed"
  defsequence framed(51)

  @doc "Encircled"
  defsequence encircled(52)

  @doc "Overlined"
  defsequence overlined(53)

  @doc "Not framed or encircled"
  defsequence not_framed_encircled(54)

  @doc "Not overlined"
  defsequence not_overlined(55)

  # Catch spaces between codes
  defp escape_sequence(<< ?\s, rest :: binary >>) do
    escape_sequence(rest)
  end

  escape_doc = """
  Escapes a string coverting named ANSI sequences into actual ANSI codes.

  The format for referring sequences is `#[red]` and `#[red,bright]` (for
  multiple sequences)

  It will also force a #[reset] to get appended to every string. If you don't
  want this behaviour, use `escape_fragment/1` and `escape_fragment/2`.

  An optional boolean parameter can be passed to enable or disable
  emitting actual ANSI codes. When false, no ANSI codes will emitted.
  By default, standard output will be checked if it is a terminal capable
  of handling these sequences (using `terminal?/0` function)

  ## Example

    IO.puts IO.ANSI.escape "Hello #[red,bright,green]yes" #=>
    "Hello \e[31m\e[1m\e[32myes\e[0m"
  """
  @doc escape_doc
  @spec escape(String.t) :: String.t
  def escape(string) do
    escape(string, terminal?)
  end

  @doc escape_doc
  @spec escape(String.t, emit :: boolean) :: String.t
  def escape(string, emit) do
    do_escape(string <> "#[reset]", false, emit,[])
  end

  escape_fragment_doc =  """
  Escapes a string coverting named ANSI sequences into actual ANSI codes.

  The format for referring sequences is `#[red]` and `#[red,bright]` (for
  multiple sequences)

  An optional boolean parameter can be passed to enable or disable
  emitting actual ANSI codes. When false, no ANSI codes will emitted.
  By default, standard output will be checked if it is a terminal capable
  of handling these sequences (using `terminal?/0` function)

  ## Example

    IO.puts IO.ANSI.escape "Hello #[red,bright,green]yes#[reset]" #=>
    "Hello \e[31m\e[1m\e[32myes\e[0m"
  """

  @doc escape_fragment_doc
  @spec escape_fragment(String.t) :: String.t
  def escape_fragment(string) do
    escape_fragment(string, terminal?)
  end

  @doc escape_fragment_doc
  @spec escape_fragment(String.t, emit :: boolean) :: String.t
  def escape_fragment(string, emit) do
    do_escape(string, false, emit,[])
  end


  defp do_escape(<< ?#, ?[, rest :: binary >>, false, emit, acc) do
    do_escape_sequence(rest, emit, acc)
  end
  defp do_escape(<< ?,, rest :: binary >>, true, emit, acc) do
    do_escape_sequence(rest, emit, acc)
  end
  defp do_escape(<< ?\s, rest :: binary >>, true, emit, acc) do
    do_escape(rest, true, emit, acc)
  end
  defp do_escape(<< ?], rest :: binary >>, true, emit, acc) do
    do_escape(rest, false, emit, acc)
  end
  defp do_escape(<< x :: [binary, size(1)], rest :: binary>>, false, emit, acc) do
    do_escape(rest, false, emit, [x|acc])
  end
  defp do_escape("", false, _emit, acc) do
    list_to_binary(Enum.reverse(acc))
  end

  defp do_escape_sequence(rest, emit, acc) do
    {code, rest} = escape_sequence(rest)
    if emit do
      acc = [code|acc]
    end
    do_escape(rest, true, emit, acc)
  end

end