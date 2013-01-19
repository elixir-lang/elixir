defmodule IO.ANSI.Sequence do
  @moduledoc false
  defmacro defsequence({name, _, [code]}) do
    defsequence(name, code)
  end
  def defsequence(name, code) do
    quote do
      doc = @doc <> "\n"
      @doc doc
      @spec unquote(name)() :: String.t
      @spec unquote(name)(:io.device | boolean) :: String.t
      def unquote(name)() do
        if IO.ANSI.terminal? do
          "\e[#{unquote(code)}m"
        else
          ""
        end
      end
      @doc doc <> """

      Instead of a device, a boolean can be passed to enable/disable
      ANSI rendering explicitly
      """
      def unquote(name)(true) do
        "\e[#{unquote(code)}m"
      end
      def unquote(name)(false) do
        "\e[#{unquote(code)}m"
      end
      def unquote(name)(device) do
        if IO.ANSI.terminal?(device) do
          "\e[#{unquote(code)}m"
        else
          ""
        end
      end

    end
  end
end

defmodule IO.ANSI do
  @moduledoc """
  This module provides functionality to render ANSI escape sequences
  (http://en.wikipedia.org/wiki/ANSI_escape_code) —  characters embedded
  in the text used to control formatting, color, and other output options
  on video text terminals
  """
  import IO.ANSI.Sequence

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

end