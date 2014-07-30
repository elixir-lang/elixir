defmodule IO.ANSI.Sequence do
  @moduledoc false

  defmacro defsequence(name, code \\ "", terminator \\ "m") do
    quote bind_quoted: [name: name, code: code, terminator: terminator] do
      def unquote(name)() do
        "\e[#{unquote(code)}#{unquote(terminator)}"
      end

      defp escape_sequence(unquote(Atom.to_char_list(name))) do
        unquote(name)()
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
  be displayed as intended. This is checked by sending a message to
  the group leader. In case the group leader does not support the message,
  it will likely lead to a timeout (and a slow down on execution time).
  """
  @spec terminal? :: boolean
  @spec terminal?(:io.device) :: boolean
  def terminal?(device \\ :erlang.group_leader) do
    !match?({:win32, _}, :os.type()) and
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

  for {code, color} <- colors do
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

  defp escape_sequence(other) do
    raise ArgumentError, "invalid ANSI sequence specification: #{other}"
  end

  @doc ~S"""
  Formats a chardata-like argument by converting named ANSI sequences into actual
  ANSI codes.

  The named sequences are represented by atoms.

  It will also append an `IO.ANSI.reset` to the chardata when a conversion is
  performed. If you don't want this behaviour, use `format_fragment/2`.

  An optional boolean parameter can be passed to enable or disable
  emitting actual ANSI codes. When `false`, no ANSI codes will emitted.
  By default, standard output will be checked if it is a terminal capable
  of handling these sequences (using `terminal?/1` function)

  ## Examples

      iex> IO.ANSI.format(["Hello, ", :red, :bright, "world!"], true)
      [[[[[[], "Hello, "], "\e[31m"], "\e[1m"], "world!"] | "\e[0m"]

  """
  def format(chardata, emit \\ terminal?) when is_boolean(emit) do
    do_format(chardata, [], [], emit, :maybe)
  end

  @doc ~S"""
  Formats a chardata-like argument by converting named ANSI sequences into actual
  ANSI codes.

  The named sequences are represented by atoms.

  An optional boolean parameter can be passed to enable or disable
  emitting actual ANSI codes. When `false`, no ANSI codes will emitted.
  By default, standard output will be checked if it is a terminal capable
  of handling these sequences (using `terminal?/1` function)

  ## Examples

      iex> IO.ANSI.format_fragment([:bright, 'Word'], true)
      [[[[[[], "\e[1m"], 87], 111], 114], 100]

  """
  def format_fragment(chardata, emit \\ terminal?) when is_boolean(emit) do
    do_format(chardata, [], [], emit, false)
  end

  defp do_format([term | rest], rem, acc, emit, append_reset) do
    do_format(term, [rest | rem], acc, emit, append_reset)
  end

  defp do_format(term, rem, acc, true, append_reset) when is_atom(term) do
    try do
      do_format([], rem, [acc | [apply(IO.ANSI, term, [])]], true, !!append_reset)
    rescue
      _ in UndefinedFunctionError ->
        raise ArgumentError, message: "invalid ANSI sequence specification: #{term}"
    end
  end

  defp do_format(term, rem, acc, false, append_reset) when is_atom(term) do
    do_format([], rem, acc, false, append_reset)
  end

  defp do_format(term, rem, acc, emit, append_reset) when not is_list(term) do
    do_format([], rem, [acc | [term]], emit, append_reset)
  end

  defp do_format([], [next | rest], acc, emit, append_reset) do
    do_format(next, rest, acc, emit, append_reset)
  end

  defp do_format([], [], acc, true, true) do
    [acc | IO.ANSI.reset]
  end

  defp do_format([], [], acc, _emit, _append_reset) do
    acc
  end

  @doc false
  def escape(string, emit \\ terminal?) when is_binary(string) and is_boolean(emit) do
    {rendered, emitted} = do_escape(string, emit, false, nil, [])
    if emitted do
      rendered <> reset
    else
      rendered
    end
  end

  @doc false
  def escape_fragment(string, emit \\ terminal?) when is_binary(string) and is_boolean(emit) do
    {escaped, _emitted} = do_escape(string, emit, false, nil, [])
    escaped
  end

  defp do_escape(<<?}, t :: binary>>, emit, emitted, buffer, acc) when is_list(buffer) do
    sequences =
      buffer
      |> Enum.reverse()
      |> :string.tokens(',')
      |> Enum.map(&(&1 |> :string.strip |> escape_sequence))
      |> Enum.reverse()

    if emit and sequences != [] do
      do_escape(t, emit, true, nil, sequences ++ acc)
    else
      do_escape(t, emit, emitted, nil, acc)
    end
  end

  defp do_escape(<<h, t :: binary>>, emit, emitted, buffer, acc) when is_list(buffer) do
    do_escape(t, emit, emitted, [h|buffer], acc)
  end

  defp do_escape(<<>>, _emit, _emitted, buffer, _acc) when is_list(buffer) do
    buffer = IO.iodata_to_binary Enum.reverse(buffer)
    raise ArgumentError, "missing } for escape fragment #{buffer}"
  end

  defp do_escape(<<?%, ?{, t :: binary>>, emit, emitted, nil, acc) do
    do_escape(t, emit, emitted, [], acc)
  end

  defp do_escape(<<h, t :: binary>>, emit, emitted, nil, acc) do
    do_escape(t, emit, emitted, nil, [h|acc])
  end

  defp do_escape(<<>>, _emit, emitted, nil, acc) do
    {IO.iodata_to_binary(Enum.reverse(acc)), emitted}
  end
end
