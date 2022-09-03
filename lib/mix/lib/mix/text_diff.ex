defmodule Mix.TextDiff do
  @moduledoc ~S'''
  Formats the diff between two strings.

  ## Examples

      iex> code = """
      ...> defmodule Foo do
      ...>   @moduledoc   false
      ...>
      ...>   def foo, do:  :foo
      ...>
      ...>   def three_times(x) do
      ...>     {x,
      ...>      x,x}
      ...>   end
      ...>
      ...>   def bar(x) do
      ...>     {:bar, x}
      ...>   end
      ...> end\
      ...> """
      iex> formatted = code |> Code.format_string!() |> IO.iodata_to_binary()
      iex> code
      ...> |> Mix.TextDiff.format(formatted, color: false)
      ...> |> IO.iodata_to_binary()
      """
       1  1   |defmodule Foo do
       2    - |  @moduledoc   false
          2 + |  @moduledoc false
       3  3   |
       4    - |  def foo, do:  :foo
          4 + |  def foo, do: :foo
       5  5   |
       6  6   |  def three_times(x) do
       7    - |    {x,
       8    - |     x,x}
          7 + |    {x, x, x}
       9  8   |  end
      10  9   |
           ...|
      """
  '''

  @newline "\n"
  @blank " "

  @separator "|"
  @line_num_pad @blank

  @gutter [
    del: " - ",
    eq: "   ",
    ins: " + ",
    skip: "..."
  ]

  @colors [
    del: [text: :red, space: :red_background],
    ins: [text: :green, space: :green_background]
  ]

  @default_opts [
    after: 2,
    before: 2,
    color: true,
    line: 1
  ]

  @doc ~S'''
  Formats the diff between two strings.

  The returned `iodata` shows the lines with changes and 2 lines before and
  after the changed positions. The string contains also a gutter with line
  number and a `-` or `+` for removed and added lines. Multiple lines without
  changes are marke with `...` in the gutter.

  ## Options

    * `after` - the count of lines printed after each change. Defaults to `2`.
    * `before` - the count of lines printed befor each change. Defaults to `2`.
    * `color` - enables color in the output. Defautls to `truel.`
    * `line` - the line number of the first line. Defaults to `1`.

  ## Examples

      iex> code = """
      ...> defmodule Bar do
      ...>   @moduledoc false
      ...>
      ...>   bar(x, y) do
      ...>     z = x + y
      ...>     {x,y  , z}
      ...>   end
      ...>
      ...>   bar(x, y, z) do
      ...>     {x, y, z}
      ...>   end
      ...> end\
      ...> """
      iex> formatted = code |> Code.format_string!() |> IO.iodata_to_binary()
      iex> code
      ...> |> Mix.TextDiff.format(formatted, color: false)
      ...> |> IO.iodata_to_binary()
      """
           ...|
       4  4   |  bar(x, y) do
       5  5   |    z = x + y
       6    - |    {x,y  , z}
          6 + |    {x, y, z}
       7  7   |  end
       8  8   |
           ...|
      """
      iex> code
      ...> |> Mix.TextDiff.format(formatted, color: false, after: 1, before: 1)
      ...> |> IO.iodata_to_binary()
      """
           ...|
       5  5   |    z = x + y
       6    - |    {x,y  , z}
          6 + |    {x, y, z}
       7  7   |  end
           ...|
      """
  '''
  @spec format(String.t(), String.t(), keyword()) :: iodata()
  def format(code, code, opts \\ default_opts())

  def format(code, code, _opts), do: []

  def format(old, new, opts) do
    opts = Keyword.merge(@default_opts, opts)

    old = String.split(old, "\n")
    new = String.split(new, "\n")

    max = max(length(new), length(old))
    line_num_digits = max |> Integer.digits() |> length()
    opts = Keyword.put(opts, :line_num_digits, line_num_digits)

    {line, opts} = Keyword.pop!(opts, :line)

    old
    |> List.myers_difference(new)
    |> diff_to_iodata({line, line}, opts)
  end

  @spec default_opts() :: keyword()
  def default_opts, do: @default_opts

  defp diff_to_iodata(diff, line_nums, opts, iodata \\ [])

  defp diff_to_iodata([], _line_nums, _opts, iodata), do: Enum.reverse(iodata)

  defp diff_to_iodata([{:eq, [""]}], _line_nums, _opts, iodata), do: Enum.reverse(iodata)

  defp diff_to_iodata([{:eq, lines}], line_nums, opts, iodata) do
    lines_after = Enum.take(lines, opts[:after])
    iodata = lines(iodata, {:eq, lines_after}, line_nums, opts)

    iodata =
      case length(lines) > opts[:after] do
        false -> iodata
        true -> lines(iodata, :skip, opts)
      end

    Enum.reverse(iodata)
  end

  defp diff_to_iodata([{:eq, lines} | diff], {line, line}, opts, [] = iodata) do
    {start, lines_before} = Enum.split(lines, opts[:before] * -1)

    iodata =
      case length(lines) > opts[:before] do
        false -> iodata
        true -> lines(iodata, :skip, opts)
      end

    line = line + length(start)
    iodata = lines(iodata, {:eq, lines_before}, {line, line}, opts)

    line = line + length(lines_before)
    diff_to_iodata(diff, {line, line}, opts, iodata)
  end

  defp diff_to_iodata([{:eq, lines} | diff], line_nums, opts, iodata) do
    case length(lines) > opts[:after] + opts[:before] do
      true ->
        {lines1, lines2, lines3} = split(lines, opts[:after], opts[:before] * -1)

        iodata =
          iodata
          |> lines({:eq, lines1}, line_nums, opts)
          |> lines(:skip, opts)
          |> lines({:eq, lines3}, add_line_nums(line_nums, length(lines1) + length(lines2)), opts)

        line_nums = add_line_nums(line_nums, length(lines))

        diff_to_iodata(diff, line_nums, opts, iodata)

      false ->
        iodata = lines(iodata, {:eq, lines}, line_nums, opts)
        line_nums = add_line_nums(line_nums, length(lines))

        diff_to_iodata(diff, line_nums, opts, iodata)
    end
  end

  defp diff_to_iodata([{:del, [del]}, {:ins, [ins]} | diff], line_nums, opts, iodata) do
    iodata = lines(iodata, {:chg, del, ins}, line_nums, opts)
    diff_to_iodata(diff, add_line_nums(line_nums, 1), opts, iodata)
  end

  defp diff_to_iodata([{kind, lines} | diff], line_nums, opts, iodata) do
    iodata = lines(iodata, {kind, lines}, line_nums, opts)
    line_nums = add_line_nums(line_nums, length(lines), kind)

    diff_to_iodata(diff, line_nums, opts, iodata)
  end

  defp split(list, count1, count2) do
    {split1, split2} = Enum.split(list, count1)
    {split2, split3} = Enum.split(split2, count2)
    {split1, split2, split3}
  end

  defp lines(iodata, :skip, opts) do
    line_num = String.duplicate(@blank, opts[:line_num_digits] * 2 + 1)
    [[line_num, @gutter[:skip], @separator, @newline] | iodata]
  end

  defp lines(iodata, {:chg, del, ins}, line_nums, opts) do
    {del, ins} = line_diff(del, ins, opts)

    [
      [gutter(line_nums, :ins, opts), ins, @newline],
      [gutter(line_nums, :del, opts), del, @newline]
      | iodata
    ]
  end

  defp lines(iodata, {kind, lines}, line_nums, opts) do
    lines
    |> Enum.with_index()
    |> Enum.reduce(iodata, fn {line, offset}, iodata ->
      line_nums = add_line_nums(line_nums, offset, kind)
      [[gutter(line_nums, kind, opts), colorize(line, kind, false, opts), @newline] | iodata]
    end)
  end

  defp gutter(line_nums, kind, opts) do
    [line_num(line_nums, kind, opts), colorize(@gutter[kind], kind, false, opts), @separator]
  end

  defp line_num({line_num_old, line_num_new}, :eq, opts) do
    old =
      line_num_old
      |> to_string()
      |> String.pad_leading(opts[:line_num_digits], @line_num_pad)

    new =
      line_num_new
      |> to_string()
      |> String.pad_leading(opts[:line_num_digits], @line_num_pad)

    [old, @blank, new]
  end

  defp line_num({line_num_old, _line_num_new}, :del, opts) do
    old =
      line_num_old
      |> to_string()
      |> String.pad_leading(opts[:line_num_digits], @line_num_pad)

    new = String.duplicate(@blank, opts[:line_num_digits])
    [old, @blank, new]
  end

  defp line_num({_line_num_old, line_num_new}, :ins, opts) do
    old = String.duplicate(@blank, opts[:line_num_digits])

    new =
      line_num_new
      |> to_string()
      |> String.pad_leading(opts[:line_num_digits], @line_num_pad)

    [old, @blank, new]
  end

  defp line_diff(del, ins, opts) do
    diff = String.myers_difference(del, ins)

    Enum.reduce(diff, {[], []}, fn
      {:eq, str}, {del, ins} -> {[del | str], [ins | str]}
      {:del, str}, {del, ins} -> {[del | colorize(str, :del, true, opts)], ins}
      {:ins, str}, {del, ins} -> {del, [ins | colorize(str, :ins, true, opts)]}
    end)
  end

  defp colorize(str, kind, space, opts) do
    case Keyword.fetch!(opts, :color) && Keyword.has_key?(@colors, kind) do
      false ->
        str

      true ->
        color = Keyword.fetch!(@colors, kind)

        case space do
          false ->
            IO.ANSI.format([color[:text], str])

          true ->
            str
            |> String.split(~r/[\t\s]+/, include_captures: true)
            |> Enum.map(fn
              <<start::binary-size(1), _::binary>> = str when start in ["\t", "\s"] ->
                IO.ANSI.format([color[:space], str])

              str ->
                IO.ANSI.format([color[:text], str])
            end)
        end
    end
  end

  defp add_line_nums({line_num_old, line_num_new}, lines, kind \\ :eq) do
    case kind do
      :eq -> {line_num_old + lines, line_num_new + lines}
      :ins -> {line_num_old, line_num_new + lines}
      :del -> {line_num_old + lines, line_num_new}
    end
  end
end
