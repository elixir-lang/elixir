defmodule IO do
  @moduledoc """
  Functions handling input/output (IO).

  Many functions in this module expect an IO device as an argument.
  An IO device must be a PID or an atom representing a process.
  For convenience, Elixir provides `:stdio` and `:stderr` as
  shortcuts to Erlang's `:standard_io` and `:standard_error`.

  The majority of the functions expect chardata, i.e. strings or
  lists of characters and strings. In case another type is given,
  functions will convert to string via the `String.Chars` protocol
  (as shown in typespecs).

  The functions starting with `bin` expect iodata as an argument,
  i.e. binaries or lists of bytes and binaries.

  ## IO devices

  An IO device may be an atom or a PID. In case it is an atom,
  the atom must be the name of a registered process. In addition,
  Elixir provides two shortcuts:

    * `:stdio` - a shortcut for `:standard_io`, which maps to
      the current `Process.group_leader/0` in Erlang

    * `:stderr` - a shortcut for the named process `:standard_error`
      provided in Erlang

  IO devices maintain their position, which means subsequent calls to any
  reading or writing functions will start from the place where the device
  was last accessed. The position of files can be changed using the
  `:file.position/2` function.

  """

  @type device :: atom | pid
  @type nodata :: {:error, term} | :eof
  @type chardata :: String.t() | maybe_improper_list(char | chardata, String.t() | [])

  defmacrop is_iodata(data) do
    quote do
      is_list(unquote(data)) or is_binary(unquote(data))
    end
  end

  @doc """
  Reads from the IO `device`.

  The `device` is iterated by the given number of characters or line by line if
  `:line` is given.
  Alternatively, if `:all` is given, then whole `device` is returned.

  It returns:

    * `data` - the output characters

    * `:eof` - end of file was encountered

    * `{:error, reason}` - other (rare) error condition;
      for instance, `{:error, :estale}` if reading from an
      NFS volume

  If `:all` is given, `:eof` is never returned, but an
  empty string in case the device has reached EOF.
  """
  @spec read(device, :all | :line | non_neg_integer) :: chardata | nodata
  def read(device \\ :stdio, line_or_chars)

  def read(device, :all) do
    do_read_all(map_dev(device), "")
  end

  def read(device, :line) do
    :io.get_line(map_dev(device), '')
  end

  def read(device, count) when is_integer(count) and count >= 0 do
    :io.get_chars(map_dev(device), '', count)
  end

  defp do_read_all(mapped_dev, acc) do
    case :io.get_line(mapped_dev, "") do
      line when is_binary(line) -> do_read_all(mapped_dev, acc <> line)
      :eof -> acc
      other -> other
    end
  end

  @doc """
  Reads from the IO `device`. The operation is Unicode unsafe.

  The `device` is iterated by the given number of bytes or line by line if
  `:line` is given.
  Alternatively, if `:all` is given, then whole `device` is returned.

  It returns:

    * `data` - the output bytes

    * `:eof` - end of file was encountered

    * `{:error, reason}` - other (rare) error condition;
      for instance, `{:error, :estale}` if reading from an
      NFS volume

  If `:all` is given, `:eof` is never returned, but an
  empty string in case the device has reached EOF.

  Note: do not use this function on IO devices in Unicode mode
  as it will return the wrong result.
  """
  @spec binread(device, :all | :line | non_neg_integer) :: iodata | nodata
  def binread(device \\ :stdio, line_or_chars)

  def binread(device, :all) do
    do_binread_all(map_dev(device), "")
  end

  def binread(device, :line) do
    case :file.read_line(map_dev(device)) do
      {:ok, data} -> data
      other -> other
    end
  end

  def binread(device, count) when is_integer(count) and count >= 0 do
    case :file.read(map_dev(device), count) do
      {:ok, data} -> data
      other -> other
    end
  end

  @read_all_size 4096
  defp do_binread_all(mapped_dev, acc) do
    case :file.read(mapped_dev, @read_all_size) do
      {:ok, data} -> do_binread_all(mapped_dev, acc <> data)
      :eof -> acc
      other -> other
    end
  end

  @doc """
  Writes `item` to the given `device`.

  By default, the `device` is the standard output.

  ## Examples

      IO.write("sample")
      #=> sample

      IO.write(:stderr, "error")
      #=> error

  """
  @spec write(device, chardata | String.Chars.t()) :: :ok
  def write(device \\ :stdio, item) do
    :io.put_chars(map_dev(device), to_chardata(item))
  end

  @doc """
  Writes `item` as a binary to the given `device`.
  No Unicode conversion happens.
  The operation is Unicode unsafe.

  Check `write/2` for more information.

  Note: do not use this function on IO devices in Unicode mode
  as it will return the wrong result.
  """
  @spec binwrite(device, iodata) :: :ok | {:error, term}
  def binwrite(device \\ :stdio, item) when is_iodata(item) do
    :file.write(map_dev(device), item)
  end

  @doc """
  Writes `item` to the given `device`, similar to `write/2`,
  but adds a newline at the end.

  By default, the `device` is the standard output. It returns `:ok`
  if it succeeds.

  ## Examples

      IO.puts("Hello World!")
      #=> Hello World!

      IO.puts(:stderr, "error")
      #=> error

  """
  @spec puts(device, chardata | String.Chars.t()) :: :ok
  def puts(device \\ :stdio, item) do
    :io.put_chars(map_dev(device), [to_chardata(item), ?\n])
  end

  @doc """
  Writes a `message` to stderr, along with the given `stacktrace`.

  This function also notifies the compiler a warning was printed
  (in case --warnings-as-errors was enabled). It returns `:ok`
  if it succeeds.

  An empty list can be passed to avoid stacktrace printing.

  ## Examples

      stacktrace = [{MyApp, :main, 1, [file: 'my_app.ex', line: 4]}]
      IO.warn("variable bar is unused", stacktrace)
      #=> warning: variable bar is unused
      #=>   my_app.ex:4: MyApp.main/1

  """
  @spec warn(chardata | String.Chars.t(), Exception.stacktrace()) :: :ok
  def warn(message, []) do
    :elixir_errors.bare_warn(nil, nil, [to_chardata(message), ?\n])
  end

  def warn(message, [{_, _, _, opts} | _] = stacktrace) do
    formatted_trace = Enum.map_join(stacktrace, "\n  ", &Exception.format_stacktrace_entry(&1))
    message = [to_chardata(message), ?\n, "  ", formatted_trace, ?\n]
    line = opts[:line]
    file = opts[:file]
    :elixir_errors.bare_warn(line, file && List.to_string(file), message)
  end

  @doc """
  Writes a `message` to stderr, along with the current stacktrace.

  It returns `:ok` if it succeeds.

  ## Examples

      IO.warn("variable bar is unused")
      #=> warning: variable bar is unused
      #=>   (iex) evaluator.ex:108: IEx.Evaluator.eval/4

  """
  @spec warn(chardata | String.Chars.t()) :: :ok
  def warn(message) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    warn(message, Enum.drop(stacktrace, 2))
  end

  @doc """
  Inspects and writes the given `item` to the device.

  It's important to note that it returns the given `item` unchanged.
  This makes it possible to "spy" on values by inserting an
  `IO.inspect/2` call almost anywhere in your code, for example,
  in the middle of a pipeline.

  It enables pretty printing by default with width of
  80 characters. The width can be changed by explicitly
  passing the `:width` option.

  The output can be decorated with a label, by providing the `:label`
  option to easily distinguish it from other `IO.inspect/2` calls.
  The label will be printed before the inspected `item`.

  See `Inspect.Opts` for a full list of remaining formatting options.

  ## Examples

      IO.inspect(<<0, 1, 2>>, width: 40)

  Prints:

      <<0, 1, 2>>

  We can use the `:label` option to decorate the output:

      IO.inspect(1..100, label: "a wonderful range")

  Prints:

      a wonderful range: 1..100

  The `:label` option is especially useful with pipelines:

      [1, 2, 3]
      |> IO.inspect(label: "before")
      |> Enum.map(&(&1 * 2))
      |> IO.inspect(label: "after")
      |> Enum.sum()

  Prints:

      before: [1, 2, 3]
      after: [2, 4, 6]

  """
  @spec inspect(item, keyword) :: item when item: var
  def inspect(item, opts \\ []) do
    inspect(:stdio, item, opts)
  end

  @doc """
  Inspects `item` according to the given options using the IO `device`.

  See `inspect/2` for a full list of options.
  """
  @spec inspect(device, item, keyword) :: item when item: var
  def inspect(device, item, opts) when is_list(opts) do
    label = if label = opts[:label], do: [to_chardata(label), ": "], else: []
    opts = struct(Inspect.Opts, opts)
    doc = Inspect.Algebra.group(Inspect.Algebra.to_doc(item, opts))
    chardata = Inspect.Algebra.format(doc, opts.width)
    puts(device, [label, chardata])
    item
  end

  @doc """
  Gets a number of bytes from IO device `:stdio`.

  If `:stdio` is a Unicode device, `count` implies
  the number of Unicode codepoints to be retrieved.
  Otherwise, `count` is the number of raw bytes to be retrieved.

  See `IO.getn/3` for a description of return values.

  """
  @spec getn(chardata | String.Chars.t(), pos_integer) :: chardata | nodata
  @spec getn(device, chardata | String.Chars.t()) :: chardata | nodata
  def getn(prompt, count \\ 1)

  def getn(prompt, count) when is_integer(count) and count > 0 do
    getn(:stdio, prompt, count)
  end

  def getn(device, prompt) when not is_integer(prompt) do
    getn(device, prompt, 1)
  end

  @doc """
  Gets a number of bytes from the IO `device`.

  If the IO `device` is a Unicode device, `count` implies
  the number of Unicode codepoints to be retrieved.
  Otherwise, `count` is the number of raw bytes to be retrieved.

  It returns:

    * `data` - the input characters

    * `:eof` - end of file was encountered

    * `{:error, reason}` - other (rare) error condition;
      for instance, `{:error, :estale}` if reading from an
      NFS volume

  """
  @spec getn(device, chardata | String.Chars.t(), pos_integer) :: chardata | nodata
  def getn(device, prompt, count) when is_integer(count) and count > 0 do
    :io.get_chars(map_dev(device), to_chardata(prompt), count)
  end

  @doc ~S"""
  Reads a line from the IO `device`.

  It returns:

    * `data` - the characters in the line terminated
      by a line-feed (LF) or end of file (EOF)

    * `:eof` - end of file was encountered

    * `{:error, reason}` - other (rare) error condition;
      for instance, `{:error, :estale}` if reading from an
      NFS volume

  ## Examples

  To display "What is your name?" as a prompt and await user input:

      IO.gets("What is your name?\n")

  """
  @spec gets(device, chardata | String.Chars.t()) :: chardata | nodata
  def gets(device \\ :stdio, prompt) do
    :io.get_line(map_dev(device), to_chardata(prompt))
  end

  @doc """
  Converts the IO `device` into an `IO.Stream`.

  An `IO.Stream` implements both `Enumerable` and
  `Collectable`, allowing it to be used for both read
  and write.

  The `device` is iterated by the given number of characters or line by line if
  `:line` is given.

  This reads from the IO as UTF-8. Check out
  `IO.binstream/2` to handle the IO as a raw binary.

  Note that an IO stream has side effects and every time
  you go over the stream you may get different results.

  ## Examples

  Here is an example on how we mimic an echo server
  from the command line:

      Enum.each(IO.stream(:stdio, :line), &IO.write(&1))

  """
  @spec stream(device, :line | pos_integer) :: Enumerable.t()
  def stream(device, line_or_codepoints)
      when line_or_codepoints == :line
      when is_integer(line_or_codepoints) and line_or_codepoints > 0 do
    IO.Stream.__build__(map_dev(device), false, line_or_codepoints)
  end

  @doc """
  Converts the IO `device` into an `IO.Stream`. The operation is Unicode unsafe.

  An `IO.Stream` implements both `Enumerable` and
  `Collectable`, allowing it to be used for both read
  and write.

  The `device` is iterated by the given number of bytes or line by line if
  `:line` is given.
  This reads from the IO device as a raw binary.

  Note that an IO stream has side effects and every time
  you go over the stream you may get different results.

  Finally, do not use this function on IO devices in Unicode
  mode as it will return the wrong result.

  """
  @spec binstream(device, :line | pos_integer) :: Enumerable.t()
  def binstream(device, line_or_bytes)
      when line_or_bytes == :line
      when is_integer(line_or_bytes) and line_or_bytes > 0 do
    IO.Stream.__build__(map_dev(device), true, line_or_bytes)
  end

  @doc """
  Converts chardata (a list of integers representing codepoints,
  lists and strings) into a string.

  In case the conversion fails, it raises an `UnicodeConversionError`.
  If a string is given, it returns the string itself.

  ## Examples

      iex> IO.chardata_to_string([0x00E6, 0x00DF])
      "æß"

      iex> IO.chardata_to_string([0x0061, "bc"])
      "abc"

      iex> IO.chardata_to_string("string")
      "string"

  """
  @spec chardata_to_string(chardata) :: String.t()
  def chardata_to_string(string) when is_binary(string) do
    string
  end

  def chardata_to_string(list) when is_list(list) do
    List.to_string(list)
  end

  @doc """
  Converts iodata (a list of integers representing bytes, lists
  and binaries) into a binary.
  The operation is Unicode unsafe.

  Notice that this function treats lists of integers as raw bytes
  and does not perform any kind of encoding conversion. If you want
  to convert from a charlist to a string (UTF-8 encoded), please
  use `chardata_to_string/1` instead.

  If this function receives a binary, the same binary is returned.

  Inlined by the compiler.

  ## Examples

      iex> bin1 = <<1, 2, 3>>
      iex> bin2 = <<4, 5>>
      iex> bin3 = <<6>>
      iex> IO.iodata_to_binary([bin1, 1, [2, 3, bin2], 4 | bin3])
      <<1, 2, 3, 1, 2, 3, 4, 5, 4, 6>>

      iex> bin = <<1, 2, 3>>
      iex> IO.iodata_to_binary(bin)
      <<1, 2, 3>>

  """
  @spec iodata_to_binary(iodata) :: binary
  def iodata_to_binary(item) do
    :erlang.iolist_to_binary(item)
  end

  @doc """
  Returns the size of an iodata.

  Inlined by the compiler.

  ## Examples

      iex> IO.iodata_length([1, 2 | <<3, 4>>])
      4

  """
  @spec iodata_length(iodata) :: non_neg_integer
  def iodata_length(item) do
    :erlang.iolist_size(item)
  end

  @doc false
  def each_stream(device, line_or_codepoints) do
    case read(device, line_or_codepoints) do
      :eof ->
        {:halt, device}

      {:error, reason} ->
        raise IO.StreamError, reason: reason

      data ->
        {[data], device}
    end
  end

  @doc false
  def each_binstream(device, line_or_chars) do
    case binread(device, line_or_chars) do
      :eof ->
        {:halt, device}

      {:error, reason} ->
        raise IO.StreamError, reason: reason

      data ->
        {[data], device}
    end
  end

  @compile {:inline, map_dev: 1, to_chardata: 1}

  # Map the Elixir names for standard IO and error to Erlang names
  defp map_dev(:stdio), do: :standard_io
  defp map_dev(:stderr), do: :standard_error
  defp map_dev(other) when is_atom(other) or is_pid(other) or is_tuple(other), do: other

  defp to_chardata(list) when is_list(list), do: list
  defp to_chardata(other), do: to_string(other)
end
