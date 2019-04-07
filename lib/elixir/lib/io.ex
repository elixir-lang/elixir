defmodule IO do
  @moduledoc ~S"""
  Functions handling input/output (IO).

  Many functions in this module expect an IO device as an argument.
  An IO device must be a PID or an atom representing a process.
  For convenience, Elixir provides `:stdio` and `:stderr` as
  shortcuts to Erlang's `:standard_io` and `:standard_error`.

  The majority of the functions expect chardata. In case another type is given,
  functions will convert those types to string via the `String.Chars` protocol
  (as shown in typespecs). For more information on chardata, see the
  "Iolists and iodata" section below.

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

  ## IO data

  IO data is a data type that can be used as a more efficient alternative to binaries
  in certain situations.

  A term of type **IO data** is a binary or a list containing bytes (integers in `0..255`),
  binaries, or nested iolists. The type is recursive. Let's see an example of one of
  the possible IO data representing the binary `"hello"`:

      [?h, "el", ["l", [?o]]]

  The built-in `t:iodata/0` type is defined in terms of `t:iolist/0`. An IO list is
  the same as IO data, but it doesn't allow for a binary at the top level (that is,
  a binary is not an IO list but it is valid IO data).

  ### Use cases for IO data

  IO data exists because often you need to do many append operations
  on smaller chunks of binaries in order to create a bigger binary. However, in
  Erlang and Elixir concatenating binaries will copy the concatenated binaries
  into a new binary.

      def email(username, domain) do
        username <> "@" <> domain
      end

  In this function, creating the email address will copy the `username` and `domain`
  binaries. Now imagine you want to use the resulting email inside another binary:

      def welcome_message(name, username, domain) do
        "Welcome #{name}, your email is: #{email(username, domain)}"
      end

      IO.puts(welcome_message("Meg", "meg", "example.com"))
      #=> "Welcome Meg, your email is: meg@example.com"

  Every time you concatenate binaries and use interpolation (`#{}`) you are making
  copies of those binaries. However, in many cases you don't need the complete
  binary while you manipulate it, but only at the end to print it out or send it
  somewhere. In such cases, you can construct the binary by creating IO data and
  then convert the final piece of IO data to a binary through `iodata_to_binary/1`:

      def email(username, domain) do
        [username, ?@, domain]
      end

      def welcome_message(name, username, domain) do
        ["Welcome ", name, ", your email is: ", email(username, domain)]
      end

      welcome_message("Meg", "meg", "example.com")
      |> IO.iodata_to_binary()
      |> IO.puts()
      #=> "Welcome Meg, your email is: meg@example.com"

  Building IO data is cheaper than concatenating binaries. Concatenating multiple
  pieces of IO data just means putting them together inside a list since IO data
  can be arbitrarily nested, and that's a cheap and efficient operation.
  `iodata_to_binary/1` is reasonably efficient since it's implemented natively
  in C.

  ### Using IO data without converting it to binary

  In cases where you need to write IO data to some kind of external device, it's
  often possible to avoid converting the IO data to binary altogether. For example,
  the `:gen_tcp.send/2` function used to write data on a TCP socket accepts IO data
  directly. This function will take care of writing the IO data to the socket directly
  without converting it to binary. There's a lot more functions that work in the same
  way. A good example is `IO.puts/1` itself, which accepts IO data for efficient writing
  on the output device:

      IO.puts([?h, "el", ["l", [?o]]])
      #=> "hello"

  In many cases avoiding to turn IO data into binaries is the correct choice
  for performance. One drawback of IO data is that you can't do things like
  pattern match on the first part of a piece of IO data like you can with a
  binary, because you usually don't know the shape of the IO data. However the
  `IO` module provides the `iodata_length/1` function for when you want to
  compute the length of a piece of IO data in an efficient way without converting
  the iodata to a binary.

  ### Chardata

  Erlang and Elixir also have the idea of `t:chardata/0`. Chardata is very
  similar to IO data: the only difference is that integers in IO data represent
  bytes while integers in chardata represent Unicode codepoints. Bytes
  (`t:byte/0`) are integers in the `0..255` range, while Unicode codepoints
  (`t:char/0`) are integers in the range `0..0x10FFFF`. The `IO` module provides
  the `chardata_to_string/1` function for chardata as the "counter-part" of the
  `iodata_to_binary/1` function for IO data.

  If you try to use `iodata_to_binary/1` on chardata, it will result in an
  argument error. For example, let's try to put a codepoint that is not
  representable with one byte, like `?π`, inside IO data:

      iex> IO.iodata_to_binary(["The symbol for pi is: ", ?π])
      ** (ArgumentError) argument error

  If we use chardata instead, it will work as expected:

      iex> IO.chardata_to_string(["The symbol for pi is: ", ?π])
      "The symbol for pi is: π"

  """

  @type device :: atom | pid
  @type nodata :: {:error, term} | :eof
  @type chardata :: String.t() | maybe_improper_list(char | chardata, String.t() | [])

  defguardp is_iodata(data) when is_list(data) or is_binary(data)

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
  Writes `chardata` to the given `device`.

  By default, the `device` is the standard output.

  ## Examples

      IO.write("sample")
      #=> sample

      IO.write(:stderr, "error")
      #=> error

  """
  @spec write(device, chardata | String.Chars.t()) :: :ok
  def write(device \\ :stdio, chardata) do
    :io.put_chars(map_dev(device), to_chardata(chardata))
  end

  @doc """
  Writes `iodata` to the given `device`.

  This operation is meant to be used with "raw" devices
  that are started without an encoding. The given `iodata`
  is written as is to the device, without conversion. For
  more information on IO data, see the "IO data" section in
  the module documentation.

  Use `write/2` for devices with encoding.

  Important: do **not** use this function on IO devices in
  Unicode mode as it will write the wrong data. In particular,
  the standard IO device is set to Unicode by default, so writing
  to stdio with this function will likely result in the wrong data
  being sent down the wire.
  """
  @spec binwrite(device, iodata) :: :ok | {:error, term}
  def binwrite(device \\ :stdio, iodata) when is_iodata(iodata) do
    :file.write(map_dev(device), iodata)
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
    :elixir_errors.io_warn(nil, nil, [to_chardata(message), ?\n])
  end

  def warn(message, [{_, _, _, opts} | _] = stacktrace) do
    formatted_trace = Enum.map_join(stacktrace, "\n  ", &Exception.format_stacktrace_entry(&1))
    message = [to_chardata(message), ?\n, "  ", formatted_trace, ?\n]
    line = opts[:line]
    file = opts[:file]
    :elixir_errors.io_warn(line, file && List.to_string(file), message)
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
  the number of Unicode code points to be retrieved.
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
  the number of Unicode code points to be retrieved.
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
  Converts chardata into a string.

  For more information about chardata, see the ["Chardata"](#module-chardata)
  section in the module documentation.

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
  Converts IO data into a binary

  The operation is Unicode unsafe.

  Notice that this function treats integers in the given IO data as
  raw bytes and does not perform any kind of encoding conversion.
  If you want to convert from a charlist to a UTF-8-encoded string,
  use `chardata_to_string/1` instead. For more information about
  IO data and chardata, see the ["IO data"](#module-io-data) section in the
  module documentation.

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
  def iodata_to_binary(iodata) do
    :erlang.iolist_to_binary(iodata)
  end

  @doc """
  Returns the size of an IO data.

  For more information about IO data, see the ["IO data"](#module-io-data)
  section in the module documentation.

  Inlined by the compiler.

  ## Examples

      iex> IO.iodata_length([1, 2 | <<3, 4>>])
      4

  """
  @spec iodata_length(iodata) :: non_neg_integer
  def iodata_length(iodata) do
    :erlang.iolist_size(iodata)
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
