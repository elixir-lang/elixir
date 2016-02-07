defmodule IO do
  @moduledoc """
  Functions handling IO.

  Many functions in this module expect an IO device as an argument.
  An IO device must be a pid or an atom representing a process.
  For convenience, Elixir provides `:stdio` and `:stderr` as
  shortcuts to Erlang's `:standard_io` and `:standard_error`.

  The majority of the functions expect char data, i.e. strings or
  lists of characters and strings. In case another type is given,
  functions will convert to string via the `String.Chars` protocol
  (as shown in typespecs).

  The functions starting with `bin*` expect iodata as an argument,
  i.e. binaries or lists of bytes and binaries.

  ## IO devices

  An IO device may be an atom or a pid. In case it is an atom,
  the atom must be the name of a registered process. In addition,
  Elixir provides two shortcuts:

    * `:stdio` - a shortcut for `:standard_io`, which maps to
      the current `Process.group_leader/0` in Erlang

    * `:stderr` - a shortcut for the named process `:standard_error`
      provided in Erlang

  IO devices maintain their position, that means subsequent calls to any
  reading or writing functions will start from the place when the device
  was last accessed. Position of files can be changed using the
  `:file.position/2` function.

  """

  @type device :: atom | pid
  @type nodata :: {:error, term} | :eof
  @type chardata() :: :unicode.chardata()

  import :erlang, only: [group_leader: 0]

  defmacrop is_iodata(data) do
    quote do
      is_list(unquote(data)) or is_binary(unquote(data))
    end
  end

  @doc """
  Reads `count` characters from the IO device, a whole
  `:line` or the whole device with `:all`.

  It returns:

    * `data` - the input characters

    * `:eof` - end of file was encountered

    * `{:error, reason}` - other (rare) error condition;
      for instance, `{:error, :estale}` if reading from an
      NFS volume

  If `:all` is given, `:eof` is never returned, but an
  empty string in case the device has reached EOF.
  """
  @spec read(device, :all | :line | non_neg_integer) :: chardata | nodata
  def read(device \\ group_leader, chars_or_line)

  def read(device, :all) do
    do_read_all(map_dev(device), "")
  end

  def read(device, :line) do
    :io.get_line(map_dev(device), '')
  end

  def read(device, count) when count >= 0 do
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
  Reads `count` characters from the IO device, a whole
  `:line` or the whole device with `:all`.

  It returns:

    * `data` - the input characters

    * `:eof` - end of file was encountered

    * `{:error, reason}` - other (rare) error condition;
      for instance, `{:error, :estale}` if reading from an
      NFS volume

  If `:all` is given, `:eof` is never returned, but an
  empty string in case the device has reached EOF.

  Note: do not use this function on IO devices in unicode mode
  as it will return the wrong result.
  """
  @spec binread(device, :all | :line | non_neg_integer) :: iodata | nodata
  def binread(device \\ group_leader, chars_or_line)

  def binread(device, :all) do
    do_binread_all(map_dev(device), "")
  end

  def binread(device, :line) do
    case :file.read_line(map_dev(device)) do
      {:ok, data} -> data
      other -> other
    end
  end

  def binread(device, count) when count >= 0 do
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
  Writes the given argument to the given device.

  By default the device is the standard output.
  It returns `:ok` if it succeeds.

  ## Examples

      IO.write "sample"
      #=> "sample"

      IO.write :stderr, "error"
      #=> "error"

  """
  @spec write(device, chardata | String.Chars.t) :: :ok
  def write(device \\ group_leader(), item) do
    :io.put_chars map_dev(device), to_chardata(item)
  end

  @doc """
  Writes the given argument to the given device
  as a binary, no unicode conversion happens.

  Check `write/2` for more information.

  Note: do not use this function on IO devices in unicode mode
  as it will return the wrong result.
  """
  @spec binwrite(device, iodata) :: :ok | {:error, term}
  def binwrite(device \\ group_leader(), item) when is_iodata(item) do
    :file.write map_dev(device), item
  end

  @doc """
  Writes the argument to the device, similar to `write/2`,
  but adds a newline at the end. The argument is expected
  to be a chardata.
  """
  @spec puts(device, chardata | String.Chars.t) :: :ok
  def puts(device \\ group_leader(), item) do
    erl_dev = map_dev(device)
    :io.put_chars erl_dev, [to_chardata(item), ?\n]
  end

  @doc """
  Inspects and writes the given argument to the device.

  It enables pretty printing by default with width of
  80 characters. The width can be changed by explicitly
  passing the `:width` option.

  See `Inspect.Opts` for a full list of options.

  ## Examples

      IO.inspect Process.list, width: 40

  """
  @spec inspect(item, Keyword.t) :: item when item: var
  def inspect(item, opts \\ []) do
    inspect group_leader(), item, opts
  end

  @doc """
  Inspects the item with options using the given device.

  See `Inspect.Opts` for a full list of options.
  """
  @spec inspect(device, item, Keyword.t) :: item when item: var
  def inspect(device, item, opts) when is_list(opts) do
    opts   = struct(Inspect.Opts, opts)
    iodata = Inspect.Algebra.format(Inspect.Algebra.to_doc(item, opts), opts.width)
    puts device, iodata
    item
  end

  @doc """
  Gets a number of bytes from the io device. If the
  io device is a unicode device, `count` implies
  the number of unicode codepoints to be retrieved.
  Otherwise, `count` is the number of raw bytes to be retrieved.
  It returns:

    * `data` - the input characters

    * `:eof` - end of file was encountered

    * `{:error, reason}` - other (rare) error condition;
      for instance, `{:error, :estale}` if reading from an
      NFS volume
  """
  @spec getn(chardata | String.Chars.t, pos_integer) :: chardata | nodata
  @spec getn(device, chardata | String.Chars.t) :: chardata | nodata
  def getn(prompt, count \\ 1)

  def getn(prompt, count) when is_integer(count) do
    getn(group_leader, prompt, count)
  end

  def getn(device, prompt) do
    getn(device, prompt, 1)
  end

  @doc """
  Gets a number of bytes from the io device. If the
  io device is a unicode device, `count` implies
  the number of unicode codepoints to be retrieved.
  Otherwise, `count` is the number of raw bytes to be retrieved.
  """
  @spec getn(device, chardata | String.Chars.t, pos_integer) :: chardata | nodata
  def getn(device, prompt, count) do
    :io.get_chars(map_dev(device), to_chardata(prompt), count)
  end

  @doc """
  Reads a line from the IO device.

  It returns:

    * `data` - the characters in the line terminated
      by a line-feed (LF) or end of file (EOF)

    * `:eof` - end of file was encountered

    * `{:error, reason}` - other (rare) error condition;
      for instance, `{:error, :estale}` if reading from an
      NFS volume

  ## Examples

  To display "What is your name?" as a prompt and await user input:

      IO.gets "What is your name?"
  """
  @spec gets(device, chardata | String.Chars.t) :: chardata | nodata
  def gets(device \\ group_leader(), prompt) do
    :io.get_line(map_dev(device), to_chardata(prompt))
  end

  @doc """
  Converts the io device into a `IO.Stream`.

  An `IO.Stream` implements both `Enumerable` and
  `Collectable`, allowing it to be used for both read
  and write.

  The device is iterated line by line if `:line` is given or
  by a given number of codepoints.

  This reads the IO as utf-8. Check out
  `IO.binstream/2` to handle the IO as a raw binary.

  Note that an IO stream has side effects and every time
  you go over the stream you may get different results.

  ## Examples

  Here is an example on how we mimic an echo server
  from the command line:

      Enum.each IO.stream(:stdio, :line), &IO.write(&1)

  """
  @spec stream(device, :line | pos_integer) :: Enumerable.t
  def stream(device, line_or_codepoints) do
    IO.Stream.__build__(map_dev(device), false, line_or_codepoints)
  end

  @doc """
  Converts the IO device into a `IO.Stream`.

  An `IO.Stream` implements both `Enumerable` and
  `Collectable`, allowing it to be used for both read
  and write.

  The device is iterated line by line or by a number of bytes.
  This reads the IO device as a raw binary.

  Note that an IO stream has side effects and every time
  you go over the stream you may get different results.

  Finally, do not use this function on IO devices in unicode
  mode as it will return the wrong result.
  """
  @spec binstream(device, :line | pos_integer) :: Enumerable.t
  def binstream(device, line_or_bytes) do
    IO.Stream.__build__(map_dev(device), true, line_or_bytes)
  end

  @doc """
  Converts chardata (a list of integers representing codepoints,
  lists and strings) into a string.

  In case the conversion fails, it raises a `UnicodeConversionError`.
  If a string is given, returns the string itself.

  ## Examples

      iex> IO.chardata_to_string([0x00E6, 0x00DF])
      "æß"

      iex> IO.chardata_to_string([0x0061, "bc"])
      "abc"

  """
  @spec chardata_to_string(chardata) :: String.t | no_return
  def chardata_to_string(string) when is_binary(string) do
    string
  end

  def chardata_to_string(list) when is_list(list) do
    List.to_string(list)
  end

  @doc """
  Converts iodata (a list of integers representing bytes, lists
  and binaries) into a binary.

  Notice that this function treats lists of integers as raw bytes
  and does not perform any kind of encoding conversion. If you want
  to convert from a char list to a string (UTF-8 encoded), please
  use `chardata_to_string/1` instead.

  If this function receives a binary, the same binary is returned.

  Inlined by the compiler.

  ## Examples

      iex> bin1 = <<1, 2, 3>>
      iex> bin2 = <<4, 5>>
      iex> bin3 = <<6>>
      iex> IO.iodata_to_binary([bin1, 1, [2, 3, bin2], 4|bin3])
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

      iex> IO.iodata_length([1, 2|<<3, 4>>])
      4

  """
  @spec iodata_length(iodata) :: non_neg_integer
  def iodata_length(item) do
    :erlang.iolist_size(item)
  end

  @doc false
  def each_stream(device, what) do
    case read(device, what) do
      :eof ->
        {:halt, device}
      {:error, reason} ->
        raise IO.StreamError, reason: reason
      data ->
        {[data], device}
    end
  end

  @doc false
  def each_binstream(device, what) do
    case binread(device, what) do
      :eof ->
        {:halt, device}
      {:error, reason} ->
        raise IO.StreamError, reason: reason
      data ->
        {[data], device}
    end
  end

  @compile {:inline, map_dev: 1, to_chardata: 1}

  # Map the Elixir names for standard io and error to Erlang names
  defp map_dev(:stdio),  do: :standard_io
  defp map_dev(:stderr), do: :standard_error
  defp map_dev(other) when is_atom(other) or is_pid(other) or is_tuple(other), do: other

  defp to_chardata(list) when is_list(list), do: list
  defp to_chardata(other), do: to_string(other)
end
