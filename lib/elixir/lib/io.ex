defexception IO.StreamError, reason: nil do
  def message(exception) do
    formatted = iolist_to_binary(:file.format_error(reason exception))
    "error during streaming: #{formatted}"
  end
end

defmodule IO do
  @moduledoc """
  Module responsible for doing IO. Many functions in this
  module expects an IO device and an io data encoded in UTF-8.
  Use the bin* functions if the data is binary, useful when
  working with raw bytes or when no unicode conversion should
  be performed.

  An IO device must be a pid or an atom representing a process.
  For convenience, Elixir provides `:stdio` and `:stderr` as
  shortcuts to Erlang's `:standard_io` and `:standard_error`.

  An io data can be:

  * A list of integers representing a string. Any unicode
    character must be represented with one entry in the list,
    this entry being an integer with the codepoint value;

  * A binary in which unicode characters are represented
    with many bytes (Elixir's default representation);

  * A list of binaries or a list of char lists (as described above);

  """

  import :erlang, only: [group_leader: 0]

  defmacrop is_iolist(data) do
    quote do
      is_list(unquote(data)) or is_binary(unquote(data))
    end
  end

  @doc """
  Reads `count` characters from the IO device or until
  the end of the line if `:line` is given. It returns:

  * `data` - The input characters.

  * `:eof` - End of file was encountered.

  * `{:error, reason}` - Other (rare) error condition,
    for instance `{:error, :estale}` if reading from an
    NFS file system.
  """
  def read(device // group_leader, chars_or_line)

  def read(device, :line) do
    :io.get_line(map_dev(device), '')
  end

  def read(device, count) when count >= 0 do
    :io.get_chars(map_dev(device), '', count)
  end

  @doc """
  Reads `count` bytes from the IO device or until
  the end of the line if `:line` is given. It returns:

  * `data` - The input characters.

  * `:eof` - End of file was encountered.

  * `{:error, reason}` - Other (rare) error condition,
    for instance `{:error, :estale}` if reading from an
    NFS file system.
  """
  def binread(device // group_leader, chars_or_line)

  def binread(device, :line) do
    case :file.read_line(map_dev(device)) do
      { :ok, data } -> data
      other -> other
    end
  end

  def binread(device, count) when count >= 0 do
    case :file.read(map_dev(device), count) do
      { :ok, data } -> data
      other -> other
    end
  end

  @doc """
  Writes the given argument to the given device.
  By default the device is the standard output.
  The argument is expected to be a chardata (i.e.
  a char list or an unicode binary).

  It returns `:ok` if it succeeds.

  ## Examples

      IO.write "sample"
      #=> "sample"

      IO.write :stderr, "error"
      #=> "error"

  """
  def write(device // group_leader(), item) when is_iolist(item) do
    :io.put_chars map_dev(device), item
  end

  @doc """
  Writes the given argument to the given device
  as a binary, no unicode conversion happens.

  Check `write/2` for more information.
  """
  def binwrite(device // group_leader(), item) when is_iolist(item) do
    :file.write map_dev(device), item
  end

  @doc """
  Writes the argument to the device, similar to `write/2`,
  but adds a newline at the end. The argument is expected
  to be a chardata.
  """
  def puts(device // group_leader(), item) when is_iolist(item) do
    erl_dev = map_dev(device)
    :io.put_chars erl_dev, [item, ?\n]
  end

  @doc """
  Inspects and writes the given argument to the device
  followed by a newline. A set of options can be given.

  It sets by default pretty printing to true and the
  width to be the width of the device, with a minimum
  of 80 characters.

  ## Examples

      IO.inspect Process.list

  """
  def inspect(item, opts // []) do
    inspect group_leader(), item, opts
  end

  @doc """
  Inspects the item with options using the given device.
  """
  def inspect(device, item, opts) when is_list(opts) do
    opts = Keyword.put_new(opts, :pretty, true)

    unless Keyword.get(opts, :width) do
      opts = case :io.columns(device) do
        { :ok, width } -> Keyword.put(opts, :width, width)
        { :error, _ }  -> opts
      end
    end

    puts device, Kernel.inspect(item, opts)
    item
  end

  @doc """
  Gets a number of bytes from the io device. If the
  io device is a unicode device, `count` implies
  the number of unicode codepoints to be retrieved.
  Otherwise, `count` is the number of raw bytes to be retrieved.
  It returns:

  * `data` - The input characters.

  * `:eof` - End of file was encountered.

  * `{:error, reason}` - Other (rare) error condition,
    for instance `{:error, :estale}` if reading from an
    NFS file system.
  """
  def getn(prompt, count // 1)

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
  def getn(device, prompt, count) do
    :io.get_chars(map_dev(device), prompt, count)
  end

  @doc """
  Reads a line from the IO device. It returns:

  * `data` - The characters in the line terminated
    by a LF (or end of file).

  * `:eof` - End of file was encountered.

  * `{:error, reason}` - Other (rare) error condition,
    for instance `{:error, :estale}` if reading from an
    NFS file system.
  """
  def gets(device // group_leader(), prompt) do
    :io.get_line(map_dev(device), prompt)
  end

  @doc """
  Converts the io device into a Stream. The device is
  iterated line by line if :line is given or by a given
  number of codepoints.

  This reads the io as utf-8. Check out
  `IO.binstream/2` to handle the IO as a raw binary.

  ## Examples

  Here is an example on how we mimic an echo server
  from the command line:

      Enum.each IO.stream(:stdio, :line), &IO.write(&1)

  """
  def stream(device, line_or_bytes) do
    fn(acc, f) -> stream(map_dev(device), line_or_bytes, acc, f) end
  end

  @doc """
  Converts the io device into a Stream. The device is
  iterated line by line.

  This reads the io as a raw binary.
  """
  def binstream(device, line_or_bytes) do
    fn(acc, f) -> binstream(map_dev(device), line_or_bytes, acc, f) end
  end

  @doc false
  def stream(device, what, acc, fun) do
    case read(device, what) do
      :eof ->
        acc
      { :error, reason } ->
        raise IO.StreamError, reason: reason
      data ->
        stream(device, what, fun.(data, acc), fun)
    end
  end

  @doc false
  def binstream(device, what, acc, fun) do
    case binread(device, what) do
      :eof ->
        acc
      { :error, reason } ->
        raise IO.StreamError, reason: reason
      data ->
        binstream(device, what, fun.(data, acc), fun)
    end
  end

  # Map the Elixir names for standard io and error to Erlang names
  defp map_dev(:stdio),  do: :standard_io
  defp map_dev(:stderr), do: :standard_error
  defp map_dev(other),   do: other
end
