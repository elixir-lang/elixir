defexception IO.StreamError, [:reason, :message] do
  def exception(opts) do
    reason    = opts[:reason]
    formatted = iodata_to_binary(:file.format_error(reason))
    IO.StreamError[message: "error during streaming: #{formatted}", reason: reason]
  end
end

defmodule IO do
  @moduledoc """
  Functions handling IO.

  Many functions in this module expects an IO device as argument.
  An IO device must be a pid or an atom representing a process.
  For convenience, Elixir provides `:stdio` and `:stderr` as
  shortcuts to Erlang's `:standard_io` and `:standard_error`.

  The majority of the functions expect char data, i.e. strings or
  lists of characters and strings. In case another type is given,
  it will do a conversion to string via the `String.Chars` protocol
  (as shown in typespecs).

  The functions starting with `bin*` expects iodata as argument,
  i.e. binaries or lists of bytes and binaries.
  """

  @type device :: atom | pid
  @type nodata :: { :error, term } | :eof

  import :erlang, only: [group_leader: 0]

  defmacrop is_iodata(data) do
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
  @spec read(device, :line | non_neg_integer) :: char_data | nodata
  def read(device \\ group_leader, chars_or_line)

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
  @spec binread(device, :line | non_neg_integer) :: iodata | nodata
  def binread(device \\ group_leader, chars_or_line)

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
  It returns `:ok` if it succeeds.

  ## Examples

      IO.write "sample"
      #=> "sample"

      IO.write :stderr, "error"
      #=> "error"

  """
  @spec write(device, char_data | String.Chars.t) :: :ok
  def write(device \\ group_leader(), item) do
    :io.put_chars map_dev(device), to_char_data(item)
  end

  @doc """
  Writes the given argument to the given device
  as a binary, no unicode conversion happens.

  Check `write/2` for more information.
  """
  @spec binwrite(device, iodata) :: :ok | { :error, term }
  def binwrite(device \\ group_leader(), item) when is_iodata(item) do
    :file.write map_dev(device), item
  end

  @doc """
  Writes the argument to the device, similar to `write/2`,
  but adds a newline at the end. The argument is expected
  to be a char_data.
  """
  @spec puts(device, char_data | String.Chars.t) :: :ok
  def puts(device \\ group_leader(), item) do
    erl_dev = map_dev(device)
    :io.put_chars erl_dev, [to_char_data(item), ?\n]
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
  @spec inspect(term, Keyword.t) :: term
  def inspect(item, opts \\ []) do
    inspect group_leader(), item, opts
  end

  @doc """
  Inspects the item with options using the given device.
  """
  @spec inspect(device, term, Keyword.t) :: term
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
  @spec getn(char_data | String.Chars.t, pos_integer) :: char_data | nodata
  @spec getn(device, char_data | String.Chars.t) :: char_data | nodata
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
  @spec getn(device, char_data | String.Chars.t, pos_integer) :: char_data | nodata
  def getn(device, prompt, count) do
    :io.get_chars(map_dev(device), to_char_data(prompt), count)
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
  @spec gets(device, char_data | String.Chars.t) :: char_data | nodata
  def gets(device \\ group_leader(), prompt) do
    :io.get_line(map_dev(device), to_char_data(prompt))
  end

  @doc """
  Converts the io device into a Stream. The device is
  iterated line by line if `:line` is given or by a given
  number of codepoints.

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
    Stream.unfold(map_dev(device), &do_stream(&1, line_or_codepoints))
  end

  @doc """
  Converts the io device into a Stream. The device is
  iterated line by line or by a number of bytes. This
  reads the IO as a raw binary.

  Note that an IO stream has side effects and every time
  you go over the stream you may get different results.
  """
  @spec binstream(device, :line | pos_integer) :: Enumerable.t
  def binstream(device, line_or_bytes) do
    Stream.unfold(map_dev(device), &do_binstream(&1, line_or_bytes))
  end

  @doc false
  def do_stream(device, what) do
    case read(device, what) do
      :eof ->
        nil
      { :error, reason } ->
        raise IO.StreamError, reason: reason
      data ->
        { data, device }
    end
  end

  @doc false
  def do_binstream(device, what) do
    case binread(device, what) do
      :eof ->
        nil
      { :error, reason } ->
        raise IO.StreamError, reason: reason
      data ->
        { data, device }
    end
  end

  # Map the Elixir names for standard io and error to Erlang names
  defp map_dev(:stdio),  do: :standard_io
  defp map_dev(:stderr), do: :standard_error
  defp map_dev(other),   do: other

  defp to_char_data(list) when is_list(list), do: list
  defp to_char_data(other), do: to_string(other)
end
