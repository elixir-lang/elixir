defmodule IO do
  @moduledoc """
  Module responsible for doing IO. Many functions in this
  module expects an IO device and an io data encoded in UTF-8.

  An IO device must be a pid or an atom representing a process.
  For convenience, Elixir provides `:stdio` and `:stderr` as
  shortcut to Erlang's `:standard_io` and `:standard_error`.

  An io data can be:

  * A list of integers representing a string. Any unicode
    character must be represented with one entry in the list,
    this entry being an integer with the codepoint value;

  * A binary in which unicode characters are represented
    with many bytes (Elixir's default representation);

  * A list of binaries or a list of char lists (as described above);

  * If none of the above, `to_binary` is invoked in the
    given argument;

  """

  import :erlang, only: [group_leader: 0]

  @doc """
  Reads `count` bytes from the IO device. It returns:

  * `data` - The input characters.

  * :eof - End of file was encountered.

  * {:error, reason} - Other (rare) error condition,
    for instance {:error, :estale} if reading from an
    NFS file system.
  """
  def read(device // group_leader(), count) do
    :io.get_chars(map_dev(device), "", count)
  end

  @doc """
  Reads `count` bytes from the IO device as binary,
  no unicode conversion happens.

  Check `read/2` for more information.
  """
  def binread(device // group_leader(), count) do
    case :file.read(map_dev(device), count) do
      { :ok, data } -> data
      other -> other
    end
  end

  @doc """
  Reads a line from the IO device. It returns:

  * `data` - The input characters.

  * :eof - End of file was encountered.

  * {:error, reason} - Other (rare) error condition,
    for instance {:error, :estale} if reading from an
    NFS file system.

  This function does the same as `gets/2`,
  except the prompt is not required as argument.
  """
  def readline(device // group_leader()) do
    :io.get_line(map_dev(device), "")
  end

  @doc """
  Reads a line from the IO device as binary,
  no unicode conversion happens.

  Check `readline/1` for more information.
  """
  def binreadline(device // group_leader()) do
    case :file.read_line(map_dev(device)) do
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
  def write(device // group_leader(), item) do
    :io.put_chars map_dev(device), to_iodata(item)
  end

  @doc """
  Writes the given argument to the given device
  as a binary, no unicode conversion happens.

  Check `write/2` for more information.
  """
  def binwrite(device // group_leader(), item) do
    :file.write map_dev(device), to_iodata(item)
  end

  @doc """
  Writes the argument to the device, similarly to write
  but adds a new line at the end. The argument is expected
  to be a chardata.
  """
  def puts(device // group_leader(), item) do
    erl_dev = map_dev(device)
    :io.put_chars erl_dev, [to_iodata(item), ?\n]
  end

  @doc """
  Inspects and writes the given argument to the device
  followed by a new line. A set of options can be given.

  ## Examples

      IO.inspect Process.list

  """
  def inspect(item, opts // []) do
    inspect group_leader(), item, opts
  end

  @doc """
  Inspects the item with options using the given device.
  """
  def inspect(device, item, opts) do
    puts device, Kernel.inspect(item, opts)
    item
  end

  @doc """
  Gets `count` bytes from the IO device. It returns:

  * `data` - The input characters.

  * :eof - End of file was encountered.

  * {:error, reason} - Other (rare) error condition,
    for instance {:error, :estale} if reading from an
    NFS file system.
  """
  def getb(prompt, count // 1)

  def getb(prompt, count) when is_integer(count) do
    getb(group_leader, prompt, count)
  end

  def getb(device, prompt) do
    getb(device, prompt, 1)
  end

  @doc """
  Gets `count` bytes from the chosen IO device.
  """
  def getb(device, prompt, count) do
    :io.get_chars(map_dev(device), to_iodata(prompt), count)
  end

  @doc """
  Reads a line from the IO device. It returns:

  * `data` - The characters in the line terminated
    by a LF (or end of file).

  * :eof - End of file was encountered.

  * {:error, reason} - Other (rare) error condition,
    for instance {:error, :estale} if reading from an
    NFS file system.
  """
  def gets(device // group_leader(), prompt) do
    :io.get_line(map_dev(device), to_iodata(prompt))
  end

  # Map the Elixir names for standard io and error to Erlang names
  defp map_dev(:stdio),  do: :standard_io
  defp map_dev(:stderr), do: :standard_error
  defp map_dev(other),   do: other

  defp to_iodata(io) when is_list(io) or is_binary(io), do: io
  defp to_iodata(other), do: to_binary(other)
end
