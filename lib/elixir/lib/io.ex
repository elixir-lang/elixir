defmodule IO do
  @moduledoc """
  Module responsible for doing IO. The function in this
  module expects an iodata as argument encoded in UTF-8.
  An iodata can be:

  * A list of integers representing a string. Any unicode
    character must be represented with one entry in the list,
    this entry being an integer with the codepoint value;
  * A binary in which unicode characters are represented
    with many bytes (Elixir's default representation);
  * A list of binaries or a list of char lists (as described above);
  * If none of the above, `to_binary` is invoked in the
    given argument;

  """

  @doc """
  Reads `count` bytes from the IO device. It returns:

  * `data` - The input characters.

  * :eof - End of file was encountered.

  * {:error, reason} - Other (rare) error condition,
    for instance {:error, :estale} if reading from an
    NFS file system.
  """
  def read(device // :stdio, count) do
    Erlang.io.get_chars(map_dev(device), "", count)
  end

  @doc """
  Read a line from the IO device. It returns:

  * `data` - The input characters.

  * :eof - End of file was encountered.

  * {:error, reason} - Other (rare) error condition,
    for instance {:error, :estale} if reading from an
    NFS file system.

  This function does the same as `gets/2`,
  except the prompt is not required as argument.
  """
  def readline(device // :stdio) do
    Erlang.io.get_line(map_dev(device), "")
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

      IO.write :standard_error, "error"
      #=> "error"

  """
  def write(device // :stdio, item) do
    Erlang.io.put_chars map_dev(device), to_iodata(item)
  end

  def print(device // :stdio, item) do
    IO.puts "IO.print is deprecated in favor of IO.write"
    Erlang.io.put_chars map_dev(device), to_iodata(item)
  end

  @doc """
  Writes the argument to the device, similarly to write
  but adds a new line at the end. The argument is expected
  to be a chardata.
  """
  def puts(device // :stdio, item) do
    erl_dev = map_dev(device)
    Erlang.io.put_chars erl_dev, to_iodata(item)
    Erlang.io.nl(erl_dev)
  end

  @doc """
  Inspects and writes the given argument to the device
  followed by a new line. Returns the item given.
  """
  def inspect(device // :stdio, item) do
    puts device, Elixir.Builtin.inspect(item)
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
  def getb(device // :stdio, prompt, count // 1) do
    Erlang.io.get_chars(map_dev(device), to_iodata(prompt), count)
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
  def gets(device // :stdio, prompt) do
    Erlang.io.get_line(map_dev(device), to_iodata(prompt))
  end

  # Map the Elixir names for standard io and error to Erlang names
  defp map_dev(:stdio),  do: :standard_io
  defp map_dev(:stderr), do: :standard_error
  defp map_dev(other),   do: other

  defp to_iodata(io) when is_list(io) or is_binary(io), do: io
  defp to_iodata(other), do: to_binary(other)
end
