defmodule IO do
  @moduledoc """
  Module responsible for doing IO. All the contents
  are expected to be in unicode.
  """

  # Map the Elixir names for standard io and error to Erlang names
  defmacrop map_dev(dev) do
    quote do
      case unquote(dev) do
        :stdio  -> :standard_io;
        :stderr -> :standard_error;
        other   -> other;
      end
    end
  end

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
    Erlang.io.put_chars map_dev(device), item
  end

  def print(device // :stdio, item) do
    IO.puts "IO.print is deprecated in favor of IO.write"
    Erlang.io.put_chars map_dev(device), item
  end

  @doc """
  Writes the argument to the device, similarly to write
  but adds a new line at the end. The argument is expected
  to be a chardata.
  """
  def puts(device // :stdio, item) do
    erl_dev = map_dev(device)
    Erlang.io.put_chars erl_dev, item
    Erlang.io.nl(erl_dev)
  end

  @doc """
  Inspects and writes the given argument to the device
  followed by a new line.
  """
  def inspect(device // :stdio, item) do
    puts device, Elixir.Builtin.inspect(item)
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
    Erlang.io.get_chars(map_dev(device), prompt, count)
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
    Erlang.io.get_line(map_dev(device), prompt)
  end
end
