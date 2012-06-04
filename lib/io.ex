defmodule IO do
  @moduledoc """
  Module responsible for doing IO. All the contents
  are expected to be in unicode.
  """

  @doc """
  Reads `count` bytes from the IO device. It returns:

  * `data` - The input characters.

  * :eof - End of file was encountered.

  * {:error, reason} - Other (rare) error condition,
    for instance {:error, :estale} if reading from an
    NFS file system.
  """
  def read(device // :standard_io, count) do
    Erlang.io.get_chars(device, "", count)
  end

  @doc """
  Read a line from the IO device. It returns:

  * `data` - The input characters.

  * :eof - End of file was encountered.

  * {:error, reason} - Other (rare) error condition,
    for instance {:error, :estale} if reading from an
    NFS file system.
  """
  def readline(device // :standard_io) do
    Erlang.io.get_line(device, "")
  end

  @doc """
  Writes the given argument to the given device.
  By default the device is the standard output.
  The argument is converted to binary before
  printing.

  It returns `:ok` if it succeeds.

  ## Examples

      IO.write :sample
      #=> "sample"

      IO.write :standard_error, "error"
      #=> "error"

  """
  def write(device // :standard_io, item) do
    Erlang.io.put_chars device, to_binary(item)
  end

  def print(device // :standard_io, item) do
    IO.puts "IO.print is deprecated in favor of IO.write"
    Erlang.io.put_chars device, to_binary(item)
  end

  @doc """
  Prints the given argument to the device,
  similarly to print but adds a new line
  at the end.
  """
  def puts(device // :standard_io, item) do
    Erlang.io.put_chars device, to_binary(item)
    Erlang.io.nl(device)
  end

  @doc """
  Prints the given argument to the device
  but inspects it before.
  """
  def inspect(device // :standard_io, item) do
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
  def getb(device // :standard_io, prompt, count // 1) do
    Erlang.io.get_chars(device, prompt, count)
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
  def gets(device // :standard_io, prompt) do
    Erlang.io.get_line(device, prompt)
  end
end
