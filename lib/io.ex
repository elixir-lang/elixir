defmodule IO do
  @moduledoc """
  Module responsible for doing IO.
  It is incomplete now. More functions will be
  added in upcoming releases.
  """

  @doc """
  Prints the given argument to the given device.
  By default the device is the standard output.
  The argument is converted to binary before
  printing.

  It returns `:ok` if it succeeds.

  ## Examples

      IO.print :sample
      #=> "sample"

      IO.print :standard_error, "error"
      #=> "error"

  """
  def print(device // :standard_io, item) do
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
  Reads `count` characters from the IO device.
  It returns:

  * `data` - The input characters. If the device
    supports Unicode, the data may represent
    codepoints larger than 255 (the latin1 range).
    If the io_server() is set to deliver binaries,
    they will be encoded in UTF-8 (regardless of if
    the device actually supports Unicode or not).

  * :eof - End of file was encountered.

  * {:error, reason} - Other (rare) error condition,
    for instance {:error, :estale} if reading from an
    NFS file system.
  """
  def getb(device, count // 1) do
    Erlang.io.get_chars(device, '', count)
  end

  @doc """
  Reads a line from the IO device. It returns:

  * `data` - The characters in the line terminated
    by a LF (or end of file). If the device supports
    Unicode, the data may represent codepoints larger
    than 255 (the latin1 range).  If the io_server()
    is set to deliver binaries, they will be encoded
    in UTF-8 (regardless of if the device actually
    supports Unicode or not).

  * :eof - End of file was encountered.

  * {:error, reason} - Other (rare) error condition,
    for instance {:error, :estale} if reading from an
    NFS file system.
  """
  def gets(devise) do
    Erlang.io.get_line(devise, '')
  end
end
