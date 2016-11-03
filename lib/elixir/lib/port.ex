defmodule Port do
  @moduledoc """
  Functions related to Erlang ports.
  """

  @type name :: {:spawn, charlist | binary} |
                {:spawn_driver, charlist | binary} |
                {:spawn_executable, charlist | atom} |
                {:fd, non_neg_integer, non_neg_integer}

  @doc """
  Opens an Erlang port given a tuple `name` and a list of `settings`.

  ## Name

  The supported values for `name` are:

    * `{:spawn, command}` - runs an external program. The first space separated
      word of `command` will be considered as the name of the program to run, so
      use `{:spawn_executable, command}` to run a program having spaces in its name.
    * `{:spawn_driver, command}` - similar to `{:spawn, command}`, but to run a
      loaded driver.
    * `{:spawn_executable, filename}` - similar to `{:spawn, filename}`, but to runs
      an external executable. With this option, `filename` in its whole is considered
      the name of the program to execute.
    * `{:fd, fd_in, fd_out}` - accesses file descriptors used by Erlang, `fd_in`
      being used for standard input, `fd_out` for standard output.

  For more information, see [`:erlang.open_port/2`](http://www.erlang.org/doc/man/erlang.html#open_port-2).

  Inlined by the compiler.
  """
  @spec open(name, list) :: port
  def open(name, settings) do
    :erlang.open_port(name, settings)
  end

  @doc """
  Closes the `port`.

  For more information, see [`:erlang.port_close/1`](http://www.erlang.org/doc/man/erlang.html#port_close-1).

  Inlined by the compiler.
  """
  @spec close(port) :: true
  def close(port) do
    :erlang.port_close(port)
  end

  @doc """
  Sends `data` to the port driver `port`.

  For more information, see [`:erlang.port_command/2`](http://www.erlang.org/doc/man/erlang.html#port_command-2).

  Inlined by the compiler.
  """
  @spec command(port, iodata, [:force | :nosuspend]) :: boolean
  def command(port, data, options \\ []) do
    :erlang.port_command(port, data, options)
  end

  @doc """
  Associates the `port` identifier with a `pid`.

  For more information, see [`:erlang.port_connect/2`](http://www.erlang.org/doc/man/erlang.html#port_connect-2).

  Inlined by the compiler.
  """
  @spec connect(port, pid) :: true
  def connect(port, pid) do
    :erlang.port_connect(port, pid)
  end

  @doc """
  Sends a synchronous control command to the `port` and returns its reply as a binary.

  Not all port drivers support this feature.

  For more information, see [`:erlang.port_control/3`](http://www.erlang.org/doc/man/erlang.html#port_control-3).

  Inlined by the compiler.
  """
  @spec control(port, integer, iodata) :: iodata | binary
  def control(port, operation, data) do
    :erlang.port_control(port, operation, data)
  end

  @doc """
  Makes a synchronous call to the `port` and returns its reply as a term.

  Not all port drivers support this control feature.

  For more information, see [`:erlang.port_call/3`](http://www.erlang.org/doc/man/erlang.html#port_call-3).

  Inlined by the compiler.
  """
  @spec call(port, integer, term) :: term
  def call(port, operation, data) do
    :erlang.port_call(port, operation, data)
  end

  @doc """
  Returns information about the `port`
  or `nil` if the port is closed.

  For more information, see [`:erlang.port_info/1`](http://www.erlang.org/doc/man/erlang.html#port_info-1).
  """
  def info(port) do
    nillify :erlang.port_info(port)
  end

  @doc """
  Returns information about the `port`
  or `nil` if the port is closed.

  For more information, see [`:erlang.port_info/2`](http://www.erlang.org/doc/man/erlang.html#port_info-2).
  """
  @spec info(port, atom) :: {atom, term} | nil
  def info(port, spec)

  def info(port, :registered_name) do
    case :erlang.port_info(port, :registered_name) do
      [] -> {:registered_name, []}
      other -> nillify(other)
    end
  end

  def info(port, item) do
    nillify :erlang.port_info(port, item)
  end

  @doc """
  Returns a list of the ports for the current node.

  For more information, see [`:erlang.ports/0`](http://www.erlang.org/doc/man/erlang.html#ports-0).

  Inlined by the compiler.
  """
  @spec list :: [port]
  def list do
    :erlang.ports
  end

  @compile {:inline, nillify: 1}
  defp nillify(:undefined), do: nil
  defp nillify(other),      do: other
end
