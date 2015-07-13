defmodule Port do
  @moduledoc """
  Functions related to Erlang ports.
  """

  @doc """
  See http://www.erlang.org/doc/man/erlang.html#open_port-2

  Inlined by the compiler.
  """
  def open(name, settings) do
    :erlang.open_port(name, settings)
  end

  @doc """
  See http://www.erlang.org/doc/man/erlang.html#port_close-1

  Inlined by the compiler.
  """
  def close(port) do
    :erlang.port_close(port)
  end

  @doc """
  See http://www.erlang.org/doc/man/erlang.html#port_command-2

  Inlined by the compiler.
  """
  def command(port, data, options \\ []) do
    :erlang.port_command(port, data, options)
  end

  @doc """
  See http://www.erlang.org/doc/man/erlang.html#port_connect-2

  Inlined by the compiler.
  """
  def connect(port, pid) do
    :erlang.port_connect(port, pid)
  end

  @doc """
  See http://www.erlang.org/doc/man/erlang.html#port_control-3

  Inlined by the compiler.
  """
  def control(port, operation, data) do
    :erlang.port_control(port, operation, data)
  end

  @doc """
  See http://www.erlang.org/doc/man/erlang.html#port_call-3

  Inlined by the compiler.
  """
  def call(port, operation, data) do
    :erlang.port_call(port, operation, data)
  end

  @doc """
  Returns information about the `port`
  or `nil` if the port is closed.

  See http://www.erlang.org/doc/man/erlang.html#port_info-1
  """
  def info(port) do
    nillify :erlang.port_info(port)
  end

  @doc """
  Returns information about the `port`
  or `nil` if the port is closed.

  See http://www.erlang.org/doc/man/erlang.html#port_info-2
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
  See http://www.erlang.org/doc/man/erlang.html#ports-0

  Inlined by the compiler.
  """
  def list do
    :erlang.ports
  end

  @compile {:inline, nillify: 1}
  defp nillify(:undefined), do: nil
  defp nillify(other),      do: other
end
