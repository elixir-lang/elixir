defmodule Port do
  @moduledoc """
  Functions related to Erlang ports.
  """

  @doc """
  See http://www.erlang.org/doc/man/erlang.html#open_port-2.
  """
  def open(name, settings) do
    :erlang.open_port(name, settings)
  end

  @doc """
  See http://www.erlang.org/doc/man/erlang.html#port_close-1.
  """
  def close(port) do
    :erlang.port_close(port)
  end

  @doc """
  See http://www.erlang.org/doc/man/erlang.html#port_command-2.
  """
  def command(port, data, options // []) do
    :erlang.port_command(port, data, options)
  end

  @doc """
  See http://www.erlang.org/doc/man/erlang.html#port_connect-2.
  """
  def connect(port, pid) do
    :erlang.port_connect(port, pid)
  end

  @doc """
  See http://www.erlang.org/doc/man/erlang.html#port_control-3.
  """
  def control(port, operation, data) do
    :erlang.port_control(port, operation, data)
  end

  @doc """
  See http://www.erlang.org/doc/man/erlang.html#port_call-3.
  """
  def call(port, operation, data) do
    :erlang.port_call(port, operation, data)
  end

  @doc """
  See http://www.erlang.org/doc/man/erlang.html#port_info-1.
  """
  def info(port) do
    :erlang.port_info(port)
  end

  @doc """
  See http://www.erlang.org/doc/man/erlang.html#port_info-2.
  """
  def info(port, item) do
    :erlang.port_info(port, item)
  end

  @doc """
  See http://www.erlang.org/doc/man/erlang.html#ports-0.
  """
  def list do
    :erlang.ports
  end
end