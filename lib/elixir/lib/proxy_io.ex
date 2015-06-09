defmodule ProxyIO do
  @moduledoc """
  This module provides an IO device that forwards IO requests to
  another IO device.

  ## Examples

      iex> {:ok, device} = StringIO.open("foo")
      iex> {:ok, proxy} = ProxyIO.open(device)
      iex> IO.read(proxy, 2)
      "fo"

  """
  use GenServer

  @doc """
  Creates an IO device.

  The IO device forwards IO requests to another device,
  defaulting to the `group_leader` of the calling process.

  ## Examples

      iex> {:ok, device} = StringIO.open("foo")
      iex> {:ok, proxy} = ProxyIO.open(device)
      iex> IO.gets(proxy, ">")
      "foo"
      iex> IO.puts(proxy, "bar")
      iex> StringIO.contents(device)
      {"", "bar"}

  """
  @spec open(IO.device) :: {:ok, pid} | {:error, :noproc}
  def open(device \\ :stdio) do
    GenServer.start_link(__MODULE__, device)
  end

  @doc """
  Stops the IO device.
  """
  @spec close(pid, timeout) :: :ok
  def close(pid, timeout \\ 5000) do
    GenServer.call(pid, :close, timeout)
  end

  @doc false
  def init(:standard_io), do: init(Process.group_leader())
  def init(:stdio),       do: init(:standard_io)
  def init(device) when is_atom(device) do
    case Process.whereis(device) do
      nil    -> {:stop, :noproc}
      device -> init(device)
    end
  end
  def init(device) do
    Process.group_leader(self(), device)
    {:ok, device}
  end

  @doc false
  def handle_info({:io_request, from, ref, req}, state) do
    reply = request(Process.group_leader(), req)
    send(from, {:io_reply, ref, reply})
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  @doc false
  def handle_call(:close, _from, state) do
    {:stop, :normal, :ok, state}
  end

  ## Helpers

  defp request(device, req) do
    ref = Process.monitor(device)
    send(device, {:io_request, self(), ref, req})
    receive do
      {:io_reply, ^ref, reply} ->
        Process.demonitor(ref, [:flush])
        reply
      {:DOWN, ^ref, _, _, reason} ->
        exit({reason, {__MODULE__, :request, [device, req]}})
    end
  end
end
