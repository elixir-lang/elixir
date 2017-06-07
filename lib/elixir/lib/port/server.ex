defmodule Port.Server do
  @moduledoc """
  Utility GenServer to exchange data with a port.
  It is used by `Port.Stream`.

  """

  use GenServer

  def start_link(name, settings, raw) do
    GenServer.start_link(__MODULE__, {name, settings, raw})
  end

  def fetch(pid) do
    GenServer.call(pid, :fetch)
  end

  def command(pid, data, options \\ []) do
    GenServer.call(pid, {:command, data, options})
  end

  def close(pid) do
    GenServer.call(pid, :close)
  end


  def init({name, settings, raw}) do
    {:ok, %{
      port: Port.open(name, settings),
      buffer: :queue.new(),
      terminated: false,
      raw: raw,
      line: nil
    }}
  end


  def handle_call(:fetch, _from, %{terminated: true} = state) do
    {:reply, {:halt, self()}, state}
  end

  def handle_call(:fetch, _from, %{buffer: buffer} = state) do
    {value, new_buffer} = case :queue.out(buffer) do
      {{:value, value}, new_buffer} -> {[value], new_buffer}
      {:empty, new_buffer}          -> {[], new_buffer}
    end

    {:reply, {value, self()}, %{state | buffer: new_buffer}}
  end

  def handle_call({:command, data, options}, _from, %{port: port} = state) do
    Port.command(port, data, options)
    {:reply, :ok, state}
  end

  def handle_call(:close, _from, %{port: port} = state) do
    Port.close(port)
    {:reply, :ok, state}
  end

  def handle_info({_port, {:data, data}}, %{raw: true, buffer: buffer} = state) do
    {:noreply, %{state | buffer: :queue.in(data, buffer)}}
  end

  def handle_info({_port, {:data, {:eol, data}}}, %{line: nil, buffer: buffer} = state) do
    {:noreply, %{state | buffer: :queue.in(data, buffer)}}
  end

  def handle_info({_port, {:data, {:eol, data}}}, %{line: line, buffer: buffer} = state) do
    buffer = :queue.in(concat(line, data), buffer)
    {:noreply, %{state | line: nil, buffer: buffer}}
  end

  def handle_info({_port, {:data, {:noeol, data}}}, %{line: nil} = state) do
    {:noreply, %{state | line: data}}
  end

  def handle_info({_port, {:data, {:noeol, data}}}, %{line: line} = state) do
    {:noreply, %{state | line: concat(line, data)}}
  end

  def handle_info({_port, {:data, data}}, %{buffer: buffer} = state) do
    {:noreply, %{state | buffer: :queue.in(data, buffer)}}
  end


  def handle_info({_port, {:exit_status, _status}}, state) do
    {:noreply, %{state | terminated: true}}
  end

  def handle_info({_port, :eof}, state) do
    {:noreply, %{state | terminated: true}}
  end

  def terminate(_reason, _state) do
    :ok
  end


  defp concat(line, data) when is_binary(data), do: line <> data
  defp concat(line, data), do: line ++ data
end
