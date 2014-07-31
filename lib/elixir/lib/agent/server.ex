defmodule Agent.Server do
  @moduledoc false

  use GenServer

  def init(fun) do
    {:ok, run(fun, [])}
  end

  def handle_call({:get, fun}, _from, state) do
    {:reply, run(fun, [state]), state}
  end

  def handle_call({:get_and_update, fun}, _from, state) do
    {reply, state} = run(fun, [state])
    {:reply, reply, state}
  end

  def handle_call({:update, fun}, _from, state) do
    {:reply, :ok, run(fun, [state])}
  end

  def handle_call(:stop, _from, state) do
    {:stop, :normal, :ok, state}
  end

  def handle_call(msg, from, state) do
    super(msg, from, state)
  end

  def handle_cast({:cast, fun}, state) do
    {:noreply, run(fun, [state])}
  end

  def handle_cast(msg, state) do
    super(msg, state)
  end

  def code_change(_old, state, fun) do
    {:ok, run(fun, [state])}
  end

  def terminate(_reason, _state) do
    # There is a race condition if the agent is
    # restarted too fast and it is registered.
    try do
      self |> :erlang.process_info(:registered_name) |> elem(1) |> Process.unregister
    rescue
      _ -> :ok
    end
    :ok
  end

  defp run({m, f, a}, extra), do: apply(m, f, extra ++ a)
  defp run(fun, extra), do: apply(fun, extra)
end
