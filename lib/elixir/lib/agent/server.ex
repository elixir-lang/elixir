defmodule Agent.Server do
  @moduledoc false

  use GenServer

  def init(fun) do
    {:ok, fun.()}
  end

  def handle_call({:get, fun}, _from, state) do
    {:reply, fun.(state), state}
  end

  def handle_call({:get_and_update, fun}, _from, state) do
    {reply, state} = fun.(state)
    {:reply, reply, state}
  end

  def handle_call({:update, fun}, _from, state) do
    {:reply, :ok, fun.(state)}
  end

  def handle_call(:stop, _from, state) do
    {:stop, :normal, :ok, state}
  end

  def handle_call(msg, from, state) do
    super(msg, from, state)
  end

  def handle_cast(fun, state) when is_function(fun, 1) do
    {:noreply, fun.(state)}
  end

  def handle_cast(msg, state) do
    super(msg, state)
  end

  def code_change(_old, state, { m, f, a }) do
    {:ok, apply(m, f, [state|a])}
  end

  def terminate(_reason, _state) do
    # There is a race condition if the agent is
    # restarted too fast and it is registered.
    try do
      self |> Process.info(:registered_name) |> elem(1) |> Process.unregister
    rescue
      _ -> :ok
    end
    :ok
  end
end
