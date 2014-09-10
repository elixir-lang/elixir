defmodule Agent.Server do
  @moduledoc false

  use GenServer

  defstruct state: nil, initial_call: nil

  def init(fun) do
    _ = initial_call(fun)
    {:ok, run(fun, [])}
  end

  def handle_call({:get, fun}, _from, state) do
    {:reply, run(fun, [state]), state}
  end

  def handle_call({:get_and_update, fun}, _from, state) do
    case run(fun, [state]) do
      {reply, state} -> {:reply, reply, state}
      other          -> {:stop, {:bad_return_value, other}, state}
    end
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

  def format_status(:normal, [_pdict, state]) do
    [{:data, [{'State', state}]}]
  end

  def format_status(:terminate, [pdict, state]) do
    %Agent.Server{state: state,
      initial_call: :proc_lib.translate_initial_call([dictionary: pdict])}
  end

  defp run({m, f, a}, extra) do
    try do
      apply(m, f, extra ++ a)
    catch
      value ->
        value
    end
  end

  defp run(fun, extra) do
    try do
      apply(fun, extra)
    catch
      value ->
        value
    end
  end

  defp initial_call(mfa) do
    Process.put(:"$initial_call", get_initial_call(mfa))
  end

  defp get_initial_call(fun) when is_function(fun, 0) do
    {:module, module} = :erlang.fun_info(fun, :module)
    {:name, name} = :erlang.fun_info(fun, :name)
    {module, name, 0}
  end

  defp get_initial_call({mod, fun, args}) do
    {mod, fun, length(args)}
  end

end
