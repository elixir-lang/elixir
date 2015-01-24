defmodule Mix.State do
  @moduledoc false

  @table __MODULE__
  @agent __MODULE__

  def new(state) do
    tab = :ets.new(@table, [:named_table, :public])
    true = :ets.insert_new(@table, state)
    tab
  end

  def delete(@table = tab) do
    :ets.delete(tab)
  end

  def start_link(tab) do
    Agent.start_link(__MODULE__, :init, [tab], [name: @agent])
  end

  def init(@table = tab) do
    case :ets.info(tab, :protection) do
      :public ->
        tab
      :undefined ->
        raise "#{tab} does not exist"
      _ ->
        raise "#{tab} is not public"
    end
  end

  def fetch(key) do
    case :ets.lookup(@table, key) do
      [{_, value}] ->
        {:ok, value}
      [] ->
        :error
    end
  end

  def get(key, default \\ nil) do
    case fetch(key) do
      {:ok, value} -> value
      :error       -> default
    end
  end

  def put(key, value) do
    Agent.update(@agent, __MODULE__, :handle_put, [key, value])
  end

  def handle_put(tab, key, value) do
    true = :ets.insert(tab, {key, value})
    tab
  end

  def prepend(key, value) do
    Agent.update(@agent, __MODULE__, :handle_prepend, [key, value])
  end

  def handle_prepend(tab, key, value) do
    true = :ets.insert(tab, {key, [value | reject(key, value)]})
    tab
  end

  def append(key, value) do
    Agent.update(@agent, __MODULE__, :handle_append, [key, value])
  end

  def handle_append(tab, key, value) do
    true = :ets.insert(tab, {key, reject(key, value) ++ [value]})
    tab
  end

  defp reject(key, value) do
    {:ok, list} = fetch(key)
    Enum.reject(list, &(&1 === value))
  end
end
