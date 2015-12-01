defmodule Mix.TasksServer do
  @moduledoc false

  def start_link() do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  def clear() do
    update fn _ -> %{} end
  end

  def run(tuple) do
    get_and_update fn set ->
      {not Map.has_key?(set, tuple), Map.put(set, tuple, true)}
    end
  end

  def put(tuple) do
    update &Map.put(&1, tuple, true)
  end

  def delete_many(many) do
    update &Map.drop(&1, many)
  end

  defp get_and_update(fun) do
    Agent.get_and_update(__MODULE__, fun, 30_000)
  end

  defp update(fun) do
    Agent.update(__MODULE__, fun, 30_000)
  end
end
