defmodule Mix.TasksServer do
  @moduledoc false

  def start_link() do
    Agent.start_link(fn -> HashSet.new end, name: __MODULE__)
  end

  def clear() do
    update fn _ -> HashSet.new end
  end

  def run(tuple) do
    get_and_update fn set ->
      {not(tuple in set), Set.put(set, tuple)}
    end
  end

  def put(tuple) do
    update &Set.put(&1, tuple)
  end

  def delete_many(many) do
    update &Enum.reduce(many, &1, fn x, acc -> Set.delete(acc, x) end)
  end

  defp get_and_update(fun) do
    Agent.get_and_update(__MODULE__, fun, 30_000)
  end

  defp update(fun) do
    Agent.update(__MODULE__, fun, 30_000)
  end
end
