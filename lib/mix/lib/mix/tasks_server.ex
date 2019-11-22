defmodule Mix.TasksServer do
  @moduledoc false
  @name __MODULE__
  @timeout 30000

  use Agent

  def start_link(_opts) do
    Agent.start_link(fn -> %{} end, name: @name)
  end

  def clear() do
    update(fn _ -> %{} end)
  end

  def run(tuple) do
    get_and_update(fn set ->
      {not Map.has_key?(set, tuple), Map.put(set, tuple, true)}
    end)
  end

  def put(tuple) do
    update(&Map.put(&1, tuple, true))
  end

  def delete_many(many) do
    update(&Map.drop(&1, many))
  end

  defp get_and_update(fun) do
    Agent.get_and_update(@name, fun, @timeout)
  end

  defp update(fun) do
    Agent.update(@name, fun, @timeout)
  end
end
