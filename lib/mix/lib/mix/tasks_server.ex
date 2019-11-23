defmodule Mix.TasksServer do
  @moduledoc false
  @name __MODULE__
  @timeout 30000

  use Agent

  def start_link(_opts) do
    Agent.start_link(fn -> %{} end, name: @name)
  end

  def run(tuple) do
    Agent.get_and_update(
      @name,
      fn set -> {not Map.has_key?(set, tuple), Map.put(set, tuple, true)} end,
      @timeout
    )
  end

  def put(tuple) do
    Agent.update(@name, &Map.put(&1, tuple, true), @timeout)
  end

  def delete_many(many) do
    Agent.update(@name, &Map.drop(&1, many), @timeout)
  end

  def clear() do
    Agent.update(@name, fn _ -> %{} end, @timeout)
  end
end
