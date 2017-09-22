defmodule ExUnit.SeedManager do
  use Agent
  
  @type seed :: :rand.state

  @spec start_link(keyword) :: {:ok, pid}
  def start_link(_opts) do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  @spec stop() :: :ok
  def stop do
    Agent.stop(__MODULE__)
  end

  @spec get() :: seed
  def get do
    test_pid = self()
    Agent.get(__MODULE__, &Map.fetch!(&1, test_pid))
  end

  @spec put(seed) :: :ok
  def put(seed) do
    test_pid = self()
    Agent.update(__MODULE__, &Map.put(&1, test_pid, seed))
  end

  @spec delete() :: :ok
  def delete() do
    test_pid = self()
    Agent.cast(__MODULE__, &Map.delete(&1, test_pid))
  end
end
