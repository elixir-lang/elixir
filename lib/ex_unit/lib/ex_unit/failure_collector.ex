defmodule ExUnit.FailureCollector do
  @moduledoc false
  @name __MODULE__

  def start_link do
    Agent.start_link(fn -> %{} end, name: @name)
  end

  def add_failure(pid, failure) do
    Agent.update(@name, fn map ->
      Map.update(map, pid, [failure], &[failure|&1])
    end)
  end

  def get_failures(pid) do
    Agent.get_and_update(@name, fn map ->
      {failures, map} = Map.pop(map, pid)
      {Enum.reverse(failures || []), map}
    end)
  end
end
