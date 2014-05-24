defmodule Mix.TasksServer do
  @moduledoc false

  def start_link() do
    Agent.start_link(fn -> HashSet.new end, name: __MODULE__)
  end

  def clear_tasks() do
    get_and_update fn set ->
      { set, HashSet.new }
    end
  end

  def run_task(task, proj) do
    run_item = { task, proj }
    get_and_update fn set ->
      { not(run_item in set), Set.put(set, run_item) }
    end
  end

  def put_task(task, proj) do
    update &Set.put(&1, { task, proj })
  end

  def delete_task(task, proj) do
    update &Set.delete(&1, { task, proj })
  end

  defp get_and_update(fun) do
    Agent.get_and_update(__MODULE__, fun, 30_000)
  end

  defp update(fun) do
    Agent.update(__MODULE__, fun, 30_000)
  end
end
