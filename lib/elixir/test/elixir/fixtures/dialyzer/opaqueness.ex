defmodule Dialyzer.Opaqueness do
  @spec bar(MapSet.t()) :: term()
  def bar(set) do
    set
  end

  def inlined do
    # inlining of literals should not violate opaqueness check
    bar(MapSet.new([1, 2, 3]))
  end

  @my_set MapSet.new([1, 2, 3])
  def module_attr do
    bar(@my_set)
  end

  # Task.Supervisor returns a Task.t() containing an opaque Task.ref()
  @spec run_task() :: Task.t()
  def run_task do
    Task.Supervisor.async(SupervisorName, fn -> :ok end)
  end
end
