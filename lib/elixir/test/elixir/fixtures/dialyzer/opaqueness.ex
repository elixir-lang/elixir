defmodule Dialyzer.Opaqueness do
  @spec bar(MapSet.t()) :: term()
  def bar(set) do
    set
  end

  def foo() do
    # inlining of literals should not violate opaqueness check
    bar(MapSet.new([1, 2, 3]))
  end

  # Task.Supervisor returns a Task.t() containing an opaque Task.ref()
  @spec run_task() :: Task.t()
  def run_task do
    Task.Supervisor.async(SupervisorName, fn -> :ok end)
  end
end
