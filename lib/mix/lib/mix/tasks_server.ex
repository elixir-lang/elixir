defmodule Mix.TasksServer do
  @moduledoc false
  @name __MODULE__

  use Agent

  def start_link(_opts) do
    Agent.start_link(
      fn -> :ets.new(@name, [:public, :set, :named_table]) end,
      name: @name
    )
  end

  def run(tuple) do
    :ets.insert_new(@name, {tuple})
  end

  def put(tuple) do
    :ets.insert(@name, {tuple})
    :ok
  end

  def get(tuple) do
    :ets.member(@name, tuple)
  end

  def delete_many(many) do
    Enum.each(many, &:ets.delete(@name, &1))
  end

  def clear() do
    :ets.delete_all_objects(@name)
    :ok
  end
end
