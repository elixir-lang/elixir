defmodule Logger.Counter do
  @moduledoc false

  @table __MODULE__
  @pos 1

  # TODO Remove this once we support Erlang/OTP 22+ exclusively.
  @compile {:no_warn_undefined, [:counters]}

  @type type() :: :ets | :counters
  @type t() :: {type(), :ets.tid() | :counters.counters_ref()}

  @spec new() :: t()
  def new do
    if Code.ensure_loaded?(:counters) do
      {:counters, :counters.new(1, [:atomics])}
    else
      table = :ets.new(@table, [:public])
      :ets.insert(table, [{@pos, 0}])
      {:ets, table}
    end
  end

  @spec delete(t()) :: :ok
  def delete({:ets, counter}), do: :ets.delete(counter)
  def delete({:counters, _}), do: :ok

  @spec read(t()) :: integer()
  def read({:ets, counter}), do: :ets.lookup_element(counter, @pos, 2)
  def read({:counters, counter}), do: :counters.get(counter, @pos)

  @spec add(t(), integer()) :: :ok
  def add({:ets, counter}, value), do: :ets.update_counter(counter, @pos, {2, value})
  def add({:counters, counter}, value), do: :counters.add(counter, @pos, value)

  @spec bump(t()) :: integer()
  def bump({:ets, counter}),
    do: :ets.update_counter(counter, @pos, {2, 1})

  def bump({:counters, counter}) do
    :counters.add(counter, @pos, 1)
    :counters.get(counter, @pos)
  end
end
