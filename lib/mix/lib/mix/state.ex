defmodule Mix.State do
  @moduledoc false
  @name __MODULE__

  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: @name)
  end

  @impl true
  def init(:ok) do
    table = :ets.new(@name, [:public, :set, :named_table, read_concurrency: true])

    :ets.insert(table,
      shell: Mix.Shell.IO,
      env: from_env("MIX_ENV", :dev),
      target: from_env("MIX_TARGET", :host),
      scm: [Mix.SCM.Git, Mix.SCM.Path]
    )

    {:ok, table}
  end

  defp from_env(varname, default) do
    case System.get_env(varname) do
      nil -> default
      "" -> default
      value -> String.to_atom(value)
    end
  end

  def fetch(key) do
    case :ets.lookup(@name, key) do
      [{^key, value}] -> {:ok, value}
      [] -> :error
    end
  end

  def get(key, default \\ nil) do
    case :ets.lookup(@name, key) do
      [{^key, value}] -> value
      [] -> default
    end
  end

  def put(key, value) do
    :ets.insert(@name, {key, value})
  end

  def update(key, fun) do
    :ets.insert(@name, {key, fun.(:ets.lookup_element(@name, key, 2))})
  end

  def read_cache(key) do
    :persistent_term.get({__MODULE__, key}, nil)
  end

  def write_cache(key, value) do
    :persistent_term.put({__MODULE__, key}, value)
    value
  end

  def delete_cache(key) do
    :persistent_term.erase({__MODULE__, key})
  end

  def clear_cache do
    for {{__MODULE__, _} = key, _value} <- :persistent_term.get() do
      :persistent_term.erase(key)
    end
  end
end
