defmodule Mix.State do
  @moduledoc false
  @name __MODULE__
  @timeout :infinity

  use Agent

  def start_link(_opts) do
    Agent.start_link(__MODULE__, :init, [], name: @name)
  end

  def init() do
    %{
      shell: Mix.Shell.IO,
      env: String.to_atom(System.get_env("MIX_ENV") || "dev"),
      target: String.to_atom(System.get_env("MIX_TARGET") || "host"),
      scm: [Mix.SCM.Git, Mix.SCM.Path],
      cache: :ets.new(@name, [:public, :set, :named_table, read_concurrency: true])
    }
  end

  def fetch(key) do
    Agent.get(@name, Map, :fetch, [key], @timeout)
  end

  def get(key, default \\ nil) do
    Agent.get(@name, Map, :get, [key, default], @timeout)
  end

  def put(key, value) do
    Agent.update(@name, Map, :put, [key, value], @timeout)
  end

  def prepend_scm(value) do
    Agent.update(
      @name,
      fn state -> update_in(state.scm, &[value | List.delete(&1, value)]) end,
      @timeout
    )
  end

  def append_scm(value) do
    Agent.update(
      @name,
      fn state -> update_in(state.scm, &(List.delete(&1, value) ++ [value])) end,
      @timeout
    )
  end

  def read_cache(key) do
    case :ets.lookup(@name, key) do
      [{^key, value}] -> value
      [] -> nil
    end
  end

  def write_cache(key, value) do
    :ets.insert(@name, {key, value})
    value
  end

  def delete_cache(key) do
    :ets.delete(@name, key)
  end

  def clear_cache do
    :ets.delete_all_objects(@name)
  end
end
