defmodule Mix.State do
  @moduledoc false
  @name __MODULE__

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
      cache: %{}
    }
  end

  def fetch(key) do
    Agent.get(@name, Map, :fetch, [key])
  end

  def get(key, default \\ nil) do
    Agent.get(@name, Map, :get, [key, default])
  end

  def put(key, value) do
    Agent.update(@name, Map, :put, [key, value])
  end

  def prepend_scm(value) do
    Agent.update(@name, fn state -> update_in(state.scm, &[value | List.delete(&1, value)]) end)
  end

  def append_scm(value) do
    Agent.update(@name, fn state -> update_in(state.scm, &(List.delete(&1, value) ++ [value])) end)
  end

  def read_cache(key) do
    Agent.get(@name, & &1.cache[key])
  end

  def write_cache(key, value) do
    Agent.cast(@name, &put_in(&1.cache[key], value))
    value
  end

  def delete_cache(key) do
    Agent.cast(@name, &(pop_in(&1.cache[key]) |> elem(1)))
  end

  def clear_cache do
    Agent.cast(@name, &put_in(&1.cache, %{}))
  end
end
