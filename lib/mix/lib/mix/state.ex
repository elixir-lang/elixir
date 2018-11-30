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
      scm: [Mix.SCM.Git, Mix.SCM.Path]
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

  def prepend(key, value) do
    Agent.update(@name, Map, :update, [key, [value], &[value | List.delete(&1, value)]])
  end

  def append(key, value) do
    Agent.update(@name, Map, :update, [key, [value], &(List.delete(&1, value) ++ [value])])
  end
end
