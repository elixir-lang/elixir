defmodule Mix.State do
  @moduledoc false
  @name __MODULE__
  @timeout :infinity

  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: @name)
  end

  def builtin_apps do
    GenServer.call(@name, :builtin_apps, @timeout)
  end

  ## ETS state storage (mutable, not cleared in tests)

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
    :ok
  end

  def update(key, fun) do
    :ets.insert(@name, {key, fun.(:ets.lookup_element(@name, key, 2))})
    :ok
  end

  ## Persistent term cache (persistent, cleared in tests)

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

  ## Callbacks

  @impl true
  def init(:ok) do
    table = :ets.new(@name, [:public, :set, :named_table, read_concurrency: true])

    :ets.insert(table,
      shell: Mix.Shell.IO,
      env: from_env("MIX_ENV", :dev),
      target: from_env("MIX_TARGET", :host),
      scm: [Mix.SCM.Git, Mix.SCM.Path]
    )

    state = %{
      builtin_apps: :code.get_path()
    }

    {:ok, state}
  end

  defp from_env(varname, default) do
    case System.get_env(varname) do
      nil -> default
      "" -> default
      value -> String.to_atom(value)
    end
  end

  @impl true
  def handle_call(:builtin_apps, _from, %{builtin_apps: builtin_apps} = state) do
    if is_map(builtin_apps) do
      {:reply, builtin_apps, state}
    else
      builtin_apps =
        for path <- builtin_apps,
            app = app_from_code_path(path),
            do: {app, path},
            into: %{}

      {:reply, builtin_apps, %{state | builtin_apps: builtin_apps}}
    end
  end

  # ../elixir/ebin -> elixir
  # ../ssl-9.6/ebin -> ssl
  defp app_from_code_path(path) do
    case path |> Enum.reverse() |> discard_ebin() |> collect_dir([]) do
      [] -> nil
      app -> List.to_atom(app)
    end
  end

  defp discard_ebin(~c"nibe/" ++ path), do: path
  defp discard_ebin(~c"nibe\\" ++ path), do: path
  defp discard_ebin(_), do: []

  defp collect_dir([?\\ | _], acc), do: acc
  defp collect_dir([?/ | _], acc), do: acc
  defp collect_dir([?- | path], _acc), do: collect_dir(path, [])
  defp collect_dir([head | path], acc), do: collect_dir(path, [head | acc])
  defp collect_dir([], acc), do: acc
end
