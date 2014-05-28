defmodule Mix.ProjectStack do
  # Keeps the project stack.
  @moduledoc false

  @timeout 30_000

  @typep file    :: binary
  @typep config  :: Keyword.t
  @typep project :: {module, config, file}

  @spec start_link :: {:ok, pid}
  def start_link() do
    Agent.start_link fn -> %{stack: [], post_config: [], cache: HashDict.new} end, name: __MODULE__
  end

  @spec push(module, config, file) :: :ok | {:error, file}
  def push(module, config, file) do
    get_and_update fn %{stack: stack} = state ->
      config  = Keyword.merge(config, state.post_config)
      project = %{name: module, config: config, file: file, recursing?: false, io_done: false, tasks: HashSet.new}

      cond do
        file = find_project_named(module, stack) ->
          {{:error, file}, state}
        true ->
          {:ok, %{state | post_config: [], stack: [project|state.stack]}}
      end
    end
  end

  @spec pop() :: project | nil
  def pop do
    get_and_update fn %{stack: stack} = state ->
      case stack do
        [h|t] -> {project_to_tuple(h), %{state | stack: t}}
        [] -> {nil, state}
      end
    end
  end

  @spec peek() :: project | nil
  def peek do
    get fn %{stack: stack} ->
      case stack do
        [h|_] -> project_to_tuple(h)
        [] -> nil
      end
    end
  end

  @spec post_config(config) :: :ok
  def post_config(config) do
    cast fn state ->
      %{state | post_config: Keyword.merge(state.post_config, config)}
    end
  end

  @spec output_app?() :: boolean
  def output_app? do
    get_and_update fn %{stack: stack} = state ->
      case stack do
        [h|t] ->
          output = not h.io_done and not umbrella?(stack) and in_umbrella?(stack)
          {output, %{state | stack: [%{h | io_done: true}|t]}}
        [] ->
          {false, state}
      end
    end
  end

  @spec clear_stack() :: :ok
  def clear_stack do
    cast fn state ->
      %{state | stack: [], post_config: []}
    end
  end

  @doc """
  Enables the recursion for the project at the top of the stack.
  Returns true if recursion was enabled or false if the project
  already had recursion enabled or there is no project in the stack.
  """
  @spec enable_recursion :: boolean
  def enable_recursion do
    get_and_update fn %{stack: stack} = state ->
      case stack do
        [h|t] ->
          {not h.recursing?, %{state | stack: [%{h | recursing?: true}|t]}}
        _ ->
          {false, state}
      end
    end
  end

  @doc """
  Disables the recursion for the project in the stack.
  Returns true if recursion was disabled or false if there
  is no project or recursion was not enabled.
  """
  @spec disable_recursion :: boolean
  def disable_recursion do
    get_and_update fn %{stack: stack} = state ->
      case stack do
        [h|t] ->
          {h.recursing?, %{state | stack: [%{h | recursing?: false}|t]}}
        _ ->
          {false, state}
      end
    end
  end

  @spec read_cache(term) :: term
  def read_cache(key) do
    get fn %{cache: cache} ->
      cache[key]
    end
  end

  @spec write_cache(term, term) :: :ok
  def write_cache(key, value) do
    cast fn state ->
      %{state | cache: Dict.put(state.cache, key, value)}
    end
  end

  @spec clear_cache :: :ok
  def clear_cache do
    cast fn state ->
      %{state | cache: HashDict.new}
    end
  end

  defp in_umbrella?(stack) do
    Enum.any?(stack, fn(%{config: conf}) ->
      conf[:apps_path] != nil
    end)
  end

  defp umbrella?(stack) do
    case stack do
      [%{name: name, config: config}|_] when name != nil -> config[:apps_path] != nil
      _ -> false
    end
  end

  defp find_project_named(name, stack) do
    name && Enum.find_value(stack, fn
      %{name: n, file: file} when n === name -> file
      %{} -> nil
    end)
  end

  defp project_to_tuple(%{name: name, config: config, file: file}) do
    {name, config, file}
  end

  defp get_and_update(fun) do
    Agent.get_and_update __MODULE__, fun, @timeout
  end

  defp get(fun) do
    Agent.get __MODULE__, fun, @timeout
  end

  defp cast(fun) do
    Agent.cast __MODULE__, fun
  end

end
