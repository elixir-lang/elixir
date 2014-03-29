defmodule Mix.ProjectStack do
  # Keeps the project stack.
  @moduledoc false

  use GenServer.Behaviour
  @timeout 30_000

  @typep file    :: binary
  @typep config  :: Keyword.t
  @typep project :: { module, config, file }

  defrecord State, stack: [], post_config: [], cache: HashDict.new
  defrecord Project, name: nil, config: nil, file: nil,
                     recursing?: false, io_done: false, tasks: HashSet.new

  @spec start_link :: { :ok, pid }
  def start_link() do
    :gen_server.start_link({ :local, __MODULE__ }, __MODULE__, [], [])
  end

  @spec push(module, config, file) :: :ok | { :error, file }
  def push(module, config, file) do
    call { :push, module, config, file }
  end

  @spec pop() :: project | nil
  def pop do
    call :pop
  end

  @spec peek() :: project | nil
  def peek do
    call :peek
  end

  @spec post_config(config) :: :ok
  def post_config(config) do
    cast { :post_config, config }
  end

  @spec output_app?() :: boolean
  def output_app? do
    call :output_app?
  end

  @spec clear_stack() :: :ok
  def clear_stack do
    cast :clear_stack
  end

  @doc """
  Enables the recursion for the project at the top of the stack.
  Returns true if recursion was enabled or false if the project
  already had recursion enabled or there is no project in the stack.
  """
  @spec enable_recursion :: boolean
  def enable_recursion do
    call :enable_recursion
  end

  @doc """
  Disables the recursion for the project in the stack.
  Returns true if recursion was disable or false if there
  is no project or recursion was not enabled.
  """
  @spec disable_recursion :: boolean
  def disable_recursion do
    call :disable_recursion
  end

  @spec read_cache(term) :: term
  def read_cache(key) do
    call({ :read_cache, key })
  end

  @spec write_cache(term, term) :: :ok
  def write_cache(key, value) do
    cast({ :write_cache, key, value })
  end

  @spec clear_cache :: :ok
  def clear_cache do
    cast(:clear_cache)
  end

  defp call(arg) do
    :gen_server.call(__MODULE__, arg, @timeout)
  end

  defp cast(arg) do
    :gen_server.cast(__MODULE__, arg)
  end

  ## Callbacks

  def init([]) do
    { :ok, State[] }
  end

  def handle_call({ :push, name, config, file }, _from, State[stack: stack] = state) do
    config  = Keyword.merge(config, state.post_config)
    project = Project[name: name, config: config, file: file]

    cond do
      file = find_project_named(name, stack) ->
        { :reply, { :error, file }, state }
      true ->
        { :reply, :ok, state.post_config([]).update_stack(&[project|&1]) }
    end
  end

  def handle_call(:pop, _from, State[stack: stack] = state) do
    case stack do
      [h|t] -> { :reply, project_to_tuple(h), state.stack(t) }
      [] -> { :reply, nil, state }
    end
  end

  def handle_call(:peek, _from, State[stack: stack] = state) do
    case stack do
      [h|_] -> { :reply, project_to_tuple(h), state }
      [] -> { :reply, nil, state }
    end
  end

  def handle_call(:output_app?, _from, State[stack: stack] = state) do
    case stack do
      [Project[]=h|t] ->
        output = not h.io_done and not umbrella?(stack) and in_umbrella?(stack)
        { :reply, output, state.stack([h.io_done(true)|t]) }
      [] ->
        { :reply, false, state }
    end
  end

  def handle_call(:enable_recursion, _from, State[stack: stack] = state) do
    case stack do
      [Project[]=h|t] ->
        { :reply, not h.recursing?, state.stack([h.recursing?(true)|t]) }
      _ ->
        { :reply, false, state }
    end
  end

  def handle_call(:disable_recursion, _from, State[stack: stack] = state) do
    case stack do
      [Project[]=h|t] ->
        { :reply, h.recursing?, state.stack([h.recursing?(false)|t]) }
      _ ->
        { :reply, false, state }
    end
  end

  def handle_call({ :read_cache, key }, _from, State[cache: cache] = state) do
    { :reply, cache[key], state }
  end

  def handle_call(request, from, config) do
    super(request, from, config)
  end

  def handle_cast({ :post_config, value }, State[] = state) do
    { :noreply, state.update_post_config(&Keyword.merge(&1, value)) }
  end

  def handle_cast(:clear_stack, State[] = state) do
    { :noreply, state.stack([]).post_config([]) }
  end

  def handle_cast({ :write_cache, key, value }, State[] = state) do
    { :noreply, state.update_cache(&Dict.put(&1, key, value)) }
  end

  def handle_cast(:clear_cache, State[] = state) do
    { :noreply, state.cache(HashDict.new) }
  end

  def handle_cast(request, state) do
    super(request, state)
  end

  defp in_umbrella?(stack) do
    Enum.any?(stack, fn(Project[config: conf]) ->
      conf[:apps_path] != nil
    end)
  end

  defp umbrella?(stack) do
    case stack do
      [Project[name: name, config: config]|_] when name != nil -> config[:apps_path] != nil
      _ -> false
    end
  end

  defp find_project_named(name, stack) do
    name && Enum.find_value(stack, fn
      Project[name: n, file: file] when n === name -> file
      Project[] -> nil
    end)
  end

  defp project_to_tuple(Project[name: name, config: config, file: file]) do
    { name, config, file }
  end
end
