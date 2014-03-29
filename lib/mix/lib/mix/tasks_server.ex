defmodule Mix.TasksServer do
  @moduledoc false
  use GenServer.Behaviour

  def start_link() do
    :gen_server.start_link({ :local, __MODULE__ }, __MODULE__, :ok, [])
  end

  def call(arg) do
    :gen_server.call(__MODULE__, arg, 30_000)
  end

  def cast(arg) do
    :gen_server.cast(__MODULE__, arg)
  end

  ## Callbacks

  def init(:ok) do
    { :ok, HashSet.new }
  end

  def handle_call(:clear_tasks, _from, set) do
    { :reply, set, HashSet.new }
  end

  def handle_call({ :run_task, task, proj }, _from, set) do
    item = { task, proj }
    { :reply, not(item in set), Set.put(set, item) }
  end

  def handle_call(request, from, config) do
    super(request, from, config)
  end

  def handle_cast({ :put_task, task, proj }, set) do
    { :noreply, Set.put(set, { task, proj }) }
  end

  def handle_cast({ :delete_task, task, proj }, set) do
    { :noreply, Set.delete(set, { task, proj }) }
  end

  def handle_cast(request, config) do
    super(request, config)
  end
end
