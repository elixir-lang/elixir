defmodule IEx.Broker do
  @moduledoc false
  @name IEx.Broker

  @type take_ref :: {takeover_ref :: reference(), server_ref :: reference()}

  use GenServer

  ## Shell API

  @doc """
  Finds the IEx server running inside `:user_drv`, on this node exclusively.
  """
  @spec shell :: pid | nil
  def shell() do
    # Locate top group leader when using the "new shell".
    if user = Process.whereis(:user) do
      case :group.interfaces(user) do
        # Old or no shell
        [] ->
          nil

        # Get current group from user_drv
        [user_drv: user_drv] ->
          case :user_drv.interfaces(user_drv) do
            [] -> nil
            [current_group: group] -> :group.interfaces(group)[:shell]
          end
      end
    end
  end

  @doc """
  Finds the evaluator and server running inside `:user_drv`, on this node exclusively.
  """
  @spec evaluator :: {evaluator :: pid, server :: pid} | nil
  def evaluator() do
    if pid = shell() do
      {:dictionary, dictionary} = Process.info(pid, :dictionary)
      {dictionary[:evaluator], pid}
    end
  end

  ## Broker API

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: @name)
  end

  @doc """
  Registers an IEx server in the broker.

  All instances, except shell ones, are registered.
  """
  @spec register(pid) :: :ok
  def register(pid) do
    GenServer.call(@name, {:register, pid})
  end

  @doc """
  Client responds to a takeover request.

  The broker's PID is needed to support remote shells.
  """
  @spec respond(pid, take_ref, boolean()) :: :ok | {:error, :refused | :already_accepted}
  def respond(broker_pid, take_ref, true) do
    GenServer.call(broker_pid, {:accept, take_ref, Process.group_leader()})
  end

  def respond(broker_pid, take_ref, false) do
    GenServer.call(broker_pid, {:refuse, take_ref})
  end

  @doc """
  Client requests a takeover.
  """
  @spec take_over(binary, keyword) ::
          {:ok, server :: pid, group_leader :: pid} | {:error, :no_iex | :refused}
  def take_over(identifier, opts) do
    case GenServer.whereis(@name) do
      nil ->
        {:error, :no_iex}

      _pid ->
        GenServer.call(@name, {:take_over, identifier, opts}, :infinity)
    end
  end

  ## Callbacks

  @impl true
  def init(:ok) do
    state = %{
      servers: %{},
      takeovers: %{}
    }

    {:ok, state}
  end

  @impl true
  def handle_call({:take_over, identifier, opts}, {_, ref} = from, state) do
    case servers(state) do
      [] ->
        {:reply, {:error, :no_iex}, state}

      servers ->
        server_refs =
          for {server_ref, server_pid} <- servers do
            send(server_pid, {:take_over, self(), {ref, server_ref}, identifier, opts})
            server_ref
          end

        state = put_in(state.takeovers[ref], {from, server_refs})
        {:noreply, state}
    end
  end

  def handle_call({:register, pid}, _from, state) do
    ref = Process.monitor(pid)
    state = put_in(state.servers[ref], pid)
    {:reply, :ok, state}
  end

  def handle_call({:accept, {ref, _server_ref}, group_leader}, {server, _}, state) do
    case pop_in(state.takeovers[ref]) do
      {nil, state} ->
        {:reply, {:error, :already_accepted}, state}

      {{from, _}, state} ->
        GenServer.reply(from, {:ok, server, group_leader})
        {:reply, :ok, state}
    end
  end

  def handle_call({:refuse, {ref, server_ref}}, _from, state) do
    if takeover = state.takeovers[ref] do
      {:reply, {:error, :refused}, refuse(state, ref, takeover, server_ref)}
    else
      {:reply, {:error, :refused}, state}
    end
  end

  @impl true
  def handle_info({:DOWN, server_ref, _, _, _}, state) do
    {_pid, state} = pop_in(state.servers[server_ref])

    state =
      Enum.reduce(state.takeovers, state, fn {ref, takeover}, state ->
        refuse(state, ref, takeover, server_ref)
      end)

    {:noreply, state}
  end

  defp refuse(state, ref, {from, server_refs}, server_ref) do
    case List.delete(server_refs, server_ref) do
      [] ->
        {_, state} = pop_in(state.takeovers[ref])
        GenServer.reply(from, {:error, :refused})
        state

      server_refs ->
        put_in(state.takeovers[ref], {from, server_refs})
    end
  end

  defp servers(state) do
    if pid = local_or_remote_shell() do
      [{Process.monitor(pid), pid} | Enum.to_list(state.servers)]
    else
      Enum.to_list(state.servers)
    end
  end

  defp local_or_remote_shell() do
    Enum.find_value([node() | Node.list()], fn node ->
      server = :rpc.call(node, IEx.Broker, :shell, [])
      if is_pid(server), do: server
    end)
  end
end
