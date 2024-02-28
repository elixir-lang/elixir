defmodule IEx.Broker do
  @moduledoc false
  @name __MODULE__

  @type take_ref :: {takeover_ref :: reference(), server_ref :: reference()}
  @type shell :: pid | nil

  use GenServer

  ## Shell API

  @doc """
  Finds the IEx server.
  """
  @spec shell :: shell()
  # TODO: Use shell:whereis() from Erlang/OTP 26+.
  def shell() do
    if user = Process.whereis(:user) do
      if user_drv = get_from_dict(user, :user_drv) do
        if group = get_from_dict(user_drv, :current_group) do
          get_from_dict(group, :shell)
        end
      end
    end
  end

  defp get_from_dict(pid, key) do
    with {:dictionary, dictionary} <- Process.info(pid, :dictionary),
         {^key, value} <- List.keyfind(dictionary, key, 0) do
      value
    else
      _ -> nil
    end
  end

  @doc """
  Finds the evaluator and server running inside `:user_drv`, on this node exclusively.
  """
  @spec evaluator(shell()) :: {evaluator :: pid, server :: pid} | nil
  def evaluator(pid \\ shell()) do
    if pid do
      {:dictionary, dictionary} = Process.info(pid, :dictionary)
      {dictionary[:evaluator], pid}
    end
  end

  ## Broker API

  def start_link(_) do
    GenServer.start_link(@name, :ok, name: @name)
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
  @spec respond(pid, take_ref, integer(), boolean()) ::
          :ok | {:error, :refused | :already_accepted}
  def respond(broker_pid, take_ref, counter, true) do
    GenServer.call(broker_pid, {:accept, take_ref, Process.group_leader(), counter})
  end

  def respond(broker_pid, take_ref, _counter, false) do
    GenServer.call(broker_pid, {:refuse, take_ref})
  end

  @doc """
  Client requests a takeover.
  """
  @spec take_over(binary, iodata, keyword) ::
          {:ok, server :: pid, group_leader :: pid, counter :: integer}
          | {:error, :no_iex | :refused | atom()}
  def take_over(location, whereami, opts) do
    case GenServer.whereis(@name) do
      nil -> {:error, :no_iex}
      _pid -> GenServer.call(@name, {:take_over, location, whereami, opts}, :infinity)
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
  def handle_call({:take_over, location, whereami, opts}, {_, ref} = from, state) do
    case servers(state) do
      [] ->
        {:reply, {:error, :no_iex}, state}

      servers ->
        server_refs =
          for {server_ref, server_pid} <- servers do
            send(server_pid, {:take_over, self(), {ref, server_ref}, location, whereami, opts})
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

  def handle_call({:accept, {ref, _server_ref}, group_leader, counter}, {server, _}, state) do
    case pop_in(state.takeovers[ref]) do
      {nil, state} ->
        {:reply, {:error, :already_accepted}, state}

      {{from, _}, state} ->
        GenServer.reply(from, {:ok, server, group_leader, counter})
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
      try do
        :erpc.call(node, IEx.Broker, :shell, [])
      catch
        _, _ -> nil
      end
    end)
  end
end
