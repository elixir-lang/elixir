# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule IEx.Broker do
  @moduledoc false
  @name __MODULE__

  @type take_ref :: {takeover_ref :: reference(), server_ref :: reference()}
  @type shell :: pid | nil

  use GenServer

  ## Shell API

  @doc """
  Finds the evaluator and server running inside `:user_drv`, on this node exclusively.
  """
  @spec evaluator(shell()) :: {evaluator :: pid, server :: pid} | nil
  def evaluator(pid \\ :shell.whereis()) do
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
        case :erpc.call(node, :shell, :whereis, []) do
          pid when is_pid(pid) -> pid
          :undefined -> nil
        end
      catch
        _, _ -> nil
      end
    end)
  end
end
