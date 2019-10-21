Code.require_file("test_helper.exs", __DIR__)

defmodule AgentTest do
  use ExUnit.Case, async: true

  doctest Agent

  def identity(state) do
    state
  end

  test "can be supervised directly" do
    assert {:ok, _} = Supervisor.start_link([{Agent, fn -> :ok end}], strategy: :one_for_one)
  end

  test "generates child_spec/1" do
    defmodule MyAgent do
      use Agent
    end

    assert MyAgent.child_spec([:hello]) == %{
             id: MyAgent,
             start: {MyAgent, :start_link, [[:hello]]}
           }

    defmodule CustomAgent do
      use Agent, id: :id, restart: :temporary, shutdown: :infinity, start: {:foo, :bar, []}
    end

    assert CustomAgent.child_spec([:hello]) == %{
             id: :id,
             restart: :temporary,
             shutdown: :infinity,
             start: {:foo, :bar, []}
           }
  end

  test "start_link/2 workflow with unregistered name and anonymous functions" do
    {:ok, pid} = Agent.start_link(&Map.new/0)

    {:links, links} = Process.info(self(), :links)
    assert pid in links

    assert :proc_lib.translate_initial_call(pid) == {Map, :new, 0}

    assert Agent.update(pid, &Map.put(&1, :hello, :world)) == :ok
    assert Agent.get(pid, &Map.get(&1, :hello), 3000) == :world
    assert Agent.get_and_update(pid, &Map.pop(&1, :hello), 3000) == :world
    assert Agent.get(pid, & &1) == %{}
    assert Agent.stop(pid) == :ok
    wait_until_dead(pid)
  end

  test "start_link/2 with spawn_opt" do
    {:ok, pid} = Agent.start_link(fn -> 0 end, spawn_opt: [priority: :high])
    assert Process.info(pid, :priority) == {:priority, :high}
  end

  test "start/2 workflow with registered name and module functions" do
    {:ok, pid} = Agent.start(Map, :new, [], name: :agent)
    assert Process.info(pid, :registered_name) == {:registered_name, :agent}
    assert :proc_lib.translate_initial_call(pid) == {Map, :new, 0}
    assert Agent.cast(:agent, Map, :put, [:hello, :world]) == :ok
    assert Agent.get(:agent, Map, :get, [:hello]) == :world
    assert Agent.get_and_update(:agent, Map, :pop, [:hello]) == :world
    assert Agent.get(:agent, AgentTest, :identity, []) == %{}
    assert Agent.stop(:agent) == :ok
    assert Process.info(pid, :registered_name) == nil
  end

  test ":sys.change_code/4 with mfa" do
    {:ok, pid} = Agent.start_link(fn -> %{} end)
    :ok = :sys.suspend(pid)
    mfa = {Map, :put, [:hello, :world]}
    assert :sys.change_code(pid, __MODULE__, "vsn", mfa) == :ok
    :ok = :sys.resume(pid)
    assert Agent.get(pid, &Map.get(&1, :hello)) == :world
    assert Agent.stop(pid) == :ok
  end

  test ":sys.change_code/4 with raising mfa" do
    {:ok, pid} = Agent.start_link(fn -> %{} end)
    :ok = :sys.suspend(pid)
    mfa = {:erlang, :error, []}
    assert match?({:error, _}, :sys.change_code(pid, __MODULE__, "vsn", mfa))
    :ok = :sys.resume(pid)
    assert Agent.get(pid, & &1) == %{}
    assert Agent.stop(pid) == :ok
  end

  defp wait_until_dead(pid) do
    if Process.alive?(pid) do
      wait_until_dead(pid)
    end
  end
end
