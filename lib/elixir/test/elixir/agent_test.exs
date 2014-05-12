Code.require_file "test_helper.exs", __DIR__

defmodule AgentTest do
  use ExUnit.Case, async: true

  test "start_link/2 workflow with unregistered name" do
    {:ok, pid} = Agent.start_link(fn -> %{} end)

    {:links, links} = Process.info(self, :links)
    assert pid in links

    assert Agent.update(pid, &Map.put(&1, :hello, :world)) == :ok
    assert Agent.get(pid, &Map.get(&1, :hello), 3000) == :world
    assert Agent.get_and_update(pid, &Map.pop(&1, :hello), 3000) == :world
    assert Agent.get(pid, &(&1)) == %{}
    assert Agent.stop(pid) == :ok
    refute Process.alive?(pid)
  end

  test "start/2 workflow with registered name" do
    {:ok, pid} = Agent.start(fn -> %{} end, name: :agent)
    assert Process.info(pid, :registered_name) == {:registered_name, :agent}
    assert Agent.cast(:agent, &Map.put(&1, :hello, :world)) == :ok
    assert Agent.get(:agent, &Map.get(&1, :hello)) == :world
    assert Agent.get_and_update(:agent, &Map.pop(&1, :hello)) == :world
    assert Agent.get(:agent, &(&1)) == %{}
    assert Agent.stop(:agent) == :ok
    assert Process.info(pid, :registered_name) == nil
  end

  test ":sys.change_code/4 with mfa" do
    { :ok, pid } = Agent.start_link(fn -> %{} end)
    :ok = :sys.suspend(pid)
    mfa = { Map, :put, [:hello, :world] }
    assert :sys.change_code(pid, __MODULE__, "vsn", mfa) == :ok
    :ok = :sys.resume(pid)
    assert Agent.get(pid, &Map.get(&1, :hello)) == :world
    assert Agent.stop(pid) == :ok
  end

  test ":sys.change_code/4 with raising mfa" do
    { :ok, pid } = Agent.start_link(fn -> %{} end)
    :ok = :sys.suspend(pid)
    mfa = { :erlang, :error, [] }
    assert match?({ :error, _ }, :sys.change_code(pid, __MODULE__, "vsn", mfa))
    :ok = :sys.resume(pid)
    assert Agent.get(pid, &(&1)) == %{}
    assert Agent.stop(pid) == :ok
  end
end
