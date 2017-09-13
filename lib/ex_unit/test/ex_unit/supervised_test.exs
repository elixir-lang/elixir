Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.SupervisedTest do
  use ExUnit.Case, async: true

  defmodule MyAgent do
    use Agent

    def start_link(:error) do
      {:error, "Because of reasons"}
    end
    def start_link(:exception) do
      raise "Because of exceptions"
    end
    def start_link(arg) do
      Agent.start_link(fn -> arg end, name: __MODULE__)
    end
  end

  test "returns error if the supervised process returns error tuple" do
    {:error, error} = start_supervised({MyAgent, :error})
    assert {"Because of reasons", _info} = error
    assert_raise RuntimeError, """
    Failed to start child with the spec {ExUnit.SupervisedTest.MyAgent, :error}.
    Reason: "Because of reasons"
    """, fn ->
      start_supervised!({MyAgent, :error})
    end
  end

  test "returns error if the supervised process raises exception" do
    {:error, {{:EXIT, {exception, _}}, _}} = start_supervised({MyAgent, :exception})
    assert exception == %RuntimeError{message: "Because of exceptions"}
    pattern = """
    Failed to start child with the spec {ExUnit.SupervisedTest.MyAgent, :exception}.
    Reason: an exception was raised:
        \\*\\* \\(RuntimeError\\) Because of exceptions
    """
    assert_raise RuntimeError, Regex.compile!(pattern), fn ->
      start_supervised!({MyAgent, :exception})
    end
  end

  test "starts a supervised process that terminates before on_exit" do
    {:ok, pid} = start_supervised(MyAgent)
    assert Process.alive?(pid)
    on_exit fn -> refute Process.alive?(pid) end
  end

  test "starts a supervised process that is permanent" do
    {:ok, _} = start_supervised({MyAgent, 0})
    Agent.update(MyAgent, & &1 + 1)
    assert Agent.get(MyAgent, & &1) == 1
    Agent.stop(MyAgent)
    wait_until_registered(MyAgent)
    assert Agent.get(MyAgent, & &1) == 0
  end

  test "starts a supervised process that is temporary" do
    {:ok, _} = start_supervised({MyAgent, 0}, restart: :temporary)
    Agent.update(MyAgent, & &1 + 1)
    assert Agent.get(MyAgent, & &1) == 1
    Agent.stop(MyAgent)
    refute Process.whereis(MyAgent)
  end

  test "starts a supervised process with id checks" do
    {:ok, pid} = start_supervised({MyAgent, 0})
    assert {:error, {:already_started, ^pid}} =
           start_supervised({MyAgent, 0})
    assert {:error, {{:already_started, ^pid}, _}} =
           start_supervised({MyAgent, 0}, id: :another)
  end

  test "stops a supervised process" do
    {:ok, pid} = start_supervised({MyAgent, 0})
    assert stop_supervised(MyAgent) == :ok
    refute Process.alive?(pid)
  end

  test "does not stop unknown processes" do
    assert stop_supervised(:unknown) == {:error, :not_found}
    {:ok, _} = start_supervised({MyAgent, 0})
    assert stop_supervised(:unknown) == {:error, :not_found}
  end

  test "raises if starting or stopping outside of test process" do
    Task.async(fn ->
      assert_raise ArgumentError, "start_supervised/2 can only be invoked from the test process", fn ->
        start_supervised(MyAgent)
      end

      assert_raise ArgumentError, "stop_supervised/1 can only be invoked from the test process", fn ->
        stop_supervised(MyAgent)
      end
    end) |> Task.await()
  end

  defp wait_until_registered(name) do
    unless Process.whereis(name) do
      wait_until_registered(name)
    end
  end
end
