Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.SupervisedTest do
  use ExUnit.Case, async: true

  defmodule MyAgent do
    use Agent

    def start_link(:error) do
      {:error, "some error"}
    end

    def start_link(:exception) do
      raise "some exception"
    end

    def start_link(arg) do
      Agent.start_link(fn -> arg end, name: __MODULE__)
    end
  end

  test "returns error if the supervised process returns an error tuple" do
    {:error, error} = start_supervised({MyAgent, :error})
    assert {"some error", _info} = error

    message =
      "failed to start child with the spec {ExUnit.SupervisedTest.MyAgent, :error}.\n" <>
        "Reason: \"some error\""

    assert_raise RuntimeError, message, fn ->
      start_supervised!({MyAgent, :error})
    end
  end

  test "returns error if the supervised process raises an exception" do
    {:error, {{:EXIT, {exception, _}}, _}} = start_supervised({MyAgent, :exception})
    assert exception == %RuntimeError{message: "some exception"}

    message =
      "failed to start child with the spec {ExUnit.SupervisedTest.MyAgent, :exception}.\n" <>
        "Reason: an exception was raised:\n" <> "    ** (RuntimeError) some exception"

    exception =
      assert_raise RuntimeError, fn ->
        start_supervised!({MyAgent, :exception})
      end

    assert String.starts_with?(Exception.message(exception), message)
  end

  test "starts a supervised process that terminates before on_exit" do
    {:ok, pid} = start_supervised(MyAgent)
    assert Process.alive?(pid)
    on_exit(fn -> refute Process.alive?(pid) end)
  end

  test "starts a supervised process that is permanent" do
    {:ok, _} = start_supervised({MyAgent, 0})
    Agent.update(MyAgent, &(&1 + 1))
    assert Agent.get(MyAgent, & &1) == 1
    Agent.stop(MyAgent)
    wait_until_registered(MyAgent)
    assert Agent.get(MyAgent, & &1) == 0
  end

  test "starts a supervised process that is temporary" do
    {:ok, _} = start_supervised({MyAgent, 0}, restart: :temporary)
    Agent.update(MyAgent, &(&1 + 1))
    assert Agent.get(MyAgent, & &1) == 1
    Agent.stop(MyAgent)
    refute Process.whereis(MyAgent)
  end

  test "starts a supervised process with ID checks" do
    {:ok, pid} = start_supervised({MyAgent, 0})

    assert {:error, {:duplicate_child_name, ExUnit.SupervisedTest.MyAgent}} =
             start_supervised({MyAgent, 0})

    assert {:error, {{:already_started, ^pid}, _}} = start_supervised({MyAgent, 0}, id: :another)

    assert_raise RuntimeError, ~r"Reason: bad child specification", fn ->
      start_supervised!(%{id: 1, start: :oops})
    end

    assert_raise RuntimeError, ~r"Reason: already started", fn ->
      start_supervised!({MyAgent, 0}, id: :another)
    end
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
      message = "start_supervised/2 can only be invoked from the test process"

      assert_raise ArgumentError, message, fn ->
        start_supervised(MyAgent)
      end

      message = "stop_supervised/1 can only be invoked from the test process"

      assert_raise ArgumentError, message, fn ->
        stop_supervised(MyAgent)
      end
    end)
    |> Task.await()
  end

  defp wait_until_registered(name) do
    unless Process.whereis(name) do
      wait_until_registered(name)
    end
  end
end
