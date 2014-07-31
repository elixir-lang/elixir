defmodule Logger.TranslatorTest do
  use Logger.Case

  defmodule MyGenServer do
    use GenServer

    def handle_call(:error, _, _) do
      raise "oops"
    end
  end

  defmodule MyGenEvent do
    use GenEvent

    def handle_call(:error, _) do
      raise "oops"
    end
  end

  test "translates GenServer crashes" do
    {:ok, pid} = GenServer.start(MyGenServer, :ok)

    assert capture_log(:info, fn ->
      catch_exit(GenServer.call(pid, :error))
    end) =~ """
    [error] GenServer #{inspect pid} terminating
    ** (exit) an exception was raised:
        ** (RuntimeError) oops
    """
  end

  test "translates GenServer crashes on debug" do
    {:ok, pid} = GenServer.start(MyGenServer, :ok)

    assert capture_log(:debug, fn ->
      catch_exit(GenServer.call(pid, :error))
    end) =~ """
    [error] GenServer #{inspect pid} terminating
    Last message: :error
    State: :ok
    ** (exit) an exception was raised:
        ** (RuntimeError) oops
    """
  end

  test "translates GenEvent crashes" do
    {:ok, pid} = GenEvent.start()
    :ok = GenEvent.add_handler(pid, MyGenEvent, :ok)

    assert capture_log(:info, fn ->
      GenEvent.call(pid, MyGenEvent, :error)
    end) =~ """
    [error] GenEvent handler Logger.TranslatorTest.MyGenEvent installed in #{inspect pid} terminating
    ** (exit) an exception was raised:
        ** (RuntimeError) oops
    """
  end

  test "translates GenEvent crashes on debug" do
    {:ok, pid} = GenEvent.start()
    :ok = GenEvent.add_handler(pid, MyGenEvent, :ok)

    assert capture_log(:debug, fn ->
      GenEvent.call(pid, MyGenEvent, :error)
    end) =~ """
    [error] GenEvent handler Logger.TranslatorTest.MyGenEvent installed in #{inspect pid} terminating
    Last message: :error
    State: :ok
    ** (exit) an exception was raised:
        ** (RuntimeError) oops
    """
  end

  test "translates Task crashes" do
    {:ok, pid} = Task.start_link(__MODULE__, :task, [self()])

    assert capture_log(fn ->
      ref = Process.monitor(pid)
      send(pid, :go)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ """
    [error] Task #{inspect pid} started from #{inspect self} terminating
    Function: &Logger.TranslatorTest.task/1
        Args: [#{inspect self}]
    ** (exit) an exception was raised:
        ** (RuntimeError) oops
    """
  end

  test "translates application stop" do
    :ok = Application.start(:eex)

    assert capture_log(fn ->
      Application.stop(:eex)
    end) =~ msg("[info]  Application eex exited with reason :stopped")
  end

  def task(parent) do
    Process.unlink(parent)
    receive do: (:go -> raise "oops")
  end
end
