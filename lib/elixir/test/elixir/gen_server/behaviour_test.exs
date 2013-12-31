Code.require_file "../test_helper.exs", __DIR__

defmodule GenServer.BehaviourTest do
  use ExUnit.Case

  setup_all do
    :error_logger.tty(false)
    :ok
  end

  teardown_all do
    :error_logger.tty(true)
    :ok
  end

  defmodule Sample do
    use GenServer.Behaviour

    # Callbacks

    def handle_call(:pop, _from, [h|t]) do
      { :reply, h, t }
    end

    def handle_call(:terminate, _from, config) do
      { :stop, :normal, :ok, config }
    end

    def handle_call(request, from, config) do
      super(request, from, config)
    end

    def handle_cast({ :push, item }, config) do
      { :noreply, [item|config] }
    end

    def handle_cast(request, config) do
      super(request, config)
    end
  end

  test "using defines callbacks" do
    assert { :ok, pid } = :gen_server.start_link(Sample, [:hello], [])
    assert :gen_server.call(pid, :pop) == :hello
    assert :gen_server.cast(pid, { :push, :world }) == :ok
    assert :gen_server.call(pid, :pop) == :world
  end

  test "call stops server on unknown requests" do
    Process.flag(:trap_exit, true)
    assert { :ok, pid } = :gen_server.start_link(Sample, [:hello], [])

    catch_exit(:gen_server.call(pid, :unknown_request))
    assert_receive { :EXIT, ^pid, {:bad_call, :unknown_request} }
  after
    Process.flag(:trap_exit, false)
  end

  test "cast stops server on unknown requests" do
    Process.flag(:trap_exit, true)
    assert { :ok, pid } = :gen_server.start_link(Sample, [:hello], [])

    :gen_server.cast(pid, :unknown_request)
    assert_receive { :EXIT, ^pid, {:bad_cast, :unknown_request} }
  after
    Process.flag(:trap_exit, false)
  end
end
