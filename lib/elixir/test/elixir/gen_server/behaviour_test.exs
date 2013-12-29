Code.require_file "../test_helper.exs", __DIR__

defmodule GenServer.BehaviourTest do
  use ExUnit.Case, async: true

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

  test :using do
    assert { :ok, pid } = :gen_server.start_link(Sample, [:hello], [])
    assert :gen_server.call(pid, :pop) == :hello
    assert :gen_server.cast(pid, { :push, :world }) == :ok
    assert :gen_server.call(pid, :pop) == :world
  end

  test "call stops server on unknown requests" do
    assert { :ok, pid } = :gen_server.start_link(Sample, [:hello], [])
    Process.unlink(pid)
    assert {{:bad_call, :unknown_request}, _} = catch_exit(:gen_server.call(pid, :unknown_request))
  end

  test "cast stops server on unknown requests" do
    assert { :ok, pid } = :gen_server.start_link(Sample, [:hello], [])
    Process.unlink(pid)
    # Won't notice the server is stopped till we next send it a (valid) message
    assert :gen_server.cast(pid, :unknown_request) == :ok
    assert {{:bad_call, :unknown_request}, _} = catch_exit(:gen_server.call(pid, :pop))
  end
end
