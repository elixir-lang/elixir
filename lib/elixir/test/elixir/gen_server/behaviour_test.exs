Code.require_file "../../test_helper.exs", __FILE__

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

    def handle_call(_request, _from, _config) do
      super
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
    assert :gen_server.call(pid, :pop, System.services_timeout) == :hello
    assert :gen_server.cast(pid, { :push, :world }) == :ok
    assert :gen_server.call(pid, :pop, System.services_timeout) == :world
  end
end
