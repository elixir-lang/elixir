Code.require_file "../../test_helper.exs", __FILE__

defmodule GenServer.Sample do
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

defmodule GenServer.BehaviourTest do
  use ExUnit.Case, async: true

  test :using do
    assert { :ok, pid } = :gen_server.start_link(GenServer.Sample, [:hello], [])
    assert :gen_server.call(pid, :pop) == :hello
    assert :gen_server.cast(pid, { :push, :world }) == :ok
    assert :gen_server.call(pid, :pop) == :world
  end
end
