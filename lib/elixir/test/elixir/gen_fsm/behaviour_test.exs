Code.require_file "../test_helper.exs", __DIR__

defmodule GenFSM.BehaviourTest do
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
    use GenFSM.Behaviour

    def init(args) do
      { :ok, :sample, args }
    end
  end

  test "sync event stops server on unknown requests" do
    Process.flag(:trap_exit, true)
    assert { :ok, pid } = :gen_fsm.start_link(Sample, [:hello], [])

    catch_exit(:gen_fsm.sync_send_all_state_event(pid, :unknown_request))
    assert_receive { :EXIT, ^pid, {:bad_sync_event, :sample, :unknown_request} }
  after
    Process.flag(:trap_exit, false)
  end

  test "event stops server on unknown requests" do
    Process.flag(:trap_exit, true)
    assert { :ok, pid } = :gen_fsm.start_link(Sample, [:hello], [])

    :gen_fsm.send_all_state_event(pid, :unknown_request)
    assert_receive { :EXIT, ^pid, {:bad_event, :sample, :unknown_request} }
  after
    Process.flag(:trap_exit, false)
  end
end
