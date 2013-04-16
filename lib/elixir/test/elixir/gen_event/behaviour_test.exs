Code.require_file "../../test_helper.exs", __FILE__

defmodule GenEvent.BehaviourTest do
  use ExUnit.Case, async: true

  defmodule MyEventHandler do
    use GenEvent.Behaviour

    # Callbacks

    def init(_) do
      { :ok, [] }
    end

    def handle_event({:notification, x}, notifications) do
      { :ok, [x|notifications] }
    end

    def handle_call(:notifications, notifications) do
      {:ok, Enum.reverse(notifications), []}
    end

  end

  test :using do
    { :ok, pid } = :gen_event.start_link
    :gen_event.add_handler(pid, MyEventHandler, [])

    :gen_event.notify(pid, {:notification, 1})
    :gen_event.notify(pid, {:notification, 2})
      
    assert :gen_event.call(pid, MyEventHandler, :notifications) == [1,2]
    assert :gen_event.call(pid, MyEventHandler, :notifications) == []
  end

end
