Code.require_file("test_helper.exs", __DIR__)

defmodule GenStatemTest do
  alias GenStatem.GenError
  use ExUnit.Case, async: true

  defmodule HandleEventFuncPushButton do
    use GenStatem

    def init([]), do: {:ok, :off, 0}

    def callback_mode, do: :handle_event_function

    def handle_event({:call, from}, :push, :off, data) do
      GenStatem.reply([{:reply, from, :on}])
      {:next_state, :on, data + 1}
    end

    def handle_event({:call, from}, :push, :on, data) do
      GenStatem.reply(from, :off)
      {:next_state, :off, data}
    end

    def handle_event({:call, from}, :get_count, state, data) do
      {:next_state, state, data, [{:reply, from, data}]}
    end

    def handle_event(_event_type, _old_state, current_state, data) do
      {:next_state, current_state, data}
    end
  end

  defmodule StateFuncPushButton do
    use GenStatem

    def init([]), do: {:ok, :off, 0}

    def callback_mode, do: :state_functions

    def off({:call, from}, :push, data) do
      GenStatem.reply({:reply, from, :on})
      {:next_state, :on, data + 1}
    end

    def off(event_type, event_content, data) do
      handle_event(event_type, event_content, data)
    end

    def on({:call, from}, :push, data) do
      {:next_state, :off, data, [{:reply, from, :off}]}
    end

    def on(event_type, event_content, data) do
      handle_event(event_type, event_content, data)
    end

    defp handle_event({:call, from}, :get_count, data) do
      {:keep_state, data, [{:reply, from, data}]}
    end

    defp handle_event(_event_type, _event_content, data) do
      {:keep_state, data}
    end
  end

  test "handle_event_function: start_link/2,3, call/2 and cast/2" do
    module = HandleEventFuncPushButton
    {:ok, pid} = GenStatem.start_link(module, [], name: module)

    {:links, links} = Process.info(self(), :links)
    assert pid in links

    assert GenStatem.cast(pid, :push) == :ok
    assert GenStatem.call(module, :push) == :on
    assert GenStatem.call(pid, :push) == :off
    assert GenStatem.call(pid, :get_count) == 1

    assert GenStatem.stop(pid) == :ok

    assert GenStatem.cast(pid, :push) == :ok
    assert GenStatem.cast({:global, :foo}, :push) == :ok
    assert GenStatem.cast({:via, :foo, :bar}, :push) == :ok
    assert GenStatem.cast(:foo, :push) == :ok
    assert GenStatem.cast({module, node()}, :push) == :ok

    assert_raise GenError, fn -> GenStatem.call(pid, :push) end
    {:ok, pid} = GenStatem.start_link(module, [])
    assert GenStatem.stop(pid) == :ok
  end

  test "handle_event_function: start/2,3, send_request/2,4 and wait_response/2,3" do
    module = HandleEventFuncPushButton
    {:ok, pid} = GenStatem.start(module, [], name: {:global, module})

    request_id = GenStatem.send_request(pid, :push)
    assert GenStatem.wait_response(request_id) == {:reply, :on}

    req_id_col = GenStatem.reqids_new()
    label = module
    req_id_col = GenStatem.send_request(pid, :push, label, req_id_col)
    assert GenStatem.reqids_size(req_id_col) == 1

    {{:reply, response}, ^module, req_id_col} = GenStatem.wait_response(req_id_col, 5_000, true)

    assert GenStatem.reqids_size(req_id_col) == 0
    assert response == :off

    req_id_col = GenStatem.send_request(pid, :push, label, req_id_col)
    {{:reply, response}, ^module, req_id_col} = GenStatem.wait_response(req_id_col, 5_000, false)

    assert GenStatem.reqids_size(req_id_col) == 1
    assert response == :on

    assert GenStatem.stop(pid) == :ok

    assert_raise GenError, fn -> GenStatem.wait_response(request_id, :invalid_timeout) end

    assert_raise GenError, fn -> GenStatem.wait_response(req_id_col, :invalid_timeout, false) end

    assert_raise GenError, fn -> GenStatem.reqids_size([]) end

    assert_raise GenError, fn ->
      GenStatem.reqids_add(:invalid_req_id, :label, GenStatem.reqids_new())
    end

    assert_raise GenError, fn -> GenStatem.reqids_to_list([]) end
    {:ok, pid} = GenStatem.start(module, [])
    assert GenStatem.stop(pid) == :ok
  end

  test "handle_event_function: start/2,3, send_request/2,4 and receive_response/2,3" do
    module = HandleEventFuncPushButton
    {:ok, pid} = GenStatem.start(module, [])

    request_id = GenStatem.send_request(pid, :push)
    assert GenStatem.receive_response(request_id) == {:reply, :on}

    req_id_col = GenStatem.reqids_new()
    label = module
    req_id_col = GenStatem.send_request(pid, :push, label, req_id_col)
    assert GenStatem.reqids_size(req_id_col) == 1

    {{:reply, response}, ^module, req_id_col} =
      GenStatem.receive_response(req_id_col, 5_000, true)

    assert GenStatem.reqids_size(req_id_col) == 0
    assert response == :off

    req_id_col = GenStatem.send_request(pid, :push) |> GenStatem.reqids_add(label, req_id_col)

    {{:reply, response}, ^module, req_id_col} =
      GenStatem.receive_response(req_id_col, 5_000, false)

    assert GenStatem.reqids_size(req_id_col) == 1
    [{req_id, label}] = GenStatem.reqids_to_list(req_id_col)
    assert is_reference(req_id) and label == module
    assert response == :on

    assert GenStatem.stop(pid) == :ok

    assert_raise GenError, fn -> GenStatem.receive_response(request_id, :invalid_timeout) end

    assert_raise GenError, fn ->
      GenStatem.receive_response(req_id_col, :invalid_timeout, false)
    end

    assert_raise ArgumentError, fn -> GenStatem.start(module, [], name: {:invalid_name, nil}) end
  end

  test "state_functions: start_monitor/2,3, send_request/2,4 and check_response/2,3" do
    module = StateFuncPushButton
    name = {:via, :global, module}
    {:ok, {pid, _mon_ref}} = GenStatem.start_monitor(module, [], name: name)

    request_id = GenStatem.send_request(pid, :push)

    receive do
      msg -> assert GenStatem.check_response(msg, request_id) == {:reply, :on}
    end

    req_id_col = GenStatem.reqids_new()
    label = module
    req_id_col = GenStatem.send_request(pid, :push, label, req_id_col)
    assert GenStatem.reqids_size(req_id_col) == 1

    receive do
      msg ->
        {{:reply, response}, ^module, req_id_col} =
          GenStatem.check_response(msg, req_id_col, true)

        assert GenStatem.reqids_size(req_id_col) == 0
        assert response == :off
    end

    req_id_col = GenStatem.reqids_new()
    req_id_col = GenStatem.send_request(pid, :push) |> GenStatem.reqids_add(label, req_id_col)

    receive do
      msg ->
        {{:reply, response}, ^module, req_id_col} =
          GenStatem.check_response(msg, req_id_col, false)

        assert GenStatem.reqids_size(req_id_col) == 1
        assert response == :on
    end

    assert GenStatem.stop(pid) == :ok

    assert_raise GenError, fn -> GenStatem.check_response(:msg, :invalid_req_id) end

    assert_raise GenError, fn ->
      GenStatem.check_response(:msg, :invalid_req_id_col, true)
    end

    {:ok, {pid, _mon_ref}} = GenStatem.start_monitor(module, [])
    assert GenStatem.stop(pid)
  end

  defmodule CustomChildSpec do
    use GenStatem,
      id: :id,
      restart: :temporary,
      shutdown: :infinity,
      start: {HandleEventFuncPushButton, :start_link, [HandleEventFuncPushButton, []]}
  end

  test "child_spec" do
    assert HandleEventFuncPushButton.child_spec([:hello]) == %{
             id: HandleEventFuncPushButton,
             start: {HandleEventFuncPushButton, :start_link, [[:hello]]}
           }

    assert CustomChildSpec.child_spec([:hello]) == %{
             id: :id,
             restart: :temporary,
             shutdown: :infinity,
             start: {HandleEventFuncPushButton, :start_link, [HandleEventFuncPushButton, []]}
           }
  end
end
