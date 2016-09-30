defmodule GenEvent.Stream do
  @moduledoc false
  defstruct manager: nil, timeout: :infinity

  @type t :: %__MODULE__{
               manager: GenEvent.manager,
               timeout: timeout}

  @doc false
  def init({_pid, _ref} = state) do
    {:ok, state}
  end

  @doc false
  def handle_event(event, _state) do
    # We do this to trick Dialyzer to not complain about non-local returns.
    case :erlang.phash2(1, 1) do
      0 -> exit({:bad_event, event})
      1 -> :remove_handler
    end
  end

  @doc false
  def handle_call(msg, _state) do
    # We do this to trick Dialyzer to not complain about non-local returns.
    reason = {:bad_call, msg}
    case :erlang.phash2(1, 1) do
      0 -> exit(reason)
      1 -> {:remove_handler, reason}
    end
  end

  @doc false
  def handle_info(_msg, state) do
    {:ok, state}
  end

  @doc false
  def terminate(_reason, _state) do
    :ok
  end

  @doc false
  def code_change(_old, state, _extra) do
    {:ok, state}
  end
end

defimpl Enumerable, for: GenEvent.Stream do
  def reduce(stream, acc, fun) do
    start_fun = fn() -> start(stream) end
    next_fun = &next(stream, &1)
    stop_fun = &stop(stream, &1)
    Stream.resource(start_fun, next_fun, stop_fun).(acc, wrap_reducer(fun))
  end

  def count(_stream) do
    {:error, __MODULE__}
  end

  def member?(_stream, _item) do
    {:error, __MODULE__}
  end

  defp wrap_reducer(fun) do
    fn
      {:ack, manager, ref, event}, acc ->
        send manager, {ref, :ok}
        fun.(event, acc)
      {:async, _manager, _ref, event}, acc ->
        fun.(event, acc)
      {:sync, manager, ref, event}, acc ->
        try do
          fun.(event, acc)
        after
          send manager, {ref, :ok}
        end
    end
  end

  defp start(%{manager: manager} = stream) do
    try do
      {:ok, {pid, ref}} = :gen.call(manager, self(),
                                    {:add_process_handler, self(), self()}, :infinity)
      mon_ref = Process.monitor(pid)
      {pid, ref, mon_ref}
    catch
      :exit, reason -> exit({reason, {__MODULE__, :start, [stream]}})
    end
  end

  defp next(%{timeout: timeout} = stream, {pid, ref, mon_ref} = acc) do
    self = self()

    receive do
      # Got an async event.
      {_from, {^pid, ^ref}, {:notify, event}} ->
        {[{:async, pid, ref, event}], acc}

      # Got a sync event.
      {_from, {^pid, ^ref}, {:sync_notify, event}} ->
        {[{:sync, pid, ref, event}], acc}

      # Got an ack event.
      {_from, {^pid, ^ref}, {:ack_notify, event}} ->
        {[{:ack, pid, ref, event}], acc}

      # The handler was removed. Stop iteration, resolve the
      # event later. We need to demonitor now, otherwise DOWN
      # appears with higher priority in the shutdown process.
      {:gen_event_EXIT, {^pid, ^ref}, _reason} = event ->
        Process.demonitor(mon_ref, [:flush])
        send(self, event)
        {:halt, {:removed, acc}}

      # The manager died. Stop iteration, resolve the event later.
      {:DOWN, ^mon_ref, _, _, _} = event ->
        send(self, event)
        {:halt, {:removed, acc}}
    after
      timeout ->
        exit({:timeout, {__MODULE__, :next, [stream, acc]}})
    end
  end

  # If we reach this branch, we know the handler was already
  # removed, so we don't trigger a request for doing so.
  defp stop(stream, {:removed, {pid, ref, mon_ref} = acc}) do
    case wait_for_handler_removal(pid, ref, mon_ref) do
      :ok ->
        flush_events(ref)
      {:error, reason} ->
        exit({reason, {__MODULE__, :stop, [stream, acc]}})
    end
  end

  # If we reach this branch, the handler was not removed yet,
  # so we trigger a request for doing so.
  defp stop(stream, {pid, ref, _} = acc) do
    _ = GenEvent.remove_handler(pid, {pid, ref}, :shutdown)
    stop(stream, {:removed, acc})
  end

  defp wait_for_handler_removal(pid, ref, mon_ref) do
    receive do
      {:gen_event_EXIT, {^pid, ^ref}, _reason} ->
        Process.demonitor(mon_ref, [:flush])
        :ok
      {:DOWN, ^mon_ref, _, _, reason} ->
        {:error, reason}
    end
  end

  defp flush_events(ref) do
    receive do
      {_from, {_pid, ^ref}, {notify, _event}} when notify in [:notify, :ack_notify, :sync_notify] ->
        flush_events(ref)
    after
      0 -> :ok
    end
  end
end
