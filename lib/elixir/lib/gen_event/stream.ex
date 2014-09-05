defmodule GenEvent.Stream do
  @moduledoc """
  Defines a `GenEvent` stream.

  This is a struct returned by `stream/2`. The struct is public and
  contains the following fields:

    * `:manager`  - the manager reference given to `GenEvent.stream/2`
    * `:timeout`  - the timeout in between events, defaults to `:infinity`

  """
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
    exit({:bad_event, event})
  end

  @doc false
  def handle_call(msg, _state) do
    exit({:bad_call, msg})
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
      {:ok, {pid, ref}} = :gen.call(manager, self(), {:add_process_handler, self(), true}, :infinity)
      mon_ref = Process.monitor(pid)
      {pid, ref, mon_ref}
    catch
      :exit, reason -> exit({reason, {__MODULE__, :start, [stream]}})
    end
  end

  defp next(%{timeout: timeout} = stream, {pid, ref, mon_ref} = acc) do
    self = self()

    receive do
      # The handler was removed. Stop iteration, resolve the
      # event later. We need to demonitor now, otherwise DOWN
      # appears with higher priority in the shutdown process.
      {:gen_event_EXIT, {GenEvent.Stream, ^self}, _reason} = event ->
        Process.demonitor(mon_ref, [:flush])
        send(self, event)
        {:halt, {:removed, acc}}

      # The manager died. Stop iteration, resolve the event later.
      {:DOWN, ^mon_ref, _, _, _} = event ->
        send(self, event)
        {:halt, {:removed, acc}}

      # Got an async event.
      {_from, {^pid, ^ref}, {:notify, event}} ->
        {[{:async, pid, ref, event}], acc}

      # Got a sync event.
      {_from, {^pid, ^ref}, {:sync_notify, event}} ->
        {[{:sync, pid, ref, event}], acc}

      # Got an ack event.
      {_from, {^pid, ^ref}, {:ack_notify, event}} ->
        {[{:ack, pid, ref, event}], acc}
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
  defp stop(stream, {pid, _, _} = acc) do
    parent = self()
    _ = Task.start(fn -> GenEvent.remove_handler(pid, {GenEvent.Stream, parent}, :shutdown) end)
    stop(stream, {:removed, acc})
  end

  defp wait_for_handler_removal(pid, ref, mon_ref) do
    self = self()

    receive do
      {_from, {^pid, ^ref}, {notify, _event}} when notify in [:ack_notify, :sync_notify] ->
        send pid, {ref, :done}
        wait_for_handler_removal(pid, ref, mon_ref)
      {:gen_event_EXIT, {GenEvent.Stream, ^self}, reason}
          when reason == :normal
          when reason == :shutdown
          when tuple_size(reason) == 3 and elem(reason, 0) == :swapped  ->
        Process.demonitor(mon_ref, [:flush])
        :ok
      {:gen_event_EXIT, {GenEvent.Stream, ^self}, reason} ->
        Process.demonitor(mon_ref, [:flush])
        {:error, reason}
      {:DOWN, ^mon_ref, _, _, reason} ->
        {:error, reason}
    end
  end

  defp flush_events(ref) do
    receive do
      {_from, {_pid, ^ref}, {:notify, _event}} ->
        flush_events(ref)
    after
      0 -> :ok
    end
  end
end
