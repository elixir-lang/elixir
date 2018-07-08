Logger.configure_backend(:console, colors: [enabled: false])

exclude = if Process.whereis(:logger), do: [:error_logger], else: [:logger]
ExUnit.start(exclude: exclude)

defmodule Logger.Case do
  use ExUnit.CaseTemplate
  import ExUnit.CaptureIO

  using _ do
    quote do
      import Logger.Case
    end
  end

  def msg(msg) do
    ~r/\d\d\:\d\d\:\d\d\.\d\d\d #{Regex.escape(msg)}/
  end

  def wait_for_handler(manager, handler) do
    unless handler in :gen_event.which_handlers(manager) do
      Process.sleep(10)
      wait_for_handler(manager, handler)
    end
  end

  def wait_for_logger() do
    try do
      :gen_event.which_handlers(Logger)
    catch
      :exit, _ ->
        Process.sleep(10)
        wait_for_logger()
    else
      _ ->
        :ok
    end
  end

  def add_test_logger_backend(inspect \\ false) do
    capture_log(:info, fn ->
      Logger.add_backend(TestLoggerBackend)
      Logger.configure_backend(TestLoggerBackend, callback_pid: self(), inspect: inspect)
    end)

    ExUnit.Callbacks.on_exit(fn ->
      :ok = Logger.remove_backend(TestLoggerBackend)
    end)
  end

  def capture_log(level \\ :debug, fun) do
    Logger.configure(level: level)

    capture_io(:user, fn ->
      fun.()
      Logger.flush()
    end)
  after
    Logger.configure(level: :debug)
  end
end

defmodule TestLoggerBackend do
  @moduledoc """
  A module that implements a custom `Logger` backend for use in testing.

  This module can be used to verify events sent to `Logger`.
  """

  @behaviour :gen_event

  def init(_) do
    {:ok, %{events: []}}
  end

  def handle_call(:get, %{events: events} = state) do
    {:ok, events, state}
  end

  def handle_call({:configure, opts}, state) do
    callback_pid = Keyword.get(opts, :callback_pid)
    inspect = Keyword.get(opts, :inspect)

    state =
      state
      |> Map.put(:callback_pid, callback_pid)
      |> Map.put(:inspect, inspect)

    {:ok, :ok, state}
  end

  def handle_event(
        {_level, _gl, {Logger, _msg, _ts, _md}} = event,
        %{events: events, callback_pid: pid, inspect: inspect} = state
      ) do
    inspect && IO.inspect(event)
    send(pid, event)
    {:ok, %{state | events: [event | events]}}
  end

  def handle_event(:flush, state) do
    {:ok, %{state | events: []}}
  end

  def handle_event(_, state) do
    {:ok, state}
  end

  def handle_info(_, state) do
    {:ok, state}
  end

  def code_change(_old, state, _extra) do
    {:ok, state}
  end

  def terminate(_reason, _state) do
    :ok
  end
end
