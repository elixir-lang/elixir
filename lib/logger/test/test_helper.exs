Logger.configure_backend(:console, colors: [enabled: false])

{line_exclude, line_include} =
  if line = System.get_env("LINE"), do: {[:test], [line: line]}, else: {[], []}

ExUnit.start(
  trace: !!System.get_env("TRACE"),
  include: line_include,
  exclude: line_exclude
)

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

defmodule Logger.TestBackend do
  @moduledoc """
  A module that implements a custom `Logger` backend for use in testing.

  This module can be used to verify events sent to `Logger`.
  """

  @behaviour :gen_event

  def init(_) do
    {:ok, %{}}
  end

  def handle_call({:configure, opts}, state) do
    callback_pid = Keyword.get(opts, :callback_pid)
    {:ok, :ok, Map.put(state, :callback_pid, callback_pid)}
  end

  def handle_event(
        {_level, _gl, {Logger, _msg, _ts, _md}} = event,
        %{callback_pid: pid} = state
      ) do
    send(pid, event)
    {:ok, state}
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
