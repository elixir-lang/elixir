defmodule Logger.ErrorHandler do
  @moduledoc false
  @behaviour :gen_event

  # TODO: Remove this module when we require Erlang/OTP 21+.

  def init({otp?, sasl?, threshold}) do
    # We store the Logger PID in the state because when we are shutting
    # down the Logger application, the Logger process may be terminated
    # and then trying to reach it will lead to crashes. So we send a
    # message to a PID, instead of named process, to avoid crashes on
    # send since this handler will be removed soon by the supervisor.
    state = %{
      otp: otp?,
      sasl: sasl?,
      discard_threshold: threshold,
      keep_threshold: trunc(threshold * 0.75),
      logger: Process.whereis(Logger),
      skip: 0
    }

    {:ok, state}
  end

  ## Handle event

  def handle_event({_type, gl, _msg}, state) when node(gl) != node() do
    {:ok, state}
  end

  def handle_event(event, state) do
    state = check_threshold_unless_skipping(state)
    log_event(event, state)
    {:ok, state}
  end

  def handle_call(request, _state) do
    exit({:bad_call, request})
  end

  def handle_info(_msg, state) do
    {:ok, state}
  end

  def code_change(_old_vsn, state, _extra) do
    {:ok, state}
  end

  def terminate(_reason, _state) do
    :ok
  end

  ## Helpers

  defp log_event({:error, gl, {pid, format, data}}, %{otp: true} = state),
    do: log_event(:error, :format, gl, pid, {format, data}, state)

  defp log_event({:error_report, gl, {pid, :std_error, format}}, %{otp: true} = state),
    do: log_event(:error, :report, gl, pid, {:std_error, format}, state)

  defp log_event({:error_report, gl, {pid, :supervisor_report, data}}, %{sasl: true} = state),
    do: log_event(:error, :report, gl, pid, {:supervisor_report, data}, state)

  defp log_event({:error_report, gl, {pid, :crash_report, data}}, %{sasl: true} = state),
    do: log_event(:error, :report, gl, pid, {:crash_report, data}, state)

  defp log_event({:warning_msg, gl, {pid, format, data}}, %{otp: true} = state),
    do: log_event(:warn, :format, gl, pid, {format, data}, state)

  defp log_event({:warning_report, gl, {pid, :std_warning, format}}, %{otp: true} = state),
    do: log_event(:warn, :report, gl, pid, {:std_warning, format}, state)

  defp log_event({:info_msg, gl, {pid, format, data}}, %{otp: true} = state),
    do: log_event(:info, :format, gl, pid, {format, data}, state)

  defp log_event({:info_report, gl, {pid, :std_info, format}}, %{otp: true} = state),
    do: log_event(:info, :report, gl, pid, {:std_info, format}, state)

  defp log_event({:info_report, gl, {pid, :progress, data}}, %{sasl: true} = state),
    do: log_event(:info, :report, gl, pid, {:progress, data}, state)

  defp log_event(_, _state), do: :ok

  defp log_event(level, kind, gl, pid, {type, _} = data, state) do
    {mode, %{level: min_level, utc_log: utc_log?}} = Logger.Config.log_data()

    with true <- Logger.compare_levels(level, min_level) != :lt and mode != :discard,
         meta = [pid: ensure_pid(pid), error_logger: ensure_type(type)],
         {message, meta} <- Logger.ErlangHandler.translate(level, kind, data, meta, %{}) do
      # Mode is always async to avoid clogging the error_logger
      event = {Logger, message, Logger.Utils.timestamp(utc_log?), meta}
      :gen_event.notify(state.logger, {level, gl, event})
    end

    :ok
  end

  defp ensure_type(type) when is_atom(type), do: type
  defp ensure_type(_), do: :format

  defp ensure_pid(pid) when is_pid(pid), do: pid
  defp ensure_pid(_), do: self()

  defp check_threshold_unless_skipping(%{skip: 0} = state) do
    check_threshold(state)
  end

  defp check_threshold_unless_skipping(%{skip: skip} = state) do
    %{state | skip: skip - 1}
  end

  def check_threshold(state) do
    %{discard_threshold: discard_threshold, keep_threshold: keep_threshold} = state
    current_length = message_queue_length()

    if current_length >= discard_threshold do
      to_drop = current_length - keep_threshold
      drop_messages(to_drop)

      message =
        "Logger dropped #{to_drop} OTP/SASL messages as it had #{current_length} messages in " <>
          "its inbox, exceeding the amount of :discard_threshold #{discard_threshold} messages. " <>
          "The number of messages was reduced to #{keep_threshold} (75% of the threshold)"

      {_, %{utc_log: utc_log?}} = Logger.Config.log_data()
      event = {Logger, message, Logger.Utils.timestamp(utc_log?), pid: self()}
      :gen_event.notify(state.logger, {:warn, Process.group_leader(), event})

      # We won't check the threshold for the next 10% of the threshold messages
      %{state | skip: trunc(discard_threshold * 0.1)}
    else
      state
    end
  end

  defp message_queue_length() do
    {:message_queue_len, len} = Process.info(self(), :message_queue_len)
    len
  end

  defp drop_messages(0) do
    :ok
  end

  defp drop_messages(count) do
    receive do
      {:notify, _event} -> drop_messages(count - 1)
    after
      0 -> :ok
    end
  end
end
