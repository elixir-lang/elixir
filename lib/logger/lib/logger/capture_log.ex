defmodule Logger.CaptureLog do
  @moduledoc ~S"""
  Functionality to capture logs for testing.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case

        import Logger.CaptureLog

        test :example do
          assert capture_log(fn ->
            Logger.error "log msg"
          end) =~ "log msg"
        end

        test "check multiple captures concurrently" do
          fun = fn ->
            for msg <- ["hello", "hi"] do
              assert capture_log(fn -> Logger.error msg end) =~ msg
            end
            Logger.debug "testing"
          end
          assert capture_log(fun) =~ "testing"
        end
      end

  """

  @doc """
  Captures Logger messages generated when evaluating `fun`.

  Returns the binary which is the captured output.

  By default, `capture_log` replaces the `group_leader` (`:stdio`)
  for the current process and captures any log messages sent by
  processes wth the replacement `group leader`. However, the
  capturing of log messages with a `group_leader` of any other
  device is also possible by giving the device explicitly as an
  argument. It is also possible to capture all log messages by
  giving `nil` for the device.

  Note that when capturing something other than `:stdio`, log
  messages from other tests or processes might be captured. However
  it is safe to use async true if this can be handled. Multiple
  captures (and other backends) can handle log messages at the same
  time. For example the `:console` backend will still log messages
  to the console.

  It is possible to capture all log messages by giving `nil` for
  the device.

  It is possible to configure the level to capture with `:level`,
  which will change the current Logger level for the duration of the
  capture. If the log level is changed while capturing, for example
  by another `capture_log/3`, the behaviour is undetermined.
  Therefore tests should be async false when setting a capturing
  level. The default level is `nil`, which will capture all messages
  at the current Logger level. It is safe to use with async true when
  the level is `nil` because it does not change the current Logger
  level.

  The format, metadata and colors can be configured with `:format`,
  `:metadata` and `:colors` respectively. Defaults for these three
  options can be configured using the `:logger` application config
  with key `:capture`. For example, in a `config/config.exs` file:

      config :logger, :capture,
        format: "\n$date $time [$level] $metadata$message",
        metadata: [:user_id],
        colors: [enabled: false]
  """
  @spec capture_log((() -> any)) :: String.t
  def capture_log(fun) do
    capture_log(:standard_io, [], fun)
  end

  @spec capture_log(IO.device() | nil | Keyword.t, (() -> any)) :: String.t
  def capture_log(device, fun) when is_atom(device) or is_pid(device) do
    capture_log(device, [], fun)
  end
  def capture_log(options, fun) when is_list(options) do
    capture_log(:standard_io, options, fun)
  end

  @spec capture_log(IO.device() | nil, Keyword.t, (() -> any)) :: String.t
  def capture_log(device, options, fun) do
    do_capture_log(map_dev(device), options, fun)
  end


  defp map_dev(:standard_io), do: :standard_io
  defp map_dev(:stdio),       do: :standard_io
  defp map_dev(nil),          do: nil
  defp map_dev(device) when is_pid(device), do: device
  defp map_dev(device) when is_atom(device) do
    case Process.whereis(device) do
      nil ->
        raise "could not find IO device registered at #{inspect device}"
      pid -> pid
    end
  end

  defp do_capture_log(:standard_io, opts, fun) do
    old_gl = Process.group_leader()
    {:ok, gl} = ProxyIO.open()
    try do
      Process.group_leader(self(), gl)
      do_capture_log(gl, opts, fun)
    after
      Process.group_leader(self(), old_gl)
      ProxyIO.close(gl)
    end
  end
  defp do_capture_log(device, opts, fun) do
    handler = {Logger.Backends.Capture, make_ref()}
    try do
      :ok = GenEvent.add_handler(Logger, handler, {device, opts})
      _ = fun.()
      :ok
    catch
      kind, reason ->
        stack = System.stacktrace()
        _ = GenEvent.remove_handler(Logger, handler, nil)
        :erlang.raise(kind, reason, stack)
    else
      :ok ->
        {:ok, output} = GenEvent.remove_handler(Logger, handler, :get)
        IO.chardata_to_string(output)
    end
  end
end
