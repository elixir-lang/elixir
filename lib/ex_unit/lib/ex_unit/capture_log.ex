defmodule ExUnit.CaptureLog do
  @moduledoc ~S"""
  Functionality to capture logs for testing.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case

        import ExUnit.CaptureLog

        test "example" do
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
          assert capture_log(fun) =~ "hello"
          assert capture_log(fun) =~ "testing"
        end
      end

  """

  alias Logger.Backends.Console

  @doc """
  Captures Logger messages generated when evaluating `fun`.

  Returns the binary which is the captured output.

  This function mutes the `:console` backend
  and captures any log messages sent to Logger from the calling
  processes. It is possible to ensure explicit log messages from other
  processes are captured by waiting for their exit or monitor
  signal. This does not guarantee to capture VM generated log messages
  when a process that is spawned using a low level `Kernel` spawn function
  (e.g. `Kernel.spawn/1`) raises an exception that isn't rescued or a throw
  that isn't caught. A `Task`, or other OTP process, will send explicit logs
  that are sent before its exit or monitor signal and will not cause VM
  generated log messages.

  Note that when the `async` is set to `true`,
  the messages from another test might be captured.

  It is possible to configure the level to capture with `:level`,
  which will set the capturing level for the duration of the
  capture, for instance, if the log level is set to :error
  any message with the lower level will be ignored.
  The default level is `nil`, which will capture all messages.
  The behaviour is undetermined if async tests change Logger level.

  The format, metadata and colors can be configured with `:format`,
  `:metadata` and `:colors` respectively. These three options
  defaults to the `:console` backend configuration parameters.
  """
  @spec capture_log(Keyword.t, (() -> any)) :: String.t
  def capture_log(opts \\ [], fun) do
    opts = Keyword.put_new(opts, :level, nil)
    {:ok, string_io} = StringIO.open("")

    try do
      _ = :gen_event.which_handlers(:error_logger)
      :ok = add_capture(string_io, opts)
      ref = ExUnit.CaptureServer.log_capture_on(self())

      try do
        fun.()
      after
        :ok = Logger.flush()
        :ok = ExUnit.CaptureServer.log_capture_off(ref)
        :ok = remove_capture(string_io)
      end

      :ok
    catch
      kind, reason ->
        stack = System.stacktrace()
        _ = StringIO.close(string_io)
        :erlang.raise(kind, reason, stack)
    else
      :ok ->
        {:ok, content} = StringIO.close(string_io)
        elem(content, 1)
    end
  end

  defp add_capture(pid, opts) do
    GenEvent.add_mon_handler(Logger, {Console, pid}, {pid, opts})
  end

  defp remove_capture(pid) do
    case GenEvent.remove_handler(Logger, {Console, pid}, nil) do
      :ok ->
        receive do
          {:gen_event_EXIT, {Console, ^pid}, _reason} -> :ok
        end
      {:error, :not_found} = error ->
        mfa = {ExUnit.CaptureLog, :remove_capture, [pid]}
        receive do
          {:gen_event_EXIT, {Console, ^pid}, reason} -> exit({reason, mfa})
        after
          # In case someone accidentally flushed the message,
          # let's raise not found.
          0 -> exit({error, mfa})
        end
    end
  end
end
