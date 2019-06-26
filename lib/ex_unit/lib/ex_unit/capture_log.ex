defmodule ExUnit.CaptureLog do
  @moduledoc ~S"""
  Functionality to capture logs for testing.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case

        import ExUnit.CaptureLog
        require Logger

        test "example" do
          assert capture_log(fn ->
                   Logger.error("log msg")
                 end) =~ "log msg"
        end

        test "check multiple captures concurrently" do
          fun = fn ->
            for msg <- ["hello", "hi"] do
              assert capture_log(fn -> Logger.error(msg) end) =~ msg
            end

            Logger.debug("testing")
          end

          assert capture_log(fun) =~ "hello"
          assert capture_log(fun) =~ "testing"
        end
      end

  """

  alias Logger.Backends.Console

  @compile {:no_warn_undefined, Logger}

  @doc """
  Captures Logger messages generated when evaluating `fun`.

  Returns the binary which is the captured output.

  This function mutes the `:console` backend and captures any log
  messages sent to Logger from the calling processes. It is possible
  to ensure explicit log messages from other processes are captured
  by waiting for their exit or monitor signal.

  However, `capture_log` does not guarantee to capture log messages
  originated from processes spawned using a low level `Kernel` spawn
  function (e.g. `Kernel.spawn/1`) and such processes exit with an
  exception or a throw. Therefore, prefer using a `Task`, or other OTP
  process, will send explicit logs before its exit or monitor signals
  and will not cause VM generated log messages.

  Note that when the `async` is set to `true`, the messages from another
  test might be captured. This is OK as long you consider such cases in
  your assertions.

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
  @spec capture_log(keyword, (() -> any)) :: String.t()
  def capture_log(opts \\ [], fun) do
    opts = Keyword.put_new(opts, :level, nil)
    {:ok, string_io} = StringIO.open("")

    try do
      _ = Process.whereis(:error_logger) && :gen_event.which_handlers(:error_logger)
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
        _ = StringIO.close(string_io)
        :erlang.raise(kind, reason, __STACKTRACE__)
    else
      :ok ->
        {:ok, content} = StringIO.close(string_io)
        elem(content, 1)
    end
  end

  defp add_capture(pid, opts) do
    case :proc_lib.start(__MODULE__, :init_proxy, [pid, opts, self()]) do
      :ok ->
        :ok

      :noproc ->
        raise "cannot capture_log/2 because the :logger application was not started"

      {:error, reason} ->
        mfa = {ExUnit.CaptureLog, :add_capture, [pid, opts]}
        exit({reason, mfa})
    end
  end

  @doc false
  def init_proxy(pid, opts, parent) do
    case :gen_event.add_sup_handler(Logger, {Console, pid}, {Console, [device: pid] ++ opts}) do
      :ok ->
        ref = Process.monitor(parent)
        :proc_lib.init_ack(:ok)

        receive do
          {:DOWN, ^ref, :process, ^parent, _reason} -> :ok
          {:gen_event_EXIT, {Console, ^pid}, _reason} -> :ok
        end

      {:EXIT, reason} ->
        :proc_lib.init_ack({:error, reason})

      {:error, reason} ->
        :proc_lib.init_ack({:error, reason})
    end
  catch
    :exit, :noproc -> :proc_lib.init_ack(:noproc)
  end

  defp remove_capture(pid) do
    case :gen_event.delete_handler(Logger, {Console, pid}, :ok) do
      :ok ->
        :ok

      {:error, :module_not_found} = error ->
        mfa = {ExUnit.CaptureLog, :remove_capture, [pid]}
        exit({error, mfa})
    end
  end
end
