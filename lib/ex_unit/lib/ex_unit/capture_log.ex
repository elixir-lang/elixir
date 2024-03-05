defmodule ExUnit.CaptureLog do
  @moduledoc ~S"""
  Functionality to capture logs for testing.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case

        import ExUnit.CaptureLog
        require Logger

        test "example" do
          {result, log} =
            with_log(fn ->
              Logger.error("log msg")
              2 + 2
            end)

          assert result == 4
          assert log =~ "log msg"
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

  @compile {:no_warn_undefined, Logger}

  @doc """
  Captures Logger messages generated when evaluating `fun`.

  Returns the binary which is the captured output. The captured log
  messages will be formatted using `Logger.default_formatter/1`. Any
  option, besides the `:level`, will be forwarded as an override to
  the default formatter.

  This function mutes the default logger handler and captures any log
  messages sent to Logger from the calling processes. It is possible
  to ensure explicit log messages from other processes are captured
  by waiting for their exit or monitor signal.

  Note that when the `async` is set to `true` on `use ExUnit.Case`,
  messages from other tests might be captured. This is OK as long
  you consider such cases in your assertions, typically by using
  the `=~/2` operator to perform partial matches.

  It is possible to configure the level to capture with `:level`,
  which will set the capturing level for the duration of the
  capture, for instance, if the log level is set to `:error`, then
  any message with the lower level will be ignored.
  The default level is `nil`, which will capture all messages.
  Note this setting does not override the overall `Logger.level/0` value.
  Therefore, if `Logger.level/0` is set to a higher level than the one
  configured in this function, no message will be captured.
  The behaviour is undetermined if async tests change Logger level.

  To get the result of the evaluation along with the captured log,
  use `with_log/2`.
  """
  @spec capture_log(keyword, (-> any)) :: String.t()
  def capture_log(opts \\ [], fun) do
    {_, log} = with_log(opts, fun)
    log
  end

  @doc """
  Invokes the given `fun` and returns the result and captured log.

  It accepts the same arguments and options as `capture_log/2`.

  ## Examples

      {result, log} =
        with_log(fn ->
          Logger.error("log msg")
          2 + 2
        end)

      assert result == 4
      assert log =~ "log msg"

  """
  @doc since: "1.13.0"
  @spec with_log(keyword, (-> result)) :: {result, String.t()} when result: any
  def with_log(opts \\ [], fun) when is_list(opts) do
    opts =
      if opts[:level] == :warn do
        IO.warn("level: :warn is deprecated, please use :warning instead")
        Keyword.put(opts, :level, :warning)
      else
        opts
      end

    {:ok, string_io} = StringIO.open("")

    try do
      ref = ExUnit.CaptureServer.log_capture_on(self(), string_io, opts)

      try do
        fun.()
      after
        :ok = Logger.flush()
        :ok = ExUnit.CaptureServer.log_capture_off(ref)
      end
    catch
      kind, reason ->
        _ = StringIO.close(string_io)
        :erlang.raise(kind, reason, __STACKTRACE__)
    else
      result ->
        {:ok, {_input, output}} = StringIO.close(string_io)
        {result, output}
    end
  end
end
