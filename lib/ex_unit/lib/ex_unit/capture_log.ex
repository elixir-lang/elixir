defmodule ExUnit.CaptureLog do
  @moduledoc ~S"""
  Functionality to capture logs for testing.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case

        import ExUnit.CaptureLog

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
  argument. It is possible to capture all log messages by
  giving `nil` for the device.

  Note that when capturing something other than `:stdio`, log
  messages from other tests or processes might be captured. However
  it is safe to use async true if this can be handled. Multiple
  captures (and other backends) can handle log messages at the same
  time. For example the `:console` backend will still log messages
  to the console.

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
  @spec capture_log(Keyword.t, (() -> any)) :: String.t
  def capture_log(opts \\ [], fun) do
    {:ok, string_io} = StringIO.open("")
    handler = {Logger.Backends.Console, make_ref()}
    try do
      :ok = GenEvent.add_handler(Logger, handler, {string_io, opts})
      _ = fun.()
      :ok
    catch
      kind, reason ->
        stack = System.stacktrace()
        remove_backend(handler, string_io)
        :erlang.raise(kind, reason, stack)
    else
      :ok ->
        :ok = Logger.flush()
        {:ok, content} = remove_backend(handler, string_io)
        elem(content, 1)
    end
  end

  defp remove_backend(handler, string_io) do
    GenEvent.remove_handler(Logger, handler, nil)
    StringIO.close(string_io)
  end
end
